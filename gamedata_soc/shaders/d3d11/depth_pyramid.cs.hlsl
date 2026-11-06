#include "common.hlsli"

// Depth pyramids shared by SSLR and GTAO, one 16x16 tile per group.
// u_hiz: level 0 of the SSLR min-Z pyramid (Stachowiak, SIGGRAPH 2015). World rays must pass behind the HUD: it gets the depth of the
// reflection cube, which has no HUD, so rays occluded behind the weapon still cross a surface and fall back to the cube march.
// u_ao_depth0..4: GTAO view-z with prefiltered MIPs (XeGTAO). HUD view-z is stored negative, GTAO taps pick the HUD projection by the sign.
RWTexture2D<float> u_hiz : register(u0);
RWTexture2D<float> u_ao_depth0 : register(u1);
RWTexture2D<float> u_ao_depth1 : register(u2);
RWTexture2D<float> u_ao_depth2 : register(u3);
RWTexture2D<float> u_ao_depth3 : register(u4);
RWTexture2D<float> u_ao_depth4 : register(u5);

groupshared float Tile[8][8];

float HiZDepth(uint2 Pixel, float Depth)
{
	[branch]
	if (Depth >= 0.02f)
	{
		return Depth;
	}

#ifdef USE_OFFSCREEN_REFLECTIONS
	float2 Ndc = (Pixel + 0.5f) * pos_decompression_params2.zw * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f);
	float3 Dir = normalize(float3(Ndc * m_invP._11_22, 1.0f));
	float ViewZ = Dir.z * s_env_dist.SampleLevel(smp_linear, Dir, 0.0f).x;

	// Inverse of GbufferGetViewZ
	return saturate((rcp(ViewZ) - m_invP._44) / m_invP._43);
#else
	return 1.0f;
#endif
}

// XeGTAO DepthMIPFilter for a 0.85 m effect radius: surfaces more than ~0.64 m in front of the farthest one are dropped
float FilterDepth(float4 Z)
{
	float4 Distance = abs(Z);
	float Far = max(max(Distance.x, Distance.y), max(Distance.z, Distance.w));
	float4 Weight = saturate((Far - Distance) * -2.55f + 1.63f);
	float Sign = max(max(Z.x, Z.y), max(Z.z, Z.w)) < 0.0f ? -1.0f : 1.0f;
	return Sign * dot(Weight, Distance) / dot(Weight, 1.0f);
}

[numthreads(8, 8, 1)]
void main(uint2 Gid : SV_GroupID, uint2 Tid : SV_GroupThreadID)
{
	uint2 Last = uint2(pos_decompression_params2.xy) - 1;
	uint2 Pixel = Gid * 16 + Tid * 2;

	float4 Depth = float4(
		s_position.Load(int3(min(Pixel, Last), 0)).x,
		s_position.Load(int3(min(Pixel + uint2(1, 0), Last), 0)).x,
		s_position.Load(int3(min(Pixel + uint2(0, 1), Last), 0)).x,
		s_position.Load(int3(min(Pixel + 1, Last), 0)).x);

	u_hiz[Pixel] = HiZDepth(Pixel, Depth.x);
	u_hiz[Pixel + uint2(1, 0)] = HiZDepth(Pixel + uint2(1, 0), Depth.y);
	u_hiz[Pixel + uint2(0, 1)] = HiZDepth(Pixel + uint2(0, 1), Depth.z);
	u_hiz[Pixel + 1] = HiZDepth(Pixel + 1, Depth.w);

	float4 Z = float4(GbufferGetViewZ(Depth.x), GbufferGetViewZ(Depth.y), GbufferGetViewZ(Depth.z), GbufferGetViewZ(Depth.w));
	Z = Depth < 0.02f ? -Z : Z;
	u_ao_depth0[Pixel] = Z.x;
	u_ao_depth0[Pixel + uint2(1, 0)] = Z.y;
	u_ao_depth0[Pixel + uint2(0, 1)] = Z.z;
	u_ao_depth0[Pixel + 1] = Z.w;

	float Level1 = FilterDepth(Z);
	u_ao_depth1[Gid * 8 + Tid] = Level1;
	Tile[Tid.y][Tid.x] = Level1;

	// Every level reads the 2x2 written by the previous one and overwrites only its own top-left texel
	[unroll]
	for (uint Level = 1; Level < 4; ++Level)
	{
		GroupMemoryBarrierWithGroupSync();

		uint Step = 1u << (Level - 1);

		[branch]
		if (all(Tid % (Step * 2) == 0))
		{
			float Filtered = FilterDepth(float4(Tile[Tid.y][Tid.x], Tile[Tid.y][Tid.x + Step], Tile[Tid.y + Step][Tid.x], Tile[Tid.y + Step][Tid.x + Step]));
			Tile[Tid.y][Tid.x] = Filtered;

			uint2 Texel = (Gid * 8 + Tid) >> Level;

			if (Level == 1)
			{
				u_ao_depth2[Texel] = Filtered;
			}
			else if (Level == 2)
			{
				u_ao_depth3[Texel] = Filtered;
			}
			else
			{
				u_ao_depth4[Texel] = Filtered;
			}
		}
	}
}
