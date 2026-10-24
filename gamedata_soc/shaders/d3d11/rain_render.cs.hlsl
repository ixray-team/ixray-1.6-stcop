#include "common.hlsli"
#include "shadow.hlsli"

Texture3D s_water;
Texture2D s_waterFall;

float4 RainDensity;
float4 RainFallof;
float4 WorldX;
float4 WorldZ;
float4 m_level_scale;

float3 GetNVNMap(Texture3D s_texture, float2 tc, float time)
{
    float4 water = s_texture.SampleLevel(smp_linear, float3(tc, time), 0) - 0.5;
    water.xyz = water.wyz;

    water.xyz *= 6;
    water.y = 0;

    return water.xyz;
}

float3 GetWaterNMap(Texture2D s_texture, float2 tc)
{
    float4 water = s_texture.SampleLevel(smp_linear, tc, 0);
    water.xyz = (water.xzy - 0.5) * 2;

    water.xyz *= 0.3;
    water.y = 0;

    return water.xyz;
}

RWTexture2D<unorm float4> u_color : register(u0);
RWTexture2D<unorm float4> u_normal : register(u1);
RWTexture2D<unorm float4> u_surface : register(u2);

#define mirror(x) saturate(1.0 - abs(abs(x) - 1.0))

float3 GbufferGetPointReal(float2 TexCoord)
{
	float Depth = s_position.SampleLevel(smp_rtlinear, TexCoord, 0).x;
	return GbufferGetPointRealJitter(TexCoord, Depth);
}

float3 GetFlatNormal(float2 tc) 
{
	float3 center_point = GbufferGetPointReal(tc);
	
	float3 crosspoint0 = GbufferGetPointReal(tc + float2(pos_decompression_params2.z, 0.0f));
	float3 crosspoint1 = GbufferGetPointReal(tc + float2(0.0f, pos_decompression_params2.w));
	
	float3 crosspoint2 = GbufferGetPointReal(tc - float2(pos_decompression_params2.z, 0.0f));
	float3 crosspoint3 = GbufferGetPointReal(tc - float2(0.0f, pos_decompression_params2.w));
	
	float3 ddx_1 = crosspoint0 - center_point;
	float3 ddy_1 = crosspoint1 - center_point;
	
	float3 ddx_2 = center_point - crosspoint2;
	float3 ddy_2 = center_point - crosspoint3;
	
	if
	(
		tc.x <= pos_decompression_params2.z ||
		tc.x >= (1.0 - pos_decompression_params2.z) ||
		tc.y <= pos_decompression_params2.w ||
		tc.y >= (1.0 - pos_decompression_params2.w)
	) 
	{
		ddx_2 = ddx_1 = tc.x > 0.5 ? ddx_2 : ddx_1;
		ddy_2 = ddy_1 = tc.y > 0.5 ? ddy_2 : ddy_1;
	}
	else
	{
		ddx_1 = abs(ddx_1.z) > abs(ddx_2.z) ? ddx_2 : ddx_1;
		ddy_1 = abs(ddy_1.z) > abs(ddy_2.z) ? ddy_2 : ddy_1;
	}
	
	return normalize(cross(ddx_1, ddy_1));
}

// float4 main(float2 tc : TEXCOORD0, float2 tcJ : TEXCOORD1, float4 Color : COLOR, float4 pos2d : SV_POSITION) : SV_Target
[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
    //IXRayGbuffer O = (IXRayGbuffer)NULL;
    //GbufferUnpack((uint2)pos2d.xy, O);
	
	IXRayMaterial M = (IXRayMaterial)NULL;
	M.Depth = s_position[DTid];
	
	if(M.Depth > 0.9999f)
	{
		return;
	}
	
	float2 TexCoord = (DTid.xy + 0.5f) * pos_decompression_params2.zw;
	M.Point = GbufferGetPointRealJitter(TexCoord, M.Depth);
	
	return;
}

