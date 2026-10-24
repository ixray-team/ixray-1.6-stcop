#ifndef SHADOW_H
#define SHADOW_H

#include "common.hlsli"
#include "pcf_filter.hlsli"

Texture2DArray<float> s_smap_sun; //Sun, three cascades
Texture2D<float> s_smap; //Point-spot, rain



SamplerComparisonState smp_smap;

SamplerState smp_jitter;
Texture2D jitter0;
Texture2D jitter1;

bool IsSunCaptureCoordValid(float3 Coord)
{
	const float Margin = 4.0f / SMAP_size;
	// vkd3d-shader, which provides D3DCompile on macOS and Linux, has no
	// isfinite intrinsic. NaN and Inf both fail this comparison, so it rejects
	// the same values as isfinite did.
	return all(abs(Coord) <= 3.402823466e+38F) && all(Coord.xy > Margin) && all(Coord.xy < 1.0f - Margin) && Coord.z > 0.0f && Coord.z < 1.0f;
}

// Correct the receiver for a changed sun direction, using the original capture.
// This bounded estimate cannot reconstruct moving or previously hidden blockers.
bool CorrectSunShadowCoord(float3 Original, float4x4 CaptureTransform, int Index, out float3 Corrected)
{
	Corrected = Original;
	if (SunCaptureDirectionDepth[Index].w <= 0.0f || !IsSunCaptureCoordValid(Original))
	{
		return false;
	}
	float3 Warp = SunReceiverWarpBias[Index].xyz;
	[branch]
	if (SunCacheParams.x < 2.0f || !any(Warp != 0.0f))
	{
		return true;
	}

	float3 TextureWarp = mul(CaptureTransform, float4(Warp, 0.0f)).xyz;
	float ReceiverDepth = Original.z - SunReceiverWarpBias[Index].w;
	[unroll]
	for (int Iteration = 0; Iteration < 2; ++Iteration)
	{
		int2 Base = int2(floor(Corrected.xy * SMAP_size - 0.5f));
		float BlockerSum = 0.0f;
		float BlockerCount = 0.0f;
		[unroll]
		for (int Y = 0; Y < 2; ++Y)
		{
			[unroll]
			for (int X = 0; X < 2; ++X)
			{
				float Depth = s_smap_sun.Load(int4(Base + int2(X, Y), Index, 0));
				if (Depth < 1.0f && Depth < ReceiverDepth - 1e-5f)
				{
					BlockerSum += Depth;
					BlockerCount += 1.0f;
				}
			}
		}
		float Distance = BlockerCount > 0.0f ? max(0.0f, ReceiverDepth - BlockerSum / max(BlockerCount, 1.0f)) * SunCaptureDirectionDepth[Index].w : 0.0f;
		Corrected = Original + Distance * TextureWarp;
		if (!IsSunCaptureCoordValid(Corrected))
		{
			return false;
		}
	}
	return true;
}

float SampleSunShaft(float3 Coord)
{
	[branch]
	if (SunCacheParams.x > 0.0f)
	{
		float3 Corrected;
		if (!CorrectSunShadowCoord(Coord, m_shadow_sun[2], 2, Corrected))
		{
			return 1.0f;
		}
		Coord = Corrected;
	}
	return s_smap_sun.SampleCmpLevelZero(smp_smap, float3(Coord.xy, 2), Coord.z);
}

// Calculates cascade indices. Index 3 means no usable capture.
bool calc_cascades(in float3 position, in float4x4 smap_vp_matrix[3], inout int cascade_index, inout float3 smap_texcoord)
{
	bool is_in_bounds = false;
	smap_texcoord = 0.0f;

	for(cascade_index = 0; cascade_index < 3; cascade_index++)
	{
		//Transform position into UV space
		float4 temp = mul(smap_vp_matrix[cascade_index], float4(position, 1.0));
		smap_texcoord = temp.xyz / temp.w;
		[branch]
		if (SunCacheParams.x > 0.0f)
		{
			float3 Corrected;
			if (CorrectSunShadowCoord(smap_texcoord, smap_vp_matrix[cascade_index], cascade_index, Corrected))
			{
				smap_texcoord = Corrected;
				return true;
			}
			continue;
		}

		//Check the bounds
		if(all(abs(smap_texcoord.xyz - 0.5) < 0.5))
		{
			is_in_bounds = true;
			break;
		}
	}
	if (SunCacheParams.x > 0.0f)
	{
		smap_texcoord = 0.0f;
	}

	return is_in_bounds;
}

//Rainmap (LVutner: Well, you can make wider kernel...)
float shadow_rain(float3 texcoord)
{
	float bias = 1e-5;
	float2 smap_dims = float2(SMAP_size, 1.0 / SMAP_size);

	return pcf_3x3(s_smap, smp_smap, texcoord, smap_dims, bias);
}

//Local light shadows
float shadow_local(float3 texcoord)
{
	float bias = 1e-5;
	float2 smap_dims = float2(SMAP_size, 1.0 / SMAP_size);

	return pcf_3x3(s_smap, smp_smap, texcoord, smap_dims, bias);
}

//Sun shadows
float shadow_sun(float3 texcoord, int cascade_index)
{
	float bias = 1e-5;
	float2 smap_dims = float2(SMAP_size, 1.0 / SMAP_size);

#if SUN_QUALITY == 0 //Low
	return pcf_3x3(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#elif SUN_QUALITY == 1 //Medium
	return pcf_5x5(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#else //High
	return pcf_7x7(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#endif
}

float sunmask(float4 P)
{
    return 0.25f + 0.75f * s_lmap.SampleLevel(smp_linear, mul(m_sunmask, P).xy, 0.0).w;
}


#endif
