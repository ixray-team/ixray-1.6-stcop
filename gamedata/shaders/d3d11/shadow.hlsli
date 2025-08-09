#ifndef SHADOW_H
#define SHADOW_H

#include "common.hlsli"
#include "pcf_filter.hlsli"


Texture2DArray<float> s_smap_sun; //Sun, three cascades
Texture2D<float> s_smap; //Point-spot, rain

//LVutner: This should go into the same cbuffer as sunmask
float4x4 m_shadow_sun[3];
float4x4 m_shadow;

SamplerComparisonState smp_smap;

SamplerState smp_jitter;
Texture2D jitter0;
Texture2D jitter1;

//Calculates cascade indices
bool calc_cascades(in float3 position, in float4x4 smap_vp_matrix[3], inout int cascade_index, inout float3 smap_texcoord)
{
	bool is_in_bounds = false;

	for(cascade_index = 0; cascade_index < 3; cascade_index++)
	{
		//Transform position into UV space
		float4 temp = mul(smap_vp_matrix[cascade_index], float4(position, 1.0));
		smap_texcoord = temp.xyz / temp.w;

		//Check the bounds
		if(all(abs(smap_texcoord.xyz - 0.5) < 0.5))
		{
			is_in_bounds = true;
			break;
		}
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
#elif SUN_QUALITY == 2 //High
	return pcf_7x7(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#elif SUN_QUALITY == 3 //Ultra
	return 1.0;
#elif SUN_QUALITY == 4 //Extreme
	return 1.0;
#endif
}

//Cloud shadows
#ifdef USE_SUNMASK
float3x4 m_sunmask;

float sunmask(float4 P)
{
    float2 tc = mul(m_sunmask, P).xy;
    return PushGamma(lerp(0.25, 1.0, s_lmap.SampleLevel(smp_linear, tc, 0.0).w)); //LVutner: Always should use REPEAT
}
#else
float sunmask(float4 P)
{
    return 1.0;
}
#endif
#endif

