#ifndef SHADOW_H
#define SHADOW_H

#include "common\common.hlsli"
#include "common\pcf_filter.hlsli"

Texture2DArray<float> s_smap_sun : register(t50); //Sun: three cascades, then the rain map
Texture2DArray<float> s_smap : register(t51); //Point-spot: the atlas pages



SamplerComparisonState smp_smap : register(s5);

SamplerState smp_jitter : register(s6);
Texture2D jitter0 : register(t52);
Texture2D jitter1 : register(t53);

//Local light shadows: Ldynamic_smap is the light's page of the atlas
float shadow_local(float3 texcoord)
{
	float bias = 1e-5;
	float2 smap_dims = float2(SMAP_size, 1.0 / SMAP_size);

	return pcf_3x3(s_smap, smp_smap, texcoord, smap_dims, bias, int(Ldynamic_smap));
}

#include "common\sun_shadow.hlsli"

#endif
