// By: Lancevrot
// Optimize: Hozar2002
// Dead: ForserX

#include "common.hlsli"

#define VIS_GLASS_NUM 9			 
#define VIS_GLASS_RADIUS 0.32f
#define VIS_GLASS_INTENSITY 0.45f

float4 screen_res;
Texture2D s_gasmask;
Texture2D s_breath;

float3 calc_visor_reflect(float3 color, float2 tc)
{
	float3 res = 0;
	float2 center = float2(0.5f, 0.5f);

	float ratio = screen_res.y * screen_res.z;
	float x = length(float2(tc.x, (tc.y - 0.5f) * ratio + 0.5f) - center);

	if (x < VIS_GLASS_RADIUS)
	{
    	return color;
    }
        
	float p = saturate((x / VIS_GLASS_RADIUS - 1.0f) * 2.0f);
	float N = 0.0f;
	
    [unroll]
	for (uint i = 0; i < VIS_GLASS_NUM; ++i)
	{
		N = 1.0f - p / 0.8f + 0.15f * (i / (VIS_GLASS_NUM - 1.0f)) * p;
		res += s_image.SampleLevel(smp_rtlinear, ((center - tc) * -N + center), 0);
	}

	res *= rcp(VIS_GLASS_NUM);
	res *= 1.0f - saturate((x - VIS_GLASS_RADIUS - 0.05f) * 5.2f);
	return (color + VIS_GLASS_INTENSITY * res) * rcp(1.0f + VIS_GLASS_INTENSITY);
}

float4 main(float2 tc0 : TEXCOORD0) : SV_Target
{
	float4 mask = s_gasmask.SampleLevel(smp_rtlinear, tc0, 0.0);
	float4 breath = s_breath.SampleLevel(smp_rtlinear, tc0, 0.0);

	float2 tc0_shift = tc0 + (mask.xy - 0.5) * 0.035; //LV: 0.035 looks better

	float4 img = s_image.SampleLevel(smp_rtlinear, tc0_shift, 0.0);
	img.xyz = calc_visor_reflect(img.xyz, tc0_shift);
	img.xyz += (mask.w * 0.2) + (breath.w * abs(timers.w));

	return img; //LV: in case we store something in alpha channel
}