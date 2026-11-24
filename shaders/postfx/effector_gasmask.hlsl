/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
// By: Lancevrot
// Optimize: Hozar2002
// Dead: ForserX

#include "common\common.hlsli"

#define VIS_GLASS_NUM 9			 
#define VIS_GLASS_RADIUS 0.32f
#define VIS_GLASS_INTENSITY 0.45f

Texture2D s_gasmask : register(t26);
Texture2D s_breath : register(t25);

float3 calc_visor_reflect(float3 color, float2 tc)
{
	float3 res = 0;
	float2 center = float2(0.5f, 0.5f);

	float ratio = screen_res.y / screen_res.x;
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
		res += s_image.SampleLevel(smp_rtlinear, ViewToTarget((center - tc) * -N + center), 0).xyz;
	}

	res *= rcp(VIS_GLASS_NUM);
	res *= 1.0f - saturate((x - VIS_GLASS_RADIUS - 0.05f) * 5.2f);
	return (color + VIS_GLASS_INTENSITY * res) * rcp(1.0f + VIS_GLASS_INTENSITY);
}

float4 ps_main(PSInputFullscreen I) : SV_Target
{
	// The mask covers the view, whatever part of the target holds it
	float2 view = I.texcoord / view_uv.xy;
	float4 mask = s_gasmask.SampleLevel(smp_rtlinear, view, 0.0);
	float4 breath = s_breath.SampleLevel(smp_rtlinear, view, 0.0);

	float2 tc0_shift = min(I.texcoord + (mask.xy - 0.5) * 0.035 * view_uv.xy, view_uv.zw); //LV: 0.035 looks better

	float4 img = s_image.SampleLevel(smp_rtlinear, tc0_shift, 0.0);
	img.xyz = calc_visor_reflect(img.xyz, tc0_shift / view_uv.xy);
	img.xyz += (mask.w * 0.2) + (breath.w * abs(timers.w));

	return img; //LV: in case we store something in alpha channel
}
