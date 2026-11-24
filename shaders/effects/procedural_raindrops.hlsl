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
#include "common\common.hlsli"
#include "common\VisorDrops.hlsli"

#include "common\extra_cb.hlsli"

float4 ps_main(PSInputFullscreen I) : SV_Target
{
	float RainAmount = saturate(visor_rain.x);
	float2 Aspect = float2(screen_res.x / screen_res.y, -1.f);
	float2 Normal = DistortUV(I.texcoord / view_uv.xy, RainAmount, timers.x, Aspect);
	float3 Scene = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0.f).xyz;
	float3 RainScene = s_image.SampleLevel(smp_rtlinear, ViewToTarget(I.texcoord / view_uv.xy + Normal), 0.f).xyz;
	return float4(lerp(Scene, RainScene, RainAmount), 1.f);
}
