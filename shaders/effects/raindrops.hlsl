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

Texture2D s_droplets : register(t25);

#include "common\extra_cb.hlsli"

float4 ps_main(PSInputFullscreen I) : SV_Target
{
	float4 rain_drops_distortion = s_droplets.Sample(smp_rtlinear, I.texcoord / view_uv.xy);
	float2 texcoord_offset = (rain_drops_distortion.xy - (127.0f / 255.0f)) * def_distort;
	float2 texcoord = ViewToTarget(I.texcoord / view_uv.xy - texcoord_offset * saturate(visor_rain.x));
	float3 scene = s_image.Sample(smp_rtlinear, texcoord).xyz;

	return float4(scene, 1.0f);
}
