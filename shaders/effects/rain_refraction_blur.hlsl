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

float4 ps_main(PSInputFullscreen I) : SV_Target
{
	float2 Step = scaled_screen_res.zw * 2.0f;

	float3 Color = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0).rgb * 4.0f;

	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(-1.0f, -1.0f), 0).rgb;
	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(1.0f, -1.0f), 0).rgb;
	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(-1.0f, 1.0f), 0).rgb;
	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(1.0f, 1.0f), 0).rgb;

	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(-2.0f, 0.0f), 0).rgb * 2.0f;
	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(2.0f, 0.0f), 0).rgb * 2.0f;
	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(0.0f, -2.0f), 0).rgb * 2.0f;
	Color += s_image.SampleLevel(smp_rtlinear, I.texcoord + Step * float2(0.0f, 2.0f), 0).rgb * 2.0f;

	return float4(Color / 16.0f, 1.0f);
}
