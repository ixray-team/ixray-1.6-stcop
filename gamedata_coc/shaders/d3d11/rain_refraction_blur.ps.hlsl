#include "common.hlsli"

float4 main(PSInputFullscreen I) : SV_Target
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
