#include "common.hlsli"

uniform float4 static_color;

float4 main(PSInputFullscreen I) : SV_Target
{
	float4 Color = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0);
	Color.xyz = tonemap(Color.xyz, 1.5f);
	Color.w = Color.w > 0.0f; // temp

	return Color * static_color;
}

