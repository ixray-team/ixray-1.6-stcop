#include "common.hlsli"

uniform texture2D s_gamma_lut;

float4 main(float2 tc0 : TEXCOORD0) : SV_Target
{
	float3 image  = s_image.Sample(smp_nofilter, tc0).xyz;

	// Compute the 1D LUT lookup scale/offset factor
	const float lutSize = 1024.0f;
	float scale = (lutSize - 1.0f) / lutSize;
	float offset = 1.0f / (2.0f * lutSize);
	
	// apply
	float3 color = float3(s_gamma_lut.Sample(smp_rtlinear, scale * image.x + offset).x,
						  s_gamma_lut.Sample(smp_rtlinear, scale * image.y + offset).y,
						  s_gamma_lut.Sample(smp_rtlinear, scale * image.z + offset).z);
	
    color = deband_color(color, tc0.xy);
	return float4(color, 1.0f);
}