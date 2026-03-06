#include "common.hlsli"

uniform float4 color_params;
uniform float4 color_grading;

float4 main(PSInputFullscreen I) : SV_Target
{
	float3 color = s_image[I.hpos.xy].xyz;
	
	color = color_params.x * pow(abs(color), color_params.y) + color_params.z;
	color = saturate(color.xyz * color_grading.xyz);
	
    color = deband_color(color, I.hpos.xy);
	return float4(color, 1.0f);
}