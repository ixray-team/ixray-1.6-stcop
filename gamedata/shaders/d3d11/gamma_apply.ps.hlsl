#include "common.hlsli"


float3 main(PSInputFullscreen I) : SV_Target
{
	float3 color = s_image[I.hpos.xy].xyz;
	
	color = color_params.x * pow(abs(color), color_params.y) + color_params.z;
	color = saturate(color.xyz * color_grading.xyz);
	
	return deband_color(color, I.hpos.xy);
}