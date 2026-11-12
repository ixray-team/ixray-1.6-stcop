#include "common.hlsli"


// Alpha is written as 1: the debug scene target has an alpha channel and is shown through ImGui
float4 main(PSInputFullscreen I) : SV_Target
{
	float3 color = s_image[I.hpos.xy].xyz;
	
	color = color_params.x * pow(abs(color), color_params.y) + color_params.z;
	color = saturate(color.xyz * color_grading.xyz);
	
	#ifdef USE_R1_STATIC_LIGHTING
	return float4(color, 1.0f);
#else
	return float4(deband_color(color, I.hpos.xy), 1.0f);
#endif
}