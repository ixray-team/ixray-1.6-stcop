#include "common.hlsli"

uniform float4 color_params; 	// brightness, gamma, contrast, 0
uniform float4 color_grading;	// r, g, b, 0

#define EPS 0.0001f

float4 main(float2 tc0 : TEXCOORD0) : SV_Target
{
	float	og 		= 1.0f / (color_params.y + EPS);
	float	B 		= color_params.x / 2.0f;
	float	C 		= color_params.z / 2.0f;
	
	float	c 		= (C + 0.5f) * pow(tc0.x, og) + (B - 0.5f) * 0.5f - C * 0.5f + 0.25f;

	float3 	color 	= float3(saturate(c*color_grading.x),
							 saturate(c*color_grading.y),
							 saturate(c*color_grading.z));

	return float4(color, 1.0f);
}