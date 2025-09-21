#include "common.hlsli"

uniform float4 color_params; 	// brightness, gamma, contrast, 0
uniform float4 color_grading;	// r, g, b, 0

struct PSInput
{
    float4 hpos : SV_POSITION;
    float4 texcoord : TEXCOORD0;
};

float4 main(in PSInput I) : SV_Target
{
	float c = color_params.x * pow(I.texcoord.x, color_params.y) + color_params.z;
	return float4(saturate(color_grading.xyz * c), 1.0f);
}