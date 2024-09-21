#include "common.hlsli"

// vertex output
struct v2p_L
{
	float4 pos : SV_POSITION;
	float4 viewpos : TEXCOORD0;
	float4 color : COLOR0;
};

float4 main(v2p_L I) : SV_TARGET
{
	return I.color;
}
