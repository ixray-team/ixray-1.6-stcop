#include "common.hlsli"

uniform float4 screen_res;

// vertex output
struct v2p_L
{
	float4 pos : SV_POSITION;
	float4 viewpos : TEXCOORD0;
	float4 color : COLOR0;
};

float4 main(v2p_L I) : SV_TARGET
{
	float depth = s_position.SampleLevel(smp_nofilter, I.pos.xy * screen_res.zw, 0).x;
	depth = depth_unpack.x / (depth - depth_unpack.y);
	clip(depth - I.viewpos.z);
	
	return I.color;
}
