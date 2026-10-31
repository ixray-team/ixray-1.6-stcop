#include "common.hlsli"

struct _input
{
    float4 tc0 : TEXCOORD0; // tc.xy, tc.w = tonemap scale
    float2 tcJ : TEXCOORD1; // jitter coords
    float4 pos2d : SV_POSITION;
};


float4 main(_input I) : SV_Target0
{
	float4 Depth = s_position.GatherRed(smp_nofilter, I.tc0.xy + 0.5f * scaled_screen_res.zw);

    // Depth = depth_unpack.x * rcp(Depth - depth_unpack.y);
	return min(min(Depth.x, Depth.y), min(Depth.z, Depth.w));
}

