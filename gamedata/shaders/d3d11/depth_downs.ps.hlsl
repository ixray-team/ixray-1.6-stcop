#include "common.hlsli"

struct _input
{
    float4 tc0 : TEXCOORD0; // tc.xy, tc.w = tonemap scale
    float2 tcJ : TEXCOORD1; // jitter coords
    float4 pos2d : SV_Position;
};

float4 main(_input I) : SV_Target0
{
	float4 Depth;

#ifndef SM_5
    Depth.x = s_position.SampleLevel(smp_nofilter, I.tc0.xy, int2(1, 0), 0).x;
    Depth.y = s_position.SampleLevel(smp_nofilter, I.tc0.xy, int2(1, 1), 0).x;
    Depth.z = s_position.SampleLevel(smp_nofilter, I.tc0.xy, int2(0, 1), 0).x;
    Depth.w = s_position.SampleLevel(smp_nofilter, I.tc0.xy, int2(0, 0), 0).x;
#else // !SM_5
    Depth = s_position.GatherRed(smp_nofilter, I.tc0.xy);
#endif // SM_5

    Depth = depth_unpack.x * rcp(Depth - depth_unpack.y);
	return min(min(Depth.x, Depth.y), min(Depth.z, Depth.w));
}

