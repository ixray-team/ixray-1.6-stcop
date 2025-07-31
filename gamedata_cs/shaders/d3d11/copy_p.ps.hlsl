#include "common.hlsli"

float4 main(float4 tc : TEXCOORD0) : SV_Target
{
    return s_generic.Sample(smp_nofilter, tc.xy / tc.w);
}

