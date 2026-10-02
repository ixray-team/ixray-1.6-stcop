#include "r1_wmark.hlsli"

float4 main(vf_wmark I) : SV_Target
{
    return lerp(0.5f, s_base.Sample(smp_base, I.tc0), I.c0.a);
}
