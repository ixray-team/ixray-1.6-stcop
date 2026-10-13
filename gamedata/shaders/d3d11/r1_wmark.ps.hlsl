#include "r1_wmark.hlsli"

float4 main(vf_wmark I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    return float4(t_base.rgb * I.c0.rgb * 2.0f, t_base.a * I.c0.a);
}
