#include "common.hlsli"

float4 main(p_bumped_new I) : SV_Target
{
    return float4(PushGamma(s_base.Sample(smp_base, I.tcdh.xy).xyz), 0.0f);
}

