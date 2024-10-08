#include "common.hlsli"

float4 main(p_bumped_new I) : SV_Target0
{
    discard;
    return float4(s_base.Sample(smp_base, I.tcdh.xy).xyz, 0.0f);
}

// THIS SHADER SHOD BE DELEATED OR FIXED
