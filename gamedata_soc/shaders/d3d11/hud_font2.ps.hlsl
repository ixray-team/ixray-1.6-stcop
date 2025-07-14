#include "common.hlsli"

// Pixel
float4 main(p_TL I) : SV_Target
{
    float4 r = s_base.Sample(smp_base, I.Tex0);
    r.w = 1 - r.w;

    return r;
}

