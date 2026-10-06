#include "common.hlsli"

float4 main(p_TL I) : SV_Target
{
    float4 res = s_base.Sample(smp_base, I.Tex0) * I.Color;

    // FFP alpha test, D3DCMP_GREATER
    clip(res.a - m_AlphaRef - 0.5f / 255.0f);

    return res;
}
