#include "common\common.hlsli"
#include "common\common_iostructs.hlsli"

v2p_TL vs_main(PULL_INPUT)
{
    v2p_TL O;
    RingVertex V = PullRing(VertexId);

    O.HPos = mul(m_WVP, float4(V.P, asfloat(V.X)));
    O.Tex0 = V.UV;
    O.Color = V.C;

    return O;
}

float4 ps_main(p_TL I) : SV_Target
{
    return s_base.Sample(smp_base, I.Tex0) * I.Color;
}
