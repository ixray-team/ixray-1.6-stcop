#include "common.hlsli"
#include "common_iostructs.hlsli"

v2p_TL main(PULL_INPUT)
{
    v2p_TL O;
    RingVertex V = PullRing(VertexId);

    O.HPos = mul(m_WVP, float4(V.P, asfloat(V.X)));
    O.Tex0 = V.UV;
    O.Color = V.C;

    return O;
}
