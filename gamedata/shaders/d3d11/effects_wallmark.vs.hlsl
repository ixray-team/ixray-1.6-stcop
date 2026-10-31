#include "common.hlsli"

// Vertex
v2p_TL main(PULL_INPUT)
{
    v2p_TL O;
    RingVertex V = PullRing(VertexId);

    O.HPos = mul(m_VP, float4(V.P, 1.0f));
    O.Tex0 = V.UV;
    O.Color = V.C;

    O.HPos.xy += m_taa_jitter.xy * O.HPos.w;

    return O;
}
