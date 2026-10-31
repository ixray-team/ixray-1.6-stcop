#include "common.hlsli"

// Vertex
v2p_TL main(PULL_INPUT)
{
    v2p_TL O;
    RingVertex V = PullRing(VertexId);

    O.HPos = mul(m_VP, float4(V.P, 1.0f)); // xform, input in world coords
    O.HPos.z = O.HPos.w;
    O.Tex0 = V.UV;
    O.Color = V.C;

    return O;
}
