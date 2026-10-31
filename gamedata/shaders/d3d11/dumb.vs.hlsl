#include "common.hlsli"

// Vertex
v2p_dumb main(PULL_INPUT)
{
    v2p_dumb O;

    O.HPos = mul(m_WVP, float4(PullVertex(VertexId).P, 1.0f));

    return O;
}
