#include "common.hlsli"


// Vertex
v2p_volume main(PULL_INPUT)
{
    float4 P = float4(PullVertex(VertexId).P, 1.0f);
    v2p_volume O;
    O.hpos = mul(m_WVP, P);
    O.tc = mul(m_texgen, P);
    return O;
}

