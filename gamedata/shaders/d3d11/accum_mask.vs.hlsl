#include "common.hlsli"

float4 main(PULL_INPUT) : SV_POSITION
{
    return mul(m_WVP, float4(PullVertex(VertexId).P, 1.0f));
}

