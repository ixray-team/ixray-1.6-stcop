#include "r1_selflight.hlsli"

vf_point main(PULL_INPUT)
{
    vf_point o;
    PulledVertex V = PullVertex(VertexId);
    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.tc0 = V.UV;
    o.color = calc_point(o.tc1, o.tc2, float4(mul(m_W, float4(V.P, 1.0f)), 1.0f), V.N);
    return o;
}
