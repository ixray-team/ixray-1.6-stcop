#include "r1_wmark.hlsli"

vf_point main(PULL_INPUT)
{
    vf_point o;
    PulledVertex V = PullVertex(VertexId);
    float3 N = V.N;
    float4 P = wmark_shift(mul(m_W, float4(V.P, 1.0f)), N);
    o.hpos = mul(m_VP, P);
    o.tc0 = V.UV;
    o.color = calc_point(o.tc1, o.tc2, P, N);
    return o;
}
