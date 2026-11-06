#include "r1_common.hlsli"
#include "r1_static.hlsli"

vf_point main(PULL_INPUT)
{
    vf_point o;
    PulledVertex V = PullVertex(VertexId);

    o.hpos = mul(m_VP, float4(V.P, 1.0f)); 
    o.tc0 = V.UV; 
    
    o.color = calc_point(o.tc1, o.tc2, float4(V.P, 1.0f), V.N); 

    return o;
}
