#include "r1_selflight.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float fog : TEXCOORD1;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);
    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.tc0 = V.UV;
    o.fog = r1_fog(mul(m_W, float4(V.P, 1.0f)));
    return o;
}
