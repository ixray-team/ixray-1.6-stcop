#include "r1_selflight.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);
    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.tc0 = V.UV;
    float3 dir_v = normalize(mul(m_WV, float4(V.P, 1.0f)));
    float3 norm_v = normalize(mul((float3x3)m_WV, V.N));
    o.c0 = abs(dot(dir_v, norm_v));
    return o;
}
