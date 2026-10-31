#include "common.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0; // base
    float4 c0 : COLOR0; // color
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);
    float4 P = float4(V.P, 1.0f);

    o.hpos = mul(m_WVP, P);
    o.tc0 = V.UV;

    float3 dir_v = normalize(mul(m_WV, P));
    float3 norm_v = normalize(mul((float3x3)m_WV, V.N));
    float fade = abs(dot(dir_v, norm_v));
    o.c0 = fade;

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;

    return o;
}

