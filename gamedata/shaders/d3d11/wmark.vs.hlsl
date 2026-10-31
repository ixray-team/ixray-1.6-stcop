#include "common.hlsli"
#include "shared\wmark.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float3 c0 : COLOR0; // c0=all lighting
    float fog : FOG;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    float4 P = wmark_shift(V.P, V.N);
    o.hpos = mul(m_VP, P);
    o.tc0 = V.UV;
    o.c0 = 0;
    o.fog = 1.0f - calc_fogging(V.P);
    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;

    return o;
}

