#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float3 c0 : COLOR0;
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.tc0 = unpack_tc_lmap(DecodeS16x2(V.X));
    o.tc1 = o.tc0;
    o.c0 = r1_v_hemi(V.N);
    o.fog = r1_fog(mul(m_W, float4(V.P, 1.0f)));

    return o;
}
