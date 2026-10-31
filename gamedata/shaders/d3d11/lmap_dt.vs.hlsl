#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float2 tch : TEXCOORD2;
    float2 tc2 : TEXCOORD3;
    float4 c0 : COLOR0;
    float4 c1 : COLOR1;
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf main(PULL_INPUT)
{
    vf o;
    PulledVertex V = PullVertex(VertexId);

    float2 dt = calc_detail(mul(m_W, float4(V.P, 1.0f)));
    float3 N = V.N;

    o.hpos = mul(m_WVP, float4(V.P, 1.0f));
    o.tc0 = V.UV;

    o.tc1 = unpack_tc_lmap(DecodeS16x2(V.X));
    o.tch = o.tc1;
    o.tc2 = o.tc0 * dt_params.xy;
    o.c0 = float4(r1_v_hemi(N), dt.x);
    o.c1 = float4(r1_v_sun(N), dt.y);
    o.fog = r1_fog(mul(m_W, float4(V.P, 1.0f)));

    return o;
}
