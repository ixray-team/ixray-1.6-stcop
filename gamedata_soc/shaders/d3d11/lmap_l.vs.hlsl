#define USE_LM_HEMI
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

vf main(v_static v)
{
    vf o;

    o.hpos = mul(m_WVP, v.P);
    o.tc0 = unpack_tc_lmap(v.lmh);
    o.tc1 = o.tc0;
    o.c0 = r1_v_hemi(unpack_normal(v.Nh.xyz));
    o.fog = r1_fog(mul(m_W, v.P));

    return o;
}
