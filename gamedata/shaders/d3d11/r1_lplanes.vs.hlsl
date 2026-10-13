#include "r1_selflight.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf main(v_selflight v)
{
    vf o;
    o.hpos = mul(m_WVP, v.P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);
    float3 dir_v = normalize(mul(m_WV, v.P));
    float3 norm_v = normalize(mul((float3x3)m_WV, unpack_normal(v.Nh.xyz)));
    o.c0 = abs(dot(dir_v, norm_v));
    return o;
}
