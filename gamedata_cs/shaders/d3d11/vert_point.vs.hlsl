#include "r1_common.hlsli"
#include "r1_static.hlsli"

vf_point main(v_static_color v)
{
    vf_point o;

    o.hpos = mul(m_WVP, v.P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);

    o.color = calc_point(o.tc1, o.tc2, float4(mul(m_W, v.P), 1.0f), unpack_normal(v.Nh.xyz));

    return o;
}
