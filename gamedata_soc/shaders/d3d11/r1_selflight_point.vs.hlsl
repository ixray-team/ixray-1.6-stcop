#include "r1_selflight.hlsli"

vf_point main(v_selflight v)
{
    vf_point o;
    o.hpos = mul(m_WVP, v.P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);
    o.color = calc_point(o.tc1, o.tc2, float4(mul(m_W, v.P), 1.0f), unpack_normal(v.Nh.xyz));
    return o;
}
