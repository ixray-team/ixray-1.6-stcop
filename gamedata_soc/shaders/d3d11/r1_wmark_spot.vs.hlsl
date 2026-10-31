#include "r1_wmark.hlsli"

vf_spot main(v_static_color v)
{
    vf_spot o;
    float3 N = unpack_normal(v.Nh.xyz);
    float4 P = wmark_shift(mul(m_W, v.P), N);
    o.hpos = mul(m_VP, P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);
    o.color = calc_spot(o.tc1, o.tc2, P, N);
    return o;
}
