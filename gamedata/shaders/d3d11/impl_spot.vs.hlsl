#include "r1_common.hlsli"
#include "r1_static.hlsli"

vf_spot main(v_static v)
{
    vf_spot o;

    o.hpos = mul(m_VP, v.P); 
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w); 
    
    o.color = calc_spot(o.tc1, o.tc2, v.P, unpack_normal(v.Nh.xyz)); 

    return o;
}
