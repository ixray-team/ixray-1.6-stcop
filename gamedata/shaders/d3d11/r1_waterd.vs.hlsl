#include "r1_water.hlsli"

vf_waterd main(v_static_color v)
{
    vf_waterd o;
    float4 P = watermove(v.P);
    o.tbase = unpack_tc_base(v.tc, v.T.w, v.B.w);
    o.tdist0 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_0, P.xz, W_DISTORT_AMP_0);
    o.tdist1 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_1, P.xz, W_DISTORT_AMP_1);
    o.hpos = mul(m_VP, P);
    return o;
}
