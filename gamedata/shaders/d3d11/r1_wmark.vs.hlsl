#include "r1_wmark.hlsli"

vf_wmark main(v_static_color v)
{
    vf_wmark o;
    float3 N = unpack_normal(v.Nh.xyz);
    float4 C = unpack_D3DCOLOR(v.color);
    float4 P = wmark_shift(mul(m_W, v.P), N);
    o.hpos = mul(m_VP, P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);
    o.c0.rgb = C.rgb + r1_v_hemi(N) * unpack_D3DCOLOR(v.Nh).w + r1_v_sun(N) * C.w + L_ambient.xyz;
    o.c0.a = r1_fog(P.xyz);
    return o;
}
