#include "r1_water.hlsli"

vf_water main(v_static_color v)
{
    vf_water o;
    float4 P = watermove(v.P);
    float3 N = unpack_normal(v.Nh.xyz);
    float3 T = unpack_normal(v.T.xyz);
    float3 B = unpack_normal(v.B.xyz);
    float4 C = unpack_D3DCOLOR(v.color);

    o.v2point = P.xyz - eye_position;
    o.tbase = unpack_tc_base(v.tc, v.T.w, v.B.w);
    o.tnorm0 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_0, P.xz, W_DISTORT_AMP_0);
    o.tnorm1 = watermove_tc(o.tbase * W_DISTORT_BASE_TILE_1, P.xz, W_DISTORT_AMP_1);

    float3x3 xform = mul((float3x3)m_W, float3x3(T.x, B.x, N.x, T.y, B.y, N.y, T.z, B.z, N.z));
    o.M1 = xform[0];
    o.M2 = xform[1];
    o.M3 = xform[2];

    float3 L = C.rgb + r1_v_hemi(N) * unpack_D3DCOLOR(v.Nh).w + r1_v_sun(N) * C.w + L_ambient.xyz;
    o.hpos = mul(m_VP, P);
    o.c0 = float4(L, r1_fog(v.P.xyz));
    return o;
}
