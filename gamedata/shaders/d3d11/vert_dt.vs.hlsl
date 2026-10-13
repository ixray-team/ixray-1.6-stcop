#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float4 c0 : COLOR0;
    float4 c1 : COLOR1;
    float fog : TEXCOORD7;
    float4 hpos : SV_POSITION;
};

vf main(v_static_color v)
{
    vf o;

    float3 N = unpack_normal(v.Nh.xyz);
    o.hpos = mul(m_WVP, v.P);
    o.tc0 = unpack_tc_base(v.tc, v.T.w, v.B.w);

    o.tc1 = o.tc0 * dt_params.xy;

    float3 L_rgb = unpack_D3DCOLOR(v.color).xyz;
    float3 L_hemi = r1_v_hemi(N) * v.Nh.w;
    float3 L_sun = r1_v_sun(N) * v.color.w;
    float3 L_final = L_rgb + L_hemi + L_sun + L_ambient.xyz;

    float2 dt = calc_detail(mul(m_W, v.P));

    o.c0 = float4(L_final.x, L_final.y, L_final.z, dt.x);
    o.c1 = dt.y;
    o.fog = r1_fog(mul(m_W, v.P));

    return o;
}
