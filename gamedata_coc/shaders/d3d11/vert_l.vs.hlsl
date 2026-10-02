#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
};

vf main(v_static_color v)
{
    vf o;

    float3 N = unpack_normal(v.Nh.xyz);
    float3 L_rgb = unpack_D3DCOLOR(v.color).xyz;
    float3 L_hemi = r1_v_hemi(N) * v.Nh.w;
    float L_sun = v.color.w;
    float3 L_final = L_rgb + L_hemi + L_ambient.xyz;

    o.hpos = mul(m_WVP, v.P);
    o.c0 = float4(L_final.x, L_final.y, L_final.z, L_sun);
    return o;
}
