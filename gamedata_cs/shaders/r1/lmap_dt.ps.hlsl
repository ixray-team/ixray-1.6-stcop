#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float2 tc1 : TEXCOORD1; // lmap
    float2 tc2 : TEXCOORD2; // hemi
    float2 tc3 : TEXCOORD3; // detail
    float4 c0 : COLOR0; // c0.a *
    float4 c1 : COLOR1; // c1.a +
    float fog : FOG;
};

// Pixel
float4 main(v2p I) : COLOR
{
    float4 t_base = tex2D(s_base, I.tc0);
    float4 t_lmap = tex2D(s_lmap, I.tc1);

    // lighting
    float3 l_base = t_lmap.rgb; // base light-map
    float3 l_hemi = I.c0 * p_hemi(I.tc2); // hemi
    float3 l_sun = I.c1 * t_lmap.a; // sun color
    float3 light = L_ambient + l_base + l_sun + l_hemi;

    // calc D-texture
    float4 t_dt = tex2D(s_detail, I.tc3);
    float3 detail = t_dt * I.c0.a + I.c1.a;

    // final-color
    float3 final = (light * t_base * 2.0f) * detail * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);

    // out
    return float4(final, t_base.a);
}
