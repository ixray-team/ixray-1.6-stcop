#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; 
    float2 tc1 : TEXCOORD1; 
    float2 tc2 : TEXCOORD2; 
    float4 c0 : COLOR0; 
    float4 c1 : COLOR1; 
    float fog : TEXCOORD7;
};








float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    float4 t_mask = s_mask.Sample(smp_base, I.tc0);

    t_mask = t_mask / dot(t_mask, 1.0);

    float4 t_lmap = s_lmap.Sample(smp_base, I.tc1);

    float3 l_base = t_lmap.rgb; 
    float3 l_hemi = I.c0 * t_base.a; 
    float3 l_sun = I.c1 * t_lmap.a; 
    float3 light = L_ambient.xyz + l_base + l_sun + l_hemi;

    float3 t_dt_r = s_dt_r.Sample(smp_base, I.tc2) * t_mask.r;
    float3 t_dt_g = s_dt_g.Sample(smp_base, I.tc2) * t_mask.g;
    float3 t_dt_b = s_dt_b.Sample(smp_base, I.tc2) * t_mask.b;
    float3 t_dt_a = s_dt_a.Sample(smp_base, I.tc2) * t_mask.a;
    float3 detail = t_dt_a + t_dt_b + t_dt_g + t_dt_r;

    float3 final = (light * t_base.rgb * 2.0f) * detail * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);
    
    return float4(final.rgb, 1.0f);
}
