#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; 
    float2 tc1 : TEXCOORD1; 
    float2 tch : TEXCOORD2; 
    float3 tc2 : TEXCOORD3; 
    float3 c0 : COLOR0;
    float3 c1 : COLOR1;
    float fog : TEXCOORD7;
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tc0);
    float4 t_lmap = s_lmap.Sample(smp_base, I.tc1);
    float4 t_env = s_env.Sample(smp_rtlinear, I.tc2);

    float3 l_base = t_lmap.rgb; 
    float3 l_hemi = I.c0 * r1_p_hemi(I.tch); 
    float3 l_sun = I.c1 * t_lmap.a; 
    float3 light = L_ambient.xyz + l_base + l_sun + l_hemi;

    float3 base = lerp(t_env, t_base, t_base.a);
    float3 final = light * base * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);

    return float4(final, t_base.a);
}
