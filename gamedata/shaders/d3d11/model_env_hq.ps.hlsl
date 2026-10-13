#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; 
    float3 tc1 : TEXCOORD1; 
    float4 tc2 : TEXCOORD2; 
    float3 c0 : COLOR0; 
    float4 c1 : COLOR1; 
    float fog : TEXCOORD7;
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = r1_sample_base(I.tc0);
    float4 t_env = s_env.Sample(smp_rtlinear, I.tc1);
    float4 t_lmap = s_lmap.Sample(smp_rtlinear, (I.tc2).xy / (I.tc2).w);

    float3 l_base = t_lmap.rgb; 
    float3 l_sun = I.c0 * t_lmap.a; 
    float3 light = lerp(l_base + l_sun, I.c1, I.c1.w);

    float3 base = lerp(t_env, t_base, t_base.a);
    float3 final = light * base * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);

    return float4(final.r, final.g, final.b, t_base.a);
}
