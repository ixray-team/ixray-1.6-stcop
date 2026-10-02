#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float3 c0 : COLOR0;
    float fog : TEXCOORD7;
};

float4 main(v2p I) : SV_Target
{
    float4 t_lmap = s_lmap.Sample(smp_rtlinear, I.tc0);

    float3 l_base = t_lmap.rgb;
    float3 l_hemi = I.c0.rgb * r1_p_hemi(I.tc1);
    float l_sun = t_lmap.a;
    float3 light = L_ambient.xyz + l_base + l_hemi;
    light = lerp(fog_color.xyz, light, I.fog);

    return float4(light, l_sun);
}
