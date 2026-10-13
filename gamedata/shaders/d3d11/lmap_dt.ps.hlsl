#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float2 tc2 : TEXCOORD2;
    float2 tc3 : TEXCOORD3;
    float4 c0 : COLOR0;
    float4 c1 : COLOR1;
    float fog : TEXCOORD7;
};

float4 main(v2p I) : SV_Target
{
    float4 t_base = r1_sample_base(I.tc0);
    float4 t_lmap = s_lmap.Sample(smp_rtlinear, I.tc1);

    float3 l_base = t_lmap.rgb;
    float3 l_hemi = I.c0.rgb * r1_p_hemi(I.tc2);
    float3 l_sun = I.c1.rgb * t_lmap.a;
    float3 light = L_ambient.xyz + l_base + l_sun + l_hemi;

    float4 t_dt = s_detail.Sample(smp_base, I.tc3);
    float3 detail = t_dt.rgb * I.c0.a + I.c1.a;

    float3 final = (light * t_base.rgb * 2.0f) * detail * 2.0f;
    final = lerp(fog_color.xyz, final, I.fog);

    return float4(final, t_base.a);
}
