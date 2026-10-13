#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; 
    float2 tc1 : TEXCOORD1; 
    float2 tc2 : TEXCOORD2; 
    float2 tc3 : TEXCOORD3; 
    float4 c : COLOR0; 
    float4 f : COLOR1; 
    float fog : TEXCOORD7;
};

Texture2D s_base0;
Texture2D s_base1;
Texture2D s_hemi0;
Texture2D s_hemi1;

float4 main(v2p I) : SV_Target
{
    float4 base0 = s_base0.Sample(smp_base, I.tc0);
    float4 base1 = s_base1.Sample(smp_base, I.tc1);
    float4 hemi0 = s_hemi0.Sample(smp_base, I.tc2);
    float4 hemi1 = s_hemi1.Sample(smp_base, I.tc3);

    float4 base = lerp(base0, base1, I.f.w) * I.c;
    clip(base.a - m_AlphaRef);
    float hemi = lerp(hemi0, hemi1, I.f.w).w;

    float3 color = base.rgb * 2.0f * (0.5f + 0.5f * hemi);
    color = lerp(fog_color.xyz, color, I.fog);

    return float4(color, base.a);
}
