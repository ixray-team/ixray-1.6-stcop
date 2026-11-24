#include "common\common.hlsli"

#include "common\extra_cb.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
};

float4 ps_main(v2p I) : SV_Target
{
    // UI needs the material's visible color, not distortion-buffer coordinates.
    float4 Color = I.c * s_base.Sample(smp_base, I.tc);
    Color.xyz = SceneFromGamma(Color.xyz) * ui_emissive_tint.xyz;
#ifndef USE_PBR
    Color.w = SceneFromGamma(Color.w);
#endif
    Color.w = saturate(Color.w);
    clip(Color.w - EPS);
    return Color;
}
