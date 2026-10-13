#include "common.hlsli"

uniform float4 ui_emissive_tint;

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
};

float4 main(v2p I) : SV_Target
{
    // UI needs the material's visible color, not distortion-buffer coordinates.
    float4 Color = I.c * s_base.Sample(smp_base, I.tc);
    Color.xyz = GammaToLinear(Color.xyz) * ui_emissive_tint.xyz;
#ifndef USE_PBR
    Color.w = GammaToLinear(Color.w);
#endif
    Color.w = saturate(Color.w);
    clip(Color.w - EPS);
    return Color;
}
