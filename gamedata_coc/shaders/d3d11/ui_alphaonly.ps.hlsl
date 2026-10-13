#include "common.hlsli"

uniform float4 ui_emissive_tint;

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;
};

float4 main(v2p I) : SV_Target
{
    // Match the visible world pass without displaying distortion RGB channels.
    float Coverage = saturate(GammaToLinear(I.c.w * s_base.Sample(smp_base, I.tc).w));
    clip(Coverage - EPS);
    return float4(Coverage * ui_emissive_tint.xyz, Coverage);
}
