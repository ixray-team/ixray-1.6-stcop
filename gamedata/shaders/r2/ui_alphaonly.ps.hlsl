#include "common.hlsli"
#include "shared\ui_icon.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c : COLOR0; // diffuse
};

float4 main(v2p I) : COLOR
{
    // Match the visible world pass without displaying distortion RGB channels.
    float Coverage = saturate(I.c.w * tex2D(s_base, I.tc0).w);
    clip(Coverage - (1.0f / 255.0f));
    return float4(ui_emissive_tint.xyz * ui_static_color.xyz, Coverage * ui_static_color.w);
}
