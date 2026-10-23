#include "common.hlsli"
#include "shared\ui_icon.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c : COLOR0; // diffuse
};

float4 main(v2p I) : COLOR
{
    // UI needs the material's visible color, not distortion-buffer coordinates.
    float4 Color = I.c * tex2D(s_base, I.tc0);
    Color.w = saturate(Color.w);
    clip(Color.w - (1.0f / 255.0f));

    Color.xyz *= ui_emissive_tint.xyz * ui_static_color.xyz;
    Color.w *= ui_static_color.w;
    return Color;
}
