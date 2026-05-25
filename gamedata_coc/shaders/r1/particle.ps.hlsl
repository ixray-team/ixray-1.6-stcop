#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c : COLOR0; // diffuse
};

// Pixel
float4 main(v2p I) : COLOR
{
    float4 result = I.c * tex2D(s_base, I.tc0);
    clip(result.a - (0.01f / 255.0f));

    return result;
}
