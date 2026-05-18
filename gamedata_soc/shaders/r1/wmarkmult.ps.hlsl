#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c0 : COLOR0; // c0=all lighting
};

// Pixel
float4 main(v2p I) : COLOR
{
    float4 res = tex2D(s_base, I.tc0);
    res = lerp(0.5f, res, I.c0.a);
    return res;
}
