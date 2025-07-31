#include "common.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0; // base & distort
};

Texture2D s_distort;

// Pixel
float4 main(v2p I) : SV_Target
{
    float2 distort = s_distort.Sample(smp_rtlinear, I.tc).xy;
    float2 offset = (distort - 127.0f / 255.0f) * def_distort;
    float3 image = s_base.Sample(smp_rtlinear, I.tc + offset).xyz;

    return float4(image, 1);
}

