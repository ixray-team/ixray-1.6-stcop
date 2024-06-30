#include "common.hlsli"
Texture2D s_distort;

float4 main(float2 texcoord : TEXCOORD0, float4 hpos : SV_Position) : SV_Target
{
    float4 distort = s_distort.SampleLevel(smp_nofilter, texcoord, 0);
    float2 offset = distort.xy - (127.0f / 255.0f);

    float2 center = texcoord + offset * def_distort;

    float depth = s_position.SampleLevel(smp_nofilter, texcoord, 0).x;
    float depth_x = s_position.SampleLevel(smp_rtlinear, center, 0).x;

    center = depth_x < 0.03f ? texcoord : center;
    return s_image.SampleLevel(smp_rtlinear, center, 0);
}

