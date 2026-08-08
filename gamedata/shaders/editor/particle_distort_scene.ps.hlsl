#include "common.hlsli"

struct v2p
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0;
    float4 proj : TEXCOORD1;
    float4 c : COLOR0;
    float fog : FOG;
};

Texture2D s_distort;
Texture2D s_image;

float4 main(v2p I) : SV_Target
{
    float2 uv = I.proj.xy / I.proj.w * float2(0.5f, -0.5f) + 0.5f;
    float4 distort = s_distort.Sample(smp_nofilter, I.tc0);
    float factor = distort.a * dot(I.c.xyz, 0.3333f);
    float2 offset = (distort.xy - (127.0f / 255.0f)) * factor;
    float3 image = s_image.Sample(smp_nofilter, uv + offset * def_distort).xyz;

    return float4(image, factor);
}
