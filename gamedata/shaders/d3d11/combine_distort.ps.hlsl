#include "common.hlsli"
Texture2D s_distort;

float4 main(float2 tc : TEXCOORD0, float4 hpos : SV_Position) : SV_Target
{
    float4 distort = s_distort.Sample(smp_nofilter, tc);
    float2 offset = (distort.xy - (127.0f / 255.0f)) * def_distort;

    gbuffer_data gbd = gbuffer_load_data(tc, hpos);

    float depth = gbd.P.z;
    float2 center = tc + offset;

    gbuffer_data gbdx = gbuffer_load_data_offset(tc, center, hpos);
    float depth_x = gbdx.P.z;

    if (depth_x < depth)
    {
        center = tc;
    }

    return s_image.Sample(smp_rtlinear, center);
}
