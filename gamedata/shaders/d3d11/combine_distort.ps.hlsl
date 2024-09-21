#include "common.hlsli"
Texture2D s_distort;

float4 main(v2p_TL Input) : SV_Target
{
    float4 distort = s_distort.SampleLevel(smp_nofilter, Input.Tex0, 0);
    float2 offset = distort.xy - (127.0f / 255.0f);

    float2 center = Input.Tex0 + offset * def_distort;

    float depth = s_position.SampleLevel(smp_nofilter, Input.Tex0, 0).x;
    float depth_x = s_position.SampleLevel(smp_nofilter, center, 0).x;

    center = depth_x < depth ? Input.Tex0 : center;
    return s_image.SampleLevel(smp_nofilter, center, 0);
}

