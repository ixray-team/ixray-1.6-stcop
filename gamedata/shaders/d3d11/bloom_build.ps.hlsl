#include "common.hlsli"

uniform float4 b_params;

float4 main(p_build I) : SV_Target
{
    float3 s0 = s_image.Sample(smp_rtlinear, I.Tex0.xy).xyz;
    float3 s1 = s_image.Sample(smp_rtlinear, I.Tex1.xy).xyz;
    float3 s2 = s_image.Sample(smp_rtlinear, I.Tex2.xy).xyz;
    float3 s3 = s_image.Sample(smp_rtlinear, I.Tex3.xy).xyz;

    float3 avg = (s0 + s1 + s2 + s3) / (2.0f * def_hdr);
    float hi = dot(avg, 1.h) - b_params.x;

    return float4(avg, hi);
}

