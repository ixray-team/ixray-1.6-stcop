/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Based on awesome tutorial
by AlexanderChristensen: https://learnopengl.com/Guest-Articles/2022/Phys.-Based-Bloom
Which in turn is based on research by
Jorge Jimenez http://www.iryoku.com/publications
*/

#include "common.hlsli"

Texture2D t_image;
float4 upsample_params;

RWTexture2D<float3> u_bloom : register(u0);

[numthreads(8, 8, 1)]
void main(uint3 id : SV_DispatchThreadID)
{
    if (any(id.xy >= (uint2)upsample_params.xy))
        return;
    float2 texcoord = (float2(id.xy) + 0.5f) * upsample_params.zw;
    float2 center = texcoord ;
    float x = 2.f * upsample_params.z;
    float y = 2.f * upsample_params.w;

    float3 a = s_image.SampleLevel(smp_rtlinear, float2 (center.x - x, center.y + y), 0).rgb;
    float3 b = s_image.SampleLevel(smp_rtlinear, float2 (center.x,     center.y + y), 0).rgb;
    float3 c = s_image.SampleLevel(smp_rtlinear, float2 (center.x + x, center.y +  y), 0).rgb;

    float3 d = s_image.SampleLevel(smp_rtlinear, float2 (center.x - x, center.y), 0).rgb;
    float3 e = s_image.SampleLevel(smp_rtlinear, float2 (center.x,     center.y), 0).rgb;
    float3 f = s_image.SampleLevel(smp_rtlinear, float2 (center.x + x, center.y), 0).rgb;

    float3 g = s_image.SampleLevel(smp_rtlinear, float2 (center.x - x, center.y - y), 0).rgb;
    float3 h = s_image.SampleLevel(smp_rtlinear, float2 (center.x,     center.y - y), 0).rgb;
    float3 i = s_image.SampleLevel(smp_rtlinear, float2 (center.x + x, center.y - y), 0).rgb;

    float3 upsample = 0.f;
    upsample += e * 4.f;
    upsample += (b + d + f + h) * 2.f;
    upsample += (a + c + g + i);
    upsample *= 1.f / 16.f;

    float3 prev = t_image.SampleLevel(smp_rtlinear, center, 0).rgb;

    u_bloom[id.xy] = upsample + prev;
}
