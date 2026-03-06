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

float4 main(PSInputFullscreen I) : SV_Target
{
    float4 res = 0.f;
    float2 center = I.texcoord.xy ;
    float x = 2.f * upsample_params.z;
    float y = 2.f * upsample_params.w;

    float3 a = s_image.Sample (smp_rtlinear, float2 (center.x - x, center.y + y)).rgb;
    float3 b = s_image.Sample (smp_rtlinear, float2 (center.x,     center.y + y)).rgb;
    float3 c = s_image.Sample (smp_rtlinear, float2 (center.x + x, center.y +  y)).rgb;

    float3 d = s_image.Sample (smp_rtlinear, float2 (center.x - x, center.y)).rgb;
    float3 e = s_image.Sample (smp_rtlinear, float2 (center.x,     center.y)).rgb;
    float3 f = s_image.Sample (smp_rtlinear, float2 (center.x + x, center.y)).rgb;

    float3 g = s_image.Sample (smp_rtlinear, float2 (center.x - x, center.y - y)).rgb;
    float3 h = s_image.Sample (smp_rtlinear, float2 (center.x,     center.y - y)).rgb;
    float3 i = s_image.Sample (smp_rtlinear, float2 (center.x + x, center.y - y)).rgb;

    float3 upsample = 0.f;
    upsample += e * 4.f;
    upsample += (b + d + f + h) * 2.f;
    upsample += (a + c + g + i);
    upsample *= 1.f / 16.f;

    float3 prev = t_image.Sample(smp_rtlinear, center);

    return res = float4 (upsample + prev, 1.f);
}
