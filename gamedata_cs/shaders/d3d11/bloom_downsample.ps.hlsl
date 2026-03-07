/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Based on awesome tutorial
by AlexanderChristensen: https://learnopengl.com/Guest-Articles/2022/Phys.-Based-Bloom
Which in turn is based on research by
Jorge Jimenez http://www.iryoku.com/publications
*/
#include "common.hlsli"

Texture2D b_image;
float4 downsample_params;

float4 main(PSInputFullscreen I) : SV_Target
{
    float4 res = 0.f;
    float2 center = I.texcoord.xy;
    float x = 0.5f * downsample_params.z;
    float y = 0.5f * downsample_params.w;
    
    float3 a = b_image.Sample(smp_rtlinear, float2(center.x - 2.f * x, center.y + 2.f * y)).rgb;
    float3 b = b_image.Sample(smp_rtlinear, float2(center.x, center.y + 2.f * y)).rgb;
    float3 c = b_image.Sample(smp_rtlinear, float2(center.x + 2.f * x, center.y + 2.f * y)).rgb;

    float3 d = b_image.Sample(smp_rtlinear, float2(center.x - 2.f * x, center.y)).rgb;
    float3 e = b_image.Sample(smp_rtlinear, float2(center.x, center.y)).rgb;
    float3 f = b_image.Sample(smp_rtlinear, float2(center.x + 2.f * x, center.y)).rgb;

    float3 g = b_image.Sample(smp_rtlinear, float2(center.x - 2.f * x, center.y - 2.f * y)).rgb;
    float3 h = b_image.Sample(smp_rtlinear, float2(center.x, center.y - 2.f * y)).rgb;
    float3 i = b_image.Sample(smp_rtlinear, float2(center.x + 2.f * x, center.y - 2.f * y)).rgb;

    float3 j = b_image.Sample(smp_rtlinear, float2(center.x - x, center.y + y)).rgb;
    float3 k = b_image.Sample(smp_rtlinear, float2(center.x + x, center.y + y)).rgb;
    float3 l = b_image.Sample(smp_rtlinear, float2(center.x - x, center.y - y)).rgb;
    float3 m = b_image.Sample(smp_rtlinear, float2(center.x + x, center.y - y)).rgb;
    
    float3 downsample = 0.f;
    downsample += e * 0.125f;
    downsample += (a + c + g + i) * 0.03125f;
    downsample += (b + d + f + h) * 0.0625f;
    downsample += (j + k + l + m) * 0.125f;
    
    //downsample = b_remap(downsample, float2(0.0f, 0.1f));

    return res = float4 (downsample, 1.f);
}
