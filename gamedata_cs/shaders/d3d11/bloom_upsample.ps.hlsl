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

void main(in PSInputFullscreen I, out float3 upsample : SV_Target)
{
    float2 center = I.texcoord.xy;
	
    float x = 2.f * upsample_params.z;
    float y = 2.f * upsample_params.w;

    float3 a = s_image.Sample (smp_rtlinear, float2 (center.x - x, center.y + y)).rgb;
    float3 b = s_image.Sample (smp_rtlinear, float2 (center.x,     center.y + y)).rgb;
    float3 c = s_image.Sample (smp_rtlinear, float2 (center.x + x, center.y + y)).rgb;

    float3 d = s_image.Sample (smp_rtlinear, float2 (center.x - x, center.y)).rgb;
    float3 e = s_image.Sample (smp_rtlinear, float2 (center.x,     center.y)).rgb;
    float3 f = s_image.Sample (smp_rtlinear, float2 (center.x + x, center.y)).rgb;

    float3 g = s_image.Sample (smp_rtlinear, float2 (center.x - x, center.y - y)).rgb;
    float3 h = s_image.Sample (smp_rtlinear, float2 (center.x,     center.y - y)).rgb;
    float3 i = s_image.Sample (smp_rtlinear, float2 (center.x + x, center.y - y)).rgb;
	
    upsample = t_image.Sample(smp_rtlinear, center);

    upsample += e * 0.25f;
    upsample += (b + d + f + h) * 0.125f;
    upsample += (a + c + g + i) * 0.0625f;
}

