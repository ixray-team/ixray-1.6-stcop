/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Based on awesome tutorial
by AlexanderChristensen: https://learnopengl.com/Guest-Articles/2022/Phys.-Based-Bloom
Which in turn is based on research by
Jorge Jimenez http://www.iryoku.com/publications
*/
#include "common.hlsli"
float4 downsample_params;

RWTexture2D<float3> u_bloom : register(u0);

[numthreads(8, 8, 1)]
void main(uint3 id : SV_DispatchThreadID)
{
    if (any(id.xy >= (uint2)downsample_params.xy))
        return;
    float2 texcoord = (float2(id.xy) + 0.5f) * downsample_params.zw;
    float2 center = texcoord;
    float x = 0.5f * downsample_params.z;
    float y = 0.5f * downsample_params.w;
    
    float3 a = s_image.SampleLevel(smp_rtlinear, float2(center.x - 2.f * x, center.y + 2.f * y), 0).rgb;
    float3 b = s_image.SampleLevel(smp_rtlinear, float2(center.x, center.y + 2.f * y), 0).rgb;
    float3 c = s_image.SampleLevel(smp_rtlinear, float2(center.x + 2.f * x, center.y + 2.f * y), 0).rgb;

    float3 d = s_image.SampleLevel(smp_rtlinear, float2(center.x - 2.f * x, center.y), 0).rgb;
    float3 e = s_image.SampleLevel(smp_rtlinear, float2(center.x, center.y), 0).rgb;
    float3 f = s_image.SampleLevel(smp_rtlinear, float2(center.x + 2.f * x, center.y), 0).rgb;

    float3 g = s_image.SampleLevel(smp_rtlinear, float2(center.x - 2.f * x, center.y - 2.f * y), 0).rgb;
    float3 h = s_image.SampleLevel(smp_rtlinear, float2(center.x, center.y - 2.f * y), 0).rgb;
    float3 i = s_image.SampleLevel(smp_rtlinear, float2(center.x + 2.f * x, center.y - 2.f * y), 0).rgb;

    float3 j = s_image.SampleLevel(smp_rtlinear, float2(center.x - x, center.y + y), 0).rgb;
    float3 k = s_image.SampleLevel(smp_rtlinear, float2(center.x + x, center.y + y), 0).rgb;
    float3 l = s_image.SampleLevel(smp_rtlinear, float2(center.x - x, center.y - y), 0).rgb;
    float3 m = s_image.SampleLevel(smp_rtlinear, float2(center.x + x, center.y - y), 0).rgb;
    
	a = clamp(a, 0.0f, 16.0f);
	b = clamp(b, 0.0f, 16.0f);
	c = clamp(c, 0.0f, 16.0f);
	d = clamp(d, 0.0f, 16.0f);
	e = clamp(e, 0.0f, 16.0f);
	f = clamp(f, 0.0f, 16.0f);
	g = clamp(g, 0.0f, 16.0f);
	h = clamp(h, 0.0f, 16.0f);
	i = clamp(i, 0.0f, 16.0f);
	j = clamp(j, 0.0f, 16.0f);
	k = clamp(k, 0.0f, 16.0f);
	l = clamp(l, 0.0f, 16.0f);
	m = clamp(m, 0.0f, 16.0f);
	
    float3 downsample = 0.f;
    downsample += e * 0.125f;
    downsample += (a + c + g + i) * 0.03125f;
    downsample += (b + d + f + h) * 0.0625f;
    downsample += (j + k + l + m) * 0.125f;

    u_bloom[id.xy] = downsample;
}
