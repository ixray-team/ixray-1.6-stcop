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

void main(in PSInputFullscreen I, out float3 downsample : SV_Target)
{
    float2 center = I.texcoord.xy;
	
    float x = 0.5f * downsample_params.z;
    float y = 0.5f * downsample_params.w;
    
    float3 a = s_image.Sample(smp_rtlinear, float2(center.x - 2.f * x, center.y + 2.f * y)).rgb;
    float3 b = s_image.Sample(smp_rtlinear, float2(center.x, center.y + 2.f * y)).rgb;
    float3 c = s_image.Sample(smp_rtlinear, float2(center.x + 2.f * x, center.y + 2.f * y)).rgb;

    float3 d = s_image.Sample(smp_rtlinear, float2(center.x - 2.f * x, center.y)).rgb;
    float3 e = s_image.Sample(smp_rtlinear, float2(center.x, center.y)).rgb;
    float3 f = s_image.Sample(smp_rtlinear, float2(center.x + 2.f * x, center.y)).rgb;

    float3 g = s_image.Sample(smp_rtlinear, float2(center.x - 2.f * x, center.y - 2.f * y)).rgb;
    float3 h = s_image.Sample(smp_rtlinear, float2(center.x, center.y - 2.f * y)).rgb;
    float3 i = s_image.Sample(smp_rtlinear, float2(center.x + 2.f * x, center.y - 2.f * y)).rgb;

    float3 j = s_image.Sample(smp_rtlinear, float2(center.x - x, center.y + y)).rgb;
    float3 k = s_image.Sample(smp_rtlinear, float2(center.x + x, center.y + y)).rgb;
    float3 l = s_image.Sample(smp_rtlinear, float2(center.x - x, center.y - y)).rgb;
    float3 m = s_image.Sample(smp_rtlinear, float2(center.x + x, center.y - y)).rgb;
    
#ifdef USE_LEGACY_LIGHT
	#ifdef USE_GAMMA_FIX
		a *= a;
		b *= b;
		c *= c;
		d *= d;
		e *= e;
		f *= f;
		g *= g;
		h *= h;
		i *= i;
		j *= j;
		k *= k;
		l *= l;
		m *= m;
	#endif
#endif
	
	a = min(a, 64.0f);
	b = min(b, 64.0f);
	c = min(c, 64.0f);
	d = min(d, 64.0f);
	e = min(e, 64.0f);
	f = min(f, 64.0f);
	g = min(g, 64.0f);
	h = min(h, 64.0f);
	i = min(i, 64.0f);
	j = min(j, 64.0f);
	k = min(k, 64.0f);
	l = min(l, 64.0f);
	m = min(m, 64.0f);
	
    downsample = e * 0.125f;
	
    downsample += (a + c + g + i) * 0.03125f;
    downsample += (b + d + f + h) * 0.0625f;
    downsample += (j + k + l + m) * 0.125f;
}

