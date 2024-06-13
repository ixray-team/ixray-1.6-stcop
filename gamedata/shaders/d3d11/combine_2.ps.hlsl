#include "common.hlsli"
#include "mblur.hlsli"
#include "dof.hlsli"

float3 deband_color (float3 image, float2 uv) {
	float3 dither = Hash23(cos(uv.xy * timers.x) * 1245.0f);
	
	float3 color = image * 255.0f;
	float3 pq = frac(color);
	
	color -= pq;
 	pq = step(dither, pq);
	
	color += pq;
	color /= 255.0f;
	
	return color;
}

float4 main( v2p_aa_AA I ) : SV_Target
{
	gbuffer_data gbd = gbuffer_load_data(I.Tex0, I.HPos);

	float3 img = dof(I.Tex0);
	float4 bloom = s_bloom.Sample(smp_rtlinear, I.Tex0);

	float scale = s_tonemap.Sample(smp_nofilter, float2(0.5f, 0.5f)).x;
	img = tonemap(img, scale);

 	float4 Color = combine_bloom(img, bloom);
	Color.xyz = deband_color(Color.xyz, I.Tex0);
	
	return Color;
}

