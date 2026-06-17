/*
		Simple nightvision. Test

		Author:
		- LVutner

		---IX-Ray Engine---
*/

#include "common.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

float4 main(PSInput I) : SV_Target
{
	//consts
	const float neg_sqr_rad = -1.0 / (65. * 65.);
	const float3 nvg_col = float3(0.3,1.0,0.2);
	const float2 min_max_bright = float2(2, 4);

	//Sample blue noise texture
	//You can replace 0 with m_taa_jitter.w % 32 to animate it (texture contains 32 frames)
	float3 jitter_tex = s_blue_noise[uint3(uint2(I.hpos.xy * 0.5) % 128, uint(20*timers.x) % 32)].xyz;

	//Unpack G-Buffer data...
	float3 Point =  GbufferGetPointRealUnjitter(I.texcoord.xy, 1.0f - s_position.SampleLevel(smp_nofilter, I.texcoord.xy, 0.0f).x);

	//Fetch fulres scene
	float3 image = s_image[I.hpos.xy].xyz;
	float falloff = saturate(dot(Point, Point) * neg_sqr_rad + 1.0);
    image = nvg_col * dot(image, (0.33));
	float bmod = lerp(min_max_bright.x, min_max_bright.y, falloff);
	
    image *= bmod;
    image = image * jitter_tex.x + image;
    image += step(jitter_tex.y, 0.1) * 0.1;
	image *= bmod;
	image /= 1.0 + image;
	image.rgb = ((image.rgb - 0.5) * 1.4) + 0.5;
    image *= 1.0-smoothstep(0.2, 1.0, length(I.texcoord-0.5));	

	return float4(image, 1.0);
}