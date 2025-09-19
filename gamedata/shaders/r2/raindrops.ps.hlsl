#include "common.hlsli"

sampler2D s_droplets;
uniform float4 rain_params;

float4 main(float2 tc0 : TEXCOORD0) : COLOR
{
	float4	rain_drops_distortion = tex2D(s_droplets, tc0);
	float2	texcoord_offset = (rain_drops_distortion.xy - (127.0f / 255.0f)) * def_distort;
	float2	texcoord = tc0 + texcoord_offset * saturate(rain_params.y);
	float3 	scene = tex2D(s_image, texcoord).xyz;

	return float4(scene, 1.0f);
}