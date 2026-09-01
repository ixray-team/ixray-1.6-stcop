#include "common.hlsli"

Texture2D s_droplets;

float4 main(PSInputFullscreen I) : SV_Target
{
	float4 rain_drops_distortion = s_droplets.Sample(smp_rtlinear, I.texcoord);
	float2 texcoord_offset = (rain_drops_distortion.xy - (127.0f / 255.0f)) * def_distort;
	float2 texcoord = I.texcoord - texcoord_offset * saturate(rain_params.y);
	float3 scene = s_image.Sample(smp_rtlinear, texcoord).xyz;

	return float4(scene, 1.0f);
}