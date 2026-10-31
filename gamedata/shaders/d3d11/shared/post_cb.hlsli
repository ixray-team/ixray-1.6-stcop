#ifndef POST_CB_HLSLI
#define POST_CB_HLSLI

#ifndef __cplusplus
#include "shared\cbuffer.hlsli"
#endif

CBUFFER(CBPost, 7)
	float4 downsample_params;
	float4 upsample_params;
	float4 adapt_params;
	float4 adapt_params2;
	float4 MiddleGray;
	float4 autoexposure_params;
	float4 bloom_params;
	float4 tonemap_params;
	float4 bloom_tint;
	float4 spp_params;
	float4 dof_params;
	float4 static_color;
	float4 ui_has_background;
	float4 RainDensity;
	float4 RainFeatures;
	float4 RainFallof;
	float4 WorldX;
	float4 WorldZ;
	float4 m_level_scale;
	float4 RainShape; // Half width, half length, radius multiplier, feather in drop-radius units.
	float4 RainColor; // Weather tint and density opacity multiplied by opacity gain.
	float4 RainShading; // PBR, weather color blend, transmission, refraction in output pixels.
	float4 HiZSizes; // xy = source size, zw = destination size
	float4 NightVisionPostprocess;
	float4 rain_layers_params;
	float4 rain_layers_direction;
	float4 shadow_cache_destination;
	float gtao_parameters;
	float puddle_constants;
	float sharpening_intensity;
	float NightVisionInterference;
	float3 dof_kernel;
CBUFFER_END

#endif
