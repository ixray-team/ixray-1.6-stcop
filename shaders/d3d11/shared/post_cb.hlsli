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
	float4 dof_params; // x - focal length (mm), y - f-number, z - sensor height (m), w - output height
	float4 dof_combine_params; // xy - output size, z - output pixels per blur pixel, w - maximum radius
	float4 dof_focus_params; // x - frame blend, y - manual mode, z - manual/fallback distance (m), w - minimum focus (m)
	float4 dof_focus_point; // xy - target UV, z - horizontal blur scale (1 / anamorphic squeeze), w - most fetches per blur tap
	float4 dof_classic; // xyz - near, focus, far (m), w - sky distance (m)
	float4 dof_rt_size; // xy - reduced size, zw - reduced texel size
	float4 dof_blur_params; // xy - output texel size, z - output pixels per blur pixel, w - maximum output radius
	float4 dof_downsample_params; // xy - source size, zw - reduced size
	float4 dof_tile_params; // xy - tile count, zw - inverse tile count (8x8 reduced texels per tile)
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
	float4 PostRect;
	float4 PostUV[3];
	float4 PostColor[2];
	float gtao_parameters;
	float puddle_constants;
	float sharpening_intensity;
	float NightVisionInterference;
	float3 dof_kernel; // x - classic radius (output pixels), y - swirl (0-1), z - radius limit
CBUFFER_END

#endif
