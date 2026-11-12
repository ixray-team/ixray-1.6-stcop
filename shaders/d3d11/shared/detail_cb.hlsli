#ifndef DETAIL_CB_HLSLI
#define DETAIL_CB_HLSLI

#ifndef __cplusplus
#include "shared\cbuffer.hlsli"
#endif

CBUFFER(CBDetail, 8)
	float4 trample_params;
	float4 wind_global;
	float4 wind_xz1;
	float4 wind_xz1_dir;
	float4 wind_xz2;
	float4 wind_xz2_dir;
	float4 wind_xz3;
	float4 wind_xz3_dir;
	float4 wind_swirl;
	float4 wind_swirl_dir;
	uint detail_first; // of the model's entries in detail_visible
	uint detail_reserved0;
	uint detail_reserved1;
	uint detail_reserved2;
CBUFFER_END

#endif
