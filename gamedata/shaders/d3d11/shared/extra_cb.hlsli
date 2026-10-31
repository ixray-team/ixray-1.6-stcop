#ifndef EXTRA_CB_HLSLI
#define EXTRA_CB_HLSLI

#ifndef __cplusplus
#include "shared\cbuffer.hlsli"
#endif

CBUFFER(CBUIIcon, 10)
	float4 ui_icon_light;
	float4 ui_icon_direction;
	float4 ui_emissive_tint;
CBUFFER_END

CBUFFER(CBVisor, 11)
	float4 visor_rain;
	float4 mask_control;
	float4x4 m_tc_xform;
CBUFFER_END

CBUFFER(CBEditor, 12)
	float4 editor_params;
	float4 editor_selection;
	float4 editor_instance_params;
	float4 editor_hiz_params;
	float4 brush_worldpos;
	float4 brush_params;
	float4 brush_noise;
	float4 brush_color;
	float4 brush_extra;
CBUFFER_END

#endif
