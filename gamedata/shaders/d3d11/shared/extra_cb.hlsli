#ifndef EXTRA_CB_HLSLI
#define EXTRA_CB_HLSLI

// Pass constants written by name through FixedConstants::OnSet (see write_extra).
// Each buffer keeps its own slot and stays bound.

cbuffer cb_ui_icon : register(b11)
{
    float4 ui_icon_light;
    float4 ui_icon_direction;
    float4 ui_emissive_tint;
};

cbuffer cb_visor : register(b12)
{
    float4 visor_rain;
    float4 mask_control;
    float4x4 m_tc_xform;
};

cbuffer cb_editor : register(b13)
{
    float4 editor_params;
    float4 editor_selection;
    float4 editor_instance_params;
    float4 editor_hiz_params;
};

#endif
