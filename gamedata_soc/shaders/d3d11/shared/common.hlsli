//////////////////////////////////////////////////
//  All comments by Nivenhbro are preceded by !
/////////////////////////////////////////////////

#ifndef SHARED_COMMON_H
#define SHARED_COMMON_H

//	Used by VS
cbuffer dynamic_transforms
{
    uniform float4x4 m_WVP;
    uniform float3x4 m_WV;
    uniform float3x4 m_W;

    uniform float4x4 m_WVP_old;
    uniform float3x4 m_WV_old;
    uniform float3x4 m_W_old;

    uniform float4x4 m_P_hud;

    uniform float4 L_material;
    uniform float4 hemi_cube_pos_faces;
    uniform float4 hemi_cube_neg_faces;
    uniform float4 dt_params;
}

cbuffer shader_params
{
    float m_AlphaRef;
}

cbuffer static_globals
{
    uniform float3x4 m_invV;

    uniform float3x4 m_V;
    uniform float4x4 m_P;
    uniform float4x4 m_VP;

    uniform float3x4 m_V_old;
    uniform float4x4 m_P_old;
    uniform float4x4 m_VP_old;

    uniform float4 timers;

    uniform float4 fog_plane;
    uniform float4 fog_params;
    uniform float4 fog_color;

    uniform float4 L_ambient;
    uniform float3 L_sun_color;
    uniform float3 L_sun_dir_w;
    uniform float4 L_sky_color;
    uniform float4 L_hemi_color;

    uniform float3 eye_position;

    uniform float4 pos_decompression_params;
    uniform float4 pos_decompression_params2;
    uniform float4 pos_decompression_params_hud;
    uniform float4 depth_unpack;
    uniform float def_aref;
    uniform float4 parallax;

    uniform float4 m_taa_jitter;
}

float calc_cyclic(float x)
{
    float f = 1.4142f * sin(x * 3.14159f);
    return f * f - 1.0f;
}

float2 calc_xz_wave(float2 dir2D, float frac)
{
    return dir2D * frac;
}
#endif

