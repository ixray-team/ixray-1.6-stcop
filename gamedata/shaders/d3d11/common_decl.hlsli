#ifndef D3D11_COMMON_DECL_H
#define D3D11_COMMON_DECL_H

cbuffer cb_frame : register(b0)
{
    float4 timers;
    float4 fog_plane;
    float4 fog_params;
    float4 fog_color;
    float4 L_sun_color;
    float4 L_sun_dir_w;
    float4 L_sun_dir_e;
    float4 L_hemi_color;
    float4 L_ambient;
    float4 L_sky_color;
    float4 env_wind;
    float4 water_intensity;
    float4 sun_shafts_intensity;
    float4 rain_params;
    float4 nvg_color;
    float4 m_hud_params;
    float4 m_zoom_deviation;
    float4 m_affects;
    float4 m_actor_params;
    float4 m_timearrow;
    float4 m_timearrow2;
    float4 test_exp_to_shaders_1;
    float4 test_exp_to_shaders_2;
    float4 color_params;
    float4 color_grading;
    float4 c_brightness;
    float4 c_colormap;
};


cbuffer cb_view : register(b1)
{
    float3x4 m_V;
    float4x4 m_P;
    float4x4 m_VP;
    float3x4 m_invV;
    float4x4 m_invP;
    float4x4 m_VP_old;
    float4x4 m_invVP_old;
    float3 eye_position;
    float3 eye_direction;
    float3 eye_normal;
    float4 m_taa_jitter;
    float4 screen_res;
    float4 scaled_screen_res;
    float4 pos_decompression_params2;
};

cbuffer cb_object : register(b2)
{
    float3x4 m_W;
    float3x4 m_invW;
    float3x4 m_WV;
    float4x4 m_WVP;
    float4x4 m_WVP_old;
};

cbuffer cb_pass : register(b5)
{
    float4x4 m_P_hud;
    float4x4 m_texgen;
    float4x4 m_xform;
    float4x4 m_xform_v;
    float4 consts;
    float4 wave;
    float4 wind;
    float4 consts_old;
    float4 wave_old;
    float4 wind_old;
    float4 c_scale;
    float4 c_bias;
    float4 c_sun;
    float4x4 m_invP_hud;
    float4 mblur_params;
};

cbuffer cb_material : register(b3)
{
    float4 L_material;
    float4 hemi_cube_pos_faces;
    float4 hemi_cube_neg_faces;
    float4 dt_params;
    float4 parallax;
    float  def_aref;
    float  m_AlphaRef;
    float2 _pad_material;
    float4 L_model_light_color;
    float4 L_model_light_dir;
    float4 triLOD;
    float4 m_lmap[2];
    float4 tfactor;
};

cbuffer cb_light : register(b4)
{
    float4 Ldynamic_color;
    float4 Ldynamic_pos;
    float4 Ldynamic_dir;
    int    Ldynamic_hud;
    float3 _pad_light;
    float4x4 m_shadow_sun[3];
    float4x4 m_shadow;
    float3x4 m_sunmask;
};

#endif
