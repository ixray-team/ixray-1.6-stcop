#ifndef SHARED_COMMON_DECL_SM3_H
#define SHARED_COMMON_DECL_SM3_H

uniform float3x4 m_W;
uniform float3x4 m_V;
uniform float4x4 m_P;
uniform float3x4 m_WV;
uniform float4x4 m_VP;
uniform float4x4 m_WVP;
uniform float4 timers;
uniform float4 fog_plane;
uniform float4 fog_params;
uniform float4 fog_color;
uniform float3 L_sun_color;
uniform float3 L_sun_dir_w;
uniform float3 L_sun_dir_e;
uniform float4 L_hemi_color;
uniform float4 L_ambient;
uniform float3 eye_position;
uniform float3 eye_direction;
uniform float3 eye_normal;
uniform float4 dt_params;

#endif
