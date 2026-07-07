#ifndef SHARED_COMMON_H
#define SHARED_COMMON_H

cbuffer static_globals
{
    float3x4 m_W;
    float3x4 m_V;
    float3x4 m_invV;
    float4x4 m_P;
    float3x4 m_WV;
    float4x4 m_VP;
    float4x4 m_WVP;
    float4 timers;
    float4 fog_plane;
    float4 fog_params; // x=near*(1/(far-near)), ?,?, w = -1/(far-near)
    float4 fog_color;
    float4 L_sky_color;
    float3 L_sun_color;
    float3 L_sun_dir_w;
    float4 L_sun_dir_e;
    float4 L_hemi_color;
    float4 L_ambient; // L_ambient.w = skynbox-lerp-factor
    float3 eye_position;
    float3 eye_direction;
    float3 eye_normal;
    float4 dt_params;
}

float3 unpack_normal(float3 v)
{
    return 2 * v - 1;
}
float3 unpack_bx2(float3 v)
{
    return 2 * v - 1;
}
float3 unpack_bx4(float3 v)
{
    return 4 * v - 2;
}

float2 unpack_tc_base(float2 tc, float du, float dv)
{
    return (tc.xy + float2(du, dv)) * (32.f / 32768.f);
}

float2 unpack_tc_lmap(float2 tc)
{
    return tc * (1.f / 32768.f);
} // [-1  .. +1 ]

float calc_cyclic(float x)
{
    float phase = 1 / (2 * 3.141592653589f);
    float sqrt2 = 1.4142136f;
    float sqrt2m2 = 2.8284271f;
    float f = sqrt2m2 * frac(x) - sqrt2; // [-sqrt2 .. +sqrt2]
    return f * f - 1.f; // [-1     .. +1]
}
float2 calc_xz_wave(float2 dir2D, float frac)
{
    // Beizer
    float2 ctrl_A = float2(0.f, 0.f);
    float2 ctrl_B = float2(dir2D.x, dir2D.y);
    return lerp(ctrl_A, ctrl_B, frac);
}

#endif
