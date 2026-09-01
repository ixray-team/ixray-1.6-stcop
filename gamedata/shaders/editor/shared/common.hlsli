#ifndef SHARED_COMMON_H
#define SHARED_COMMON_H

#include "common_decl.hlsli"

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
