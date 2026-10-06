#pragma once
#ifndef SHARED_COMMON_MATH_H
#define SHARED_COMMON_MATH_H
#ifndef SM_5
float3 unpack_normal(float3 v) { return 2.0f * v - 1.0f; }
float3 unpack_bx2(float3 v) { return 2.0f * v - 1.0f; }
float3 unpack_bx4(float3 v) { return 4.0f * v - 2.0f; }
float2 unpack_tc_base(float2 tc, float du, float dv) { return (tc.xy + float2(du, dv)) * (32.f / 32768.f); }
float2 unpack_tc_lmap(float2 tc) { return tc * (1.f / 32768.f); }
#endif
float calc_cyclic(float x) { float f = 1.4142f * sin(x * 3.14159f); return f * f - 1.0f; }
float2 calc_xz_wave(float2 dir2D, float frac) { return dir2D * frac; }
#endif
