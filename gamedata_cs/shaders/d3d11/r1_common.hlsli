#ifndef R1_COMMON_HLSLI
#define R1_COMMON_HLSLI
#define COMMON_H

#include "r1_decl.hlsli"
#include "common_iostructs.hlsli"
#include "common_samplers.hlsli"

float3 unpack_normal(float3 v)
{
    return 2.0f * v.zyx - 1.0f;
}

float4 unpack_D3DCOLOR(float4 v)
{
    return v.bgra;
}

float2 unpack_tc_base(float2 tc, float du, float dv)
{
    return (tc + float2(du, dv)) * (32.0f / 32768.0f);
}

float2 unpack_tc_lmap(float2 tc)
{
    return tc * (1.0f / 32768.0f);
}

#endif
