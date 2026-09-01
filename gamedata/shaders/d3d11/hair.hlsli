#ifndef HAIR_H
#define HAIR_H
/*
    Simple hair vertex animation

    Author:
    - ForserX

    ---IX-Ray Engine---
*/

#include "common.hlsli"

Texture2D s_hair;

void hair_wave_anim(float2 tc, float indoor_factor, inout float3 pos, inout float3 pos_old, float3 normal)
{
    float2 wind_dir = mul(m_invW, env_wind.xyz).xz;
    float wind_strength = env_wind.w * 0.5f + 0.15f;
	
    float4 mask = s_hair.SampleLevel(smp_rtlinear, tc, 0);
    float phase = 8.0f;

    // Волновая анимация
    float2 wave1 = sin(timers.xy * 2.0f + phase) * 0.15f;
    float2 wave2 = sin(timers.xy * 3.7f + phase * 1.3f) * 0.05f;
	
    float2 amplitude = (wave1 + wave2) * wind_strength * mask.x * indoor_factor;
	
    pos_old.xz += wind_dir * amplitude.y * 0.1f;
    pos.xz += wind_dir * amplitude.x * 0.1f;
}

#endif
