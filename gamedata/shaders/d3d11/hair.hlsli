#ifndef HAIR_H
#define HAIR_H
/*
    Simple hair vertex animation

    Author:
    - ForserX

    ---IX-Ray Engine---
*/

#include "common.hlsli"

float4 env_wind;
Texture2D s_hair;

void hair_wave_anim(float2 tc, float mask, inout float3 pos, inout float3 pos_old, float3 normal)
{
    float2 wind_dir = env_wind.xy;
    float wind_strength = env_wind.z * 0.5f + 0.15f;

    wind_dir = normalize(wind_dir);

    float time = timers.x;
    float phase = tc.y * 8.0f;

    // Волновая анимация
    float wave1 = sin(time * 2.0f + phase) * 0.15f;
    float wave2 = sin(time * 3.7f + phase * 1.3f) * 0.05f;
    float amplitude = (wave1 + wave2) * wind_strength * mask;

    float2 offset = wind_dir * amplitude;

    pos.xz += offset * 0.1f;

    float time_old = time - 0.016f;
    float wave1_old = sin(time_old * 2.0f + phase) * 0.15f;
    float wave2_old = sin(time_old * 3.7f + phase * 1.3f) * 0.05f;
    float amplitude_old = (wave1_old + wave2_old) * wind_strength * mask;
    float2 offset_old = wind_dir * amplitude_old;

    pos_old.xz += offset_old * 0.1f * 0.95f;
}

#endif