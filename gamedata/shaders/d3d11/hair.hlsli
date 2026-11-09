#ifndef HAIR_H
#define HAIR_H
/*
    Hair vertex animation: wind sway, inertia of the head and body collision

    Author:
    - ForserX

    ---IX-Ray Engine---
*/

#include "common.hlsli"

Texture2D s_hair;

#if defined(SKIN_0) || defined(SKIN_1) || defined(SKIN_2) || defined(SKIN_3) || defined(SKIN_4)
#define HAIR_DYNAMICS

#define HAIR_SKIN_NEAR 0.09f
#define HAIR_SKIN_FAR 0.15f

#define HAIR_SCALP_LOW 0.0f
#define HAIR_SCALP_HIGH 0.05f

#define HAIR_BOXES 8

cbuffer HairConstants
{
    float4 hair_lag[3];
    float4 hair_lag_old[3];
    float4 hair_params;
    float4 hair_head_bone;
    float4 hair_head_low;
    float4 hair_head_high;
    float4 hair_boxes[HAIR_BOXES * 4];
}

float3 hair_closest_on_segment(float3 p, float3 a, float3 b)
{
    float3 ab = b - a;
    float t = saturate(dot(p - a, ab) / max(dot(ab, ab), 1e-6f));
    return a + ab * t;
}

float3 hair_push_out(float3 p, float3 rest, float3 a, float3 b, float limit)
{
    float3 closest = hair_closest_on_segment(p, a, b);
    float3 dir = p - closest;
    float dist = length(dir);

    if (dist >= limit)
    {
        return p;
    }

    float3 rest_dir = rest - hair_closest_on_segment(rest, a, b);
    float3 push_dir = dist > 1e-5f ? dir / dist : rest_dir / max(length(rest_dir), 1e-5f);
    return closest + push_dir * limit;
}

float hair_box_depth(float3 local, float3 half_size, out int axis)
{
    float3 gap = half_size - abs(local);
    axis = gap.x < gap.y ? (gap.x < gap.z ? 0 : 2) : (gap.y < gap.z ? 1 : 2);
    return gap[axis];
}

float3 hair_push_out_box(float3 p, float3 rest, int index)
{
    float4 half_size = hair_boxes[index * 4 + 3];
    if (half_size.w <= 0.0f)
    {
        return p;
    }

    float3x4 to_box = float3x4(hair_boxes[index * 4 + 0], hair_boxes[index * 4 + 1], hair_boxes[index * 4 + 2]);
    float3 local = mul(to_box, float4(p, 1.0f));

    int axis;
    float depth = hair_box_depth(local, half_size.xyz, axis);

    int rest_axis;
    float allowed = max(0.0f, hair_box_depth(mul(to_box, float4(rest, 1.0f)), half_size.xyz, rest_axis));

    if (depth <= allowed)
    {
        return p;
    }

    float side = local[axis] < 0.0f ? -1.0f : 1.0f;
    return p + to_box[axis].xyz * (side * (depth - allowed));
}

float3 hair_collide(float3 p, float3 rest)
{
    if (hair_head_bone.w > 0.0f)
    {
        float axis_rest = length(rest - hair_closest_on_segment(rest, hair_head_low.xyz, hair_head_high.xyz));
        p = hair_push_out(p, rest, hair_head_low.xyz, hair_head_high.xyz, axis_rest);
    }

    [unroll]
    for (int i = 0; i < HAIR_BOXES; ++i)
    {
        p = hair_push_out_box(p, rest, i);
    }

    return p;
}

float hair_scalp_freedom(float3 rest)
{
    if (hair_head_bone.w <= 0.0f)
    {
        return 1.0f;
    }

    float3 up = normalize(hair_head_high.xyz - hair_head_low.xyz);
    float scalp = smoothstep(HAIR_SCALP_LOW, HAIR_SCALP_HIGH, dot(rest - hair_head_bone.xyz, up));

    float axis_dist = length(rest - hair_closest_on_segment(rest, hair_head_low.xyz, hair_head_high.xyz));
    float free = smoothstep(HAIR_SKIN_NEAR, HAIR_SKIN_FAR, axis_dist);

    return lerp(1.0f, free, scalp);
}
#endif

float3 hair_wind_offset(float time, float mask, float indoor_factor)
{
    float2 wind_dir = mul((float3x3)m_invW, env_wind.xyz).xz;
    float wind_strength = env_wind.w * 0.5f + 0.15f;
    float phase = 8.0f;

    float wave1 = sin(time * 2.0f + phase) * 0.15f;
    float wave2 = sin(time * 3.7f + phase * 1.3f) * 0.05f;

    float amplitude = (wave1 + wave2) * wind_strength * mask * indoor_factor * 0.1f;
    return float3(wind_dir.x, 0.0f, wind_dir.y) * amplitude;
}

float3 hair_move(float3 rest, float time, float mask, float indoor_factor, float3 normal, float4 lag[3])
{
    float3 offset = hair_wind_offset(time, mask, indoor_factor);

#ifdef HAIR_DYNAMICS
    bool dynamics = hair_params.w > 0.0f;
    if (dynamics)
    {
        float3 lagged = mul(float3x4(lag[0], lag[1], lag[2]), float4(rest, 1.0f));
        offset += (lagged - rest) * mask * hair_params.x;
        offset *= hair_scalp_freedom(rest);
    }
#endif

    float3 n = normalize(normal);
    offset -= n * min(0.0f, dot(offset, n));

    float3 pos = rest + offset;

#ifdef HAIR_DYNAMICS
    if (dynamics)
    {
        pos = hair_collide(pos, rest);
    }
#endif

    return pos;
}

#ifndef HAIR_DYNAMICS

static const float4 hair_lag[3] = {float4(1, 0, 0, 0), float4(0, 1, 0, 0), float4(0, 0, 1, 0)};
#define hair_lag_old hair_lag
#endif

#ifndef DISABLE_MOTION_VECTORS
void hair_wave_anim(float2 tc, float indoor_factor, inout float3 pos, inout float3 pos_old, float3 normal)
{
    float mask = s_hair.SampleLevel(smp_rtlinear, tc, 0).x;

    pos = hair_move(pos, timers.x, mask, indoor_factor, normal, hair_lag);
    pos_old = hair_move(pos_old, timers.y, mask, indoor_factor, normal, hair_lag_old);
}
#else
void hair_wave_anim(float2 tc, float indoor_factor, inout float3 pos, float3 normal)
{
    float mask = s_hair.SampleLevel(smp_rtlinear, tc, 0).x;

    pos = hair_move(pos, timers.x, mask, indoor_factor, normal, hair_lag);
}
#endif

#endif
