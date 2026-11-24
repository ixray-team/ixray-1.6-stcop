/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#ifndef WETNESS_HLSLI
#define WETNESS_HLSLI

// Rain on a surface, as a part of its material: what the rain reaches gets darker and smoother before anything
// lights it. The includer declares s_smap_sun and smp_smap: the rain's map is the slice after the sun's cascades,
// what the rain reaches seen from above. rain_map takes a point of the world into it, rain_setup holds where the
// wetness fades out with distance and how much of the slice the map takes
#define RAIN_MAP_SLICE 3.0f

// Water mirrors 2% of what meets it head-on (its index is 1.33). The metal-roughness model keeps a reflectance
// as a specular value s with F0 = 0.16 * s * s
#define WATER_F0 0.02f
#define WATER_SPECULAR 0.35355f
#ifndef RAIN_TAPS
#define RAIN_TAPS 16
#endif

float rain_hash(float2 cell)
{
    return frac(sin(dot(cell, float2(127.1f, 311.7f))) * 43758.5453f);
}

float rain_noise(float2 at)
{
    float2 cell = floor(at);
    float2 part = frac(at);
    part = part * part * (3.0f - 2.0f * part);
    return lerp(lerp(rain_hash(cell), rain_hash(cell + float2(1.0f, 0.0f)), part.x), lerp(rain_hash(cell + float2(0.0f, 1.0f)), rain_hash(cell + 1.0f), part.x), part.y);
}

// returns how wet the rain makes a point: 0 under a roof, far away, on what faces down and when it does not rain.
// hemi is how much of the sky the point has, view_depth its distance along the view, pixel its place on the screen.
// reached_part is how much of the rain reaches the point, whatever the distance: what water that stays lies by
float rain_exposure(float3 world_pos, float3 world_normal, float hemi, float view_depth, float2 pixel, out float reached_part)
{
    float density = saturate(rain_params.x);
    reached_part = 0.0f;

    if (rain_setup.w <= 0.0f || density <= 0.0f) {
        return 0.0f;
    }

    float3 from = world_pos + world_normal * 0.2f;
    float3 coord = float3(dot(rain_map[0], float4(from, 1.0f)), dot(rain_map[1], float4(from, 1.0f)), dot(rain_map[2], float4(from, 1.0f)));

    // A disc of two metres around the point, turned by the pixel. Each tap is compared against the surface's own
    // plane, or a slope would shelter itself
    const float radius = 2.0f;
    float2 uv_x = float2(rain_map[0].x, rain_map[1].x) * radius;
    float2 uv_z = float2(rain_map[0].z, rain_map[1].z) * radius;
    float depth_up = rain_map[2].y * radius;
    float2 slope = world_normal.xz / max(world_normal.y, 0.5f);
    // The turn changes every frame: the frames average what one pixel's taps leave uneven
    float2 moved = pixel + 5.588238f * float(uint(m_taa_jitter.w) % 64u);
    float turn = frac(52.9829189f * frac(dot(moved, float2(0.06711056f, 0.00583715f)))) * 6.2831853f;
    float reached = 0.0f;

    [unroll]
    for (int tap_idx = 0; tap_idx < RAIN_TAPS; ++tap_idx) {
        float2 offset;
        sincos(tap_idx * 2.39996f + turn, offset.y, offset.x);
        offset *= sqrt((tap_idx + 0.5f) / RAIN_TAPS);
        reached += s_smap_sun.SampleCmpLevelZero(smp_smap, float3(coord.xy + uv_x * offset.x + uv_z * offset.y, RAIN_MAP_SLICE), coord.z - depth_up * dot(slope, offset) - 1e-5f);
    }

    // Where the map ends the sky a point has stands in for it: over the outer third of a circle inside the map, or
    // the map's square showed as a straight line across the ground
    float inside = saturate((1.0f - length(coord.xy / rain_setup.z * 2.0f - 1.0f)) * 3.0f);
    float wet = lerp(smoothstep(0.1f, 0.6f, hemi), reached / RAIN_TAPS * smoothstep(0.0f, 0.5f, hemi), inside);

    // The border of a wet patch follows a noise, not the disc
    float2 edge_at = world_pos.xz + world_pos.y * 0.7f;
    float edge = (rain_noise(edge_at * 0.9f) * 0.75f + rain_noise(edge_at * 3.3f) * 0.25f) * 0.2f;
    wet = smoothstep(edge, edge + 0.8f, wet);
    reached_part = wet;

    float far = 1.0f - smoothstep(min(rain_setup.y - 15.0f, rain_setup.x), rain_setup.y, view_depth);
    return wet * far * far * saturate(world_normal.y * 10.0f + 5.5f) * density;
}

#ifdef RAIN_RIPPLES
Texture3D s_rain_splash : register(t54);
Texture2D s_rain_streaks : register(t55);

float3 rain_streak(float2 uv)
{
    float3 streak = (s_rain_streaks.SampleLevel(smp_linear, uv, 0.0f).xzy - 0.5f) * 0.6f;
    streak.y = 0.0f;
    return streak;
}

// returns what falling rain adds to the normal of a wet surface, in the world: the rings of drops on what faces up
// and the streaks that run down what stands. rain_params.z is how far the streaks have run
float3 rain_ripples(float3 world_pos, float3 world_normal)
{
    float3 splash = (s_rain_splash.SampleLevel(smp_linear, float3(world_pos.xz, timers.x * 3.0f), 0.0f).wyz - 0.5f) * 6.0f;
    splash.y = 0.0f;

    float3 at = world_pos * 0.5f;
    float3 along_x = rain_streak(float2(at.z, at.y + rain_params.z));
    float3 along_z = rain_streak(float2(at.x, at.y + rain_params.z));
    return splash * max(world_normal.y, 0.0f) + along_x.yxz * abs(world_normal.x) + along_z.zxy * abs(world_normal.z);
}
#endif

// A wet surface of the metal-roughness model: darker unless it is a metal, smooth, with the sheen of water
void wet_surface(inout float3 color, inout float roughness, inout float specular, float metalness, float wet)
{
    color *= lerp(1.0f, lerp(0.66f, 1.0f, metalness), wet);
    roughness = lerp(roughness, min(0.2f, roughness), wet);
    specular = lerp(specular, 0.5f * max(0.5f * saturate(dot(color, 0.333f)), specular), wet);
}

#endif
