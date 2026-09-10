#ifndef COMMON_CLOUDS_HLSLI
#define COMMON_CLOUDS_HLSLI

#include "common_sky.hlsli"

Texture3D<float4> s_cloud_fbm_noise;
// xy: layer bottom/top (km), z: engine units to km, w: reserved.
uniform float4 cloud_layer_params;
// xy: internal scene resolution (before FSR/DLSS), z: four-frame trace phase.
uniform float4 cloud_screen_params;

uint2 cloud_trace_pixel(uint2 low_pixel, uint2 full_size)
{
    const uint2 phases[4] = { uint2(0, 0), uint2(1, 1), uint2(1, 0), uint2(0, 1) };
    return min(low_pixel * 2u + phases[uint(cloud_screen_params.z) & 3u], full_size - 1u);
}

float2 cloud_unjittered_uv(float2 uv)
{
    // Sky VS shifts clip XY by +m_taa_jitter.xy. Invert that shift exactly once.
    // AP and m_invP are unjittered; the local checkerboard offset is independent.
    return uv - m_taa_jitter.xy * float2(0.5f, -0.5f);
}

// One 2x2x4 sequence = 16 midpoint strata. Each 2x2 block of cells and each
// cell over four phases cover all four coarse quarters with mean offset 0.5.
float cloud_stratified_ray_jitter(uint2 cell, uint phase)
{
    uint2 tile = cell & 1u;
    uint lane = ((tile.x ^ tile.y) << 1u) | tile.y; // Bayer order: 0,2 / 3,1
    const uint4 ranks[4] = {
        uint4(0, 5, 10, 15), uint4(11, 14, 1, 4),
        uint4(6, 3, 12, 9), uint4(13, 8, 7, 2)
    };
    return (float(ranks[phase & 3u][lane]) + 0.5f) * (1.0f / 16.0f);
}

static const float CLOUD_SHAPE_FREQUENCY = 64.0f * cloud_layer_params.z;
static const float CLOUD_EROSION_FREQUENCY = 640.0f * cloud_layer_params.z;
static const float CLOUD_EROSION_STRENGTH = 0.74f;
static const float CLOUD_EXTINCTION_KM_INV = 8.0f;

float3 cloud_planet_camera()
{
    return float3(0.0f, SKY_EARTH_RADIUS + max(eye_position.y * cloud_layer_params.z, 0.0f), 0.0f);
}

float cloud_relative_height(float3 planet_position)
{
    return (length(planet_position) - (SKY_EARTH_RADIUS + cloud_layer_params.x))
        / (cloud_layer_params.y - cloud_layer_params.x);
}

float cloud_vertical_profile(float h)
{
    return smoothstep(0.0f, 0.055f, h) * (1.0f - smoothstep(0.1f, 0.3f, h));
}

static const float2 cloud_window = float2(0.69, 0.98);

// The same density is used by view rays and shadow rays.
float cloud_density(float3 world_position, float h, float profile, float3 erosion_uv_offset, out float cloud_coverage)
{
    cloud_coverage = 0.0f;
    if (h <= 0.0f || h >= 1.0f)
        return 0.0f;

    float4 low_freq = s_cloud_fbm_noise.SampleLevel(smp_linear, world_position * CLOUD_SHAPE_FREQUENCY, 0.0f);
    float low_freq_fbm = dot(low_freq.gba, float3(0.625, 0.25, 0.125));
    float shape = Remap(low_freq.r, low_freq_fbm - 1.0f, 1.0, 0.0, 1.0);
    float cover = smoothstep(cloud_window.x, cloud_window.y, shape);
    cloud_coverage = cover;
    float density = saturate(profile + cover - 1.0f);
    if (density <= 0.0f)
        return 0.0f;

    float4 erosion = s_cloud_fbm_noise.SampleLevel(smp_linear, world_position * CLOUD_EROSION_FREQUENCY + erosion_uv_offset, 0.0f);
    float high_freq_fbm = dot(erosion.gba, float3(0.625, 0.25, 0.125));
    erosion.r = lerp(erosion.r, high_freq_fbm, cloud_coverage);
    float threshold = saturate(erosion) * CLOUD_EROSION_STRENGTH;
    return saturate((density - threshold) / (1.0f - threshold));
}

float cloud_density_cheap(float3 world_position, float h, float profile, out float cloud_coverage)
{
    cloud_coverage = 0.0f;
    if (h <= 0.0f || h >= 1.0f)
        return 0.0f;

    float4 low_freq = s_cloud_fbm_noise.SampleLevel(smp_linear, world_position * CLOUD_SHAPE_FREQUENCY, 0.0f);
    float low_freq_fbm = dot(low_freq.gba, float3(0.625, 0.25, 0.125));
    float shape = Remap(low_freq.r, low_freq_fbm - 1.0f, 1.0, 0.0, 1.0);
    float cover = smoothstep(cloud_window.x, cloud_window.y, shape);
    cloud_coverage = cover;
    float density = saturate(profile + cover - 1.0f); //profile * cover;
    return saturate(density);
}

#endif
