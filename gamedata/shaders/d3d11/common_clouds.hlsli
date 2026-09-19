#ifndef COMMON_CLOUDS_HLSLI
#define COMMON_CLOUDS_HLSLI

#include "common_sky.hlsli"

Texture3D<float4> s_cloud_fbm_noise : register(t0);
// xy: layer bottom/top (km), z: engine units to km, w: reserved.
uniform float4 cloud_layer_params;
// xy: unjittered internal scene resolution, z: trace phase, w: block size (2 or 4).
uniform float4 cloud_screen_params;

uint2 cloud_trace_offset()
{
    const uint2 phases2[4] = { uint2(0, 0), uint2(1, 1), uint2(1, 0), uint2(0, 1) };
    const uint2 phases4[16] = {
        uint2(0, 0), uint2(2, 2), uint2(2, 0), uint2(0, 2),
        uint2(1, 1), uint2(3, 3), uint2(3, 1), uint2(1, 3),
        uint2(1, 0), uint2(3, 2), uint2(3, 0), uint2(1, 2),
        uint2(0, 1), uint2(2, 3), uint2(2, 1), uint2(0, 3)
    };
    return uint(cloud_screen_params.w) == 4u
        ? phases4[uint(cloud_screen_params.z) & 15u]
        : phases2[uint(cloud_screen_params.z) & 3u];
}

uint2 cloud_trace_pixel(uint2 low_pixel)
{
    // Edge blocks trace a small guard band when the viewport is not divisible by N.
    return low_pixel * uint(cloud_screen_params.w) + cloud_trace_offset();
}

static const float CLOUD_SHAPE_FREQUENCY = 64.0f * cloud_layer_params.z;
static const float CLOUD_EROSION_FREQUENCY = 640.0f * cloud_layer_params.z;
static const float CLOUD_EROSION_STRENGTH = 0.65f;
static const float CLOUD_EXTINCTION_KM_INV = 8.0f;
static const uint3 CLOUD_FBM_NOISE_SIZE = uint3(128u, 128u, 128u);

// Nearest-texel lookup for periodic normalized 3D noise coordinates.
// texel_offset is deliberately expressed in texels, so changing a noise texture's
// resolution does not change the stochastic sampling radius.
float4 sample_3d_noise(Texture3D<float4> noise_texture, float3 uvw, uint3 texture_size, float3 texel_offset)
{
    const float3 texel_position = frac(uvw) * float3(texture_size) + texel_offset;
    int3 texel = int3(floor(texel_position));
    // The current offset is bounded to +/-0.5 texel, so a coordinate can cross
    // at most one boundary. These component-wise corrections avoid integer modulo.
    texel = texel < 0 ? texel + int3(texture_size) : texel;
    texel = texel >= int3(texture_size) ? texel - int3(texture_size) : texel;
    return noise_texture.Load(int4(texel, 0));
}

float4 sample_3d_noise(Texture3D<float4> noise_texture, float3 uvw, uint3 texture_size)
{
    return sample_3d_noise(noise_texture, uvw, texture_size, float3(0.0f, 0.0f, 0.0f));
}

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
    return smoothstep(0.0f, 0.055f, h) * (1.0f - smoothstep(0.1f, 0.9f, h));
}

static const float2 cloud_window = float2(0.75, 0.98);

// The same density is used by view rays and shadow rays.
float cloud_density(
    float3 world_position,
    float h,
    float profile,
    float3 noise_texel_offset,
    out float cloud_coverage)
{
    cloud_coverage = 0.0f;
    if (h <= 0.0f || h >= 1.0f)
        return 0.0f;

    float4 low_freq = sample_3d_noise(
        s_cloud_fbm_noise, world_position * CLOUD_SHAPE_FREQUENCY,
        CLOUD_FBM_NOISE_SIZE, noise_texel_offset);
    float low_freq_fbm = dot(low_freq.gba, float3(0.625, 0.25, 0.125));
    float shape = Remap(low_freq.r, low_freq_fbm - 1.0f, 1.0, 0.0, 1.0);
    float cover = smoothstep(cloud_window.x, cloud_window.y, shape);
    cloud_coverage = cover;
    float density = saturate(profile + cover - 1.0f);
    if (density <= 0.0f)
        return 0.0f;

    float4 erosion = sample_3d_noise(
        s_cloud_fbm_noise, world_position * CLOUD_EROSION_FREQUENCY,
        CLOUD_FBM_NOISE_SIZE, noise_texel_offset);
    float high_freq_fbm = dot(erosion.gba, float3(0.625, 0.25, 0.125));
    erosion.r = lerp(erosion.r, high_freq_fbm, cloud_coverage);
    float threshold = saturate(erosion.r) * CLOUD_EROSION_STRENGTH;
    return saturate((density - threshold) / (1.0f - threshold));
}

float cloud_density(float3 world_position, float h, float profile, out float cloud_coverage)
{
    return cloud_density(world_position, h, profile, float3(0.0f, 0.0f, 0.0f), cloud_coverage);
}

float cloud_density_cheap(
    float3 world_position,
    float h,
    float profile,
    float3 noise_texel_offset,
    out float cloud_coverage)
{
    cloud_coverage = 0.0f;
    if (h <= 0.0f || h >= 1.0f)
        return 0.0f;

    float4 low_freq = sample_3d_noise(
        s_cloud_fbm_noise, world_position * CLOUD_SHAPE_FREQUENCY,
        CLOUD_FBM_NOISE_SIZE, noise_texel_offset);
    float low_freq_fbm = dot(low_freq.gba, float3(0.625, 0.25, 0.125));
    float shape = Remap(low_freq.r, low_freq_fbm - 1.0f, 1.0, 0.0, 1.0);
    float cover = smoothstep(cloud_window.x, cloud_window.y, shape);
    cloud_coverage = cover;
    float density = saturate(profile + cover - 1.0f); //profile * cover;
    return saturate(density);
}

float cloud_density_cheap(float3 world_position, float h, float profile, out float cloud_coverage)
{
    return cloud_density_cheap(
        world_position, h, profile, float3(0.0f, 0.0f, 0.0f), cloud_coverage);
}

#endif
