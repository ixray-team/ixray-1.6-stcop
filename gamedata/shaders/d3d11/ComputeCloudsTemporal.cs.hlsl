#include "common_clouds.hlsli"

// Bound through the selected temporal pass.T; t0..t7 belong to the raymarch.
Texture2D<float4> s_cloud_current : register(t8);
Texture2D<float4> s_cloud_history : register(t9);
RWTexture2D<float4> u_cloud_history : register(u0);

uniform float4x4 cloud_previous_view_projection; // unjittered, engine units
// x: valid history; y: unchanged view (exact Load avoids resampling drift).
uniform float4 cloud_temporal_params;

float4 cloud_spatial_fallback(uint2 pixel)
{
    uint width, height;
    s_cloud_current.GetDimensions(width, height);
    float2 low_position = (float2(pixel) - float2(cloud_trace_offset())) / cloud_screen_params.w;
    float2 uv = (low_position + 0.5f) / float2(width, height);
    return s_cloud_current.SampleLevel(smp_rtlinear, uv, 0.0f);
}

bool cloud_reproject(uint2 pixel, out float2 previous_uv)
{
    previous_uv = 0.0f;
    float2 uv = (float2(pixel) + 0.5f) / cloud_screen_params.xy;
    float3 direction = sky_world_ray_direction_from_screen_uv(uv);
    float3 origin = cloud_planet_camera();
    float radius = SKY_EARTH_RADIUS + 0.5f * (cloud_layer_params.x + cloud_layer_params.y);
    float distance = sky_ray_sphere_intersection(origin, direction, radius);
    float ground = sky_ray_sphere_intersection(origin, direction, SKY_EARTH_RADIUS);
    if (distance <= 0.0f || (ground >= 0.0f && ground < distance))
        return false;

    // A representative cloud-layer surface approximates translation parallax
    // without cloud depth textures or dependencies on scene geometry.
    float3 world = eye_position + direction * (distance / cloud_layer_params.z);
    float4 clip = mul(cloud_previous_view_projection, float4(world, 1.0f));
    if (!all(isfinite(clip)) || clip.w <= 1e-5f)
        return false;
    previous_uv = clip.xy / clip.w * float2(0.5f, -0.5f) + 0.5f;
    float2 border = 0.5f / cloud_screen_params.xy;
    return all(previous_uv >= border) && all(previous_uv <= 1.0f - border);
}

[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint2 pixel = dispatch_id.xy;
    uint2 size = uint2(cloud_screen_params.xy);
    if (any(pixel >= size))
        return;

    uint block_size = uint(cloud_screen_params.w);
    bool sampled_now = all((pixel % block_size) == cloud_trace_offset());
    float4 result;
    if (sampled_now)
    {
        // Empty measurements are authoritative too; alpha is transmittance,
        // never a marker indicating whether a pixel was measured.
        result = s_cloud_current.Load(int3(pixel / block_size, 0));
    }
    else if (cloud_temporal_params.x > 0.5f && cloud_temporal_params.y > 0.5f)
    {
        result = s_cloud_history.Load(int3(pixel, 0));
    }
    else
    {
        float2 previous_uv;
        if (cloud_temporal_params.x > 0.5f && cloud_reproject(pixel, previous_uv))
            result = s_cloud_history.SampleLevel(smp_rtlinear, previous_uv, 0.0f);
        else
            result = cloud_spatial_fallback(pixel);
    }

    if (!all(isfinite(result)))
        result = float4(0.0f, 0.0f, 0.0f, 1.0f);
    // Identical reconstruction weights for premultiplied radiance/transmittance.
    u_cloud_history[pixel] = float4(max(result.rgb, 0.0f), saturate(result.a));
}
