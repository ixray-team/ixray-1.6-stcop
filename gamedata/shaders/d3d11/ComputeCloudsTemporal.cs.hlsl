#include "common_clouds.hlsli"

Texture2D<float4> s_cloud_current;
Texture2D<float> s_cloud_depth;
Texture2D<float4> s_cloud_history;
Texture2D<float> s_cloud_history_depth;
RWTexture2D<float4> u_cloud_resolved : register(u0);
RWTexture2D<float4> u_cloud_history : register(u1);
RWTexture2D<float> u_cloud_history_depth : register(u2);

uniform float4x4 cloud_previous_view_projection;
uniform float4x4 cloud_previous_inverse_view_projection;
uniform float4 cloud_previous_camera; // engine units
uniform float4 cloud_previous_jitter; // clip XY, same convention as m_taa_jitter
// valid, fresh-measurement weight, engine units to km, relative depth tolerance
uniform float4 cloud_temporal_params;

// Four low-res taps give a spatial fallback and a current-frame clipping envelope.
// Empty sky participates in colour filtering, but never as a zero-km cloud surface.
float4 cloud_reconstruct(uint2 pixel, uint2 size, out float depth_km, out float4 lo, out float4 hi)
{
    uint2 low_size = (size + 1u) / 2u;
    uint2 phase = cloud_trace_pixel(uint2(0, 0), size);
    int2 base = int2(floor((float2(pixel) - float2(phase)) * 0.5f));
    float4 colour = 0.0f;
    float weight_sum = 0.0f, depth_sum = 0.0f, depth_weight = 0.0f;
    lo = 1e30f;
    hi = -1e30f;
    [unroll]
    for (uint i = 0u; i < 4u; ++i)
    {
        int2 tap = clamp(base + int2(i & 1u, i >> 1u), int2(0, 0), int2(low_size) - 1);
        float4 value = s_cloud_current.Load(int3(tap, 0));
        float distance = s_cloud_depth.Load(int3(tap, 0));
        if (!all(isfinite(value)) || !isfinite(distance))
        {
            value = float4(0, 0, 0, 1);
            distance = 0.0f;
        }
        float2 delta = abs(float2(cloud_trace_pixel(uint2(tap), size)) - float2(pixel));
        float2 weights = saturate(1.0f - delta * 0.5f);
        float weight = weights.x * weights.y;
        colour += value * weight;
        weight_sum += weight;
        float dw = distance > 0.0f ? weight * saturate(1.0f - value.a) : 0.0f;
        depth_sum += distance * dw;
        depth_weight += dw;
        lo = min(lo, value);
        hi = max(hi, value);
    }
    depth_km = depth_weight > 1e-6f ? depth_sum / depth_weight : 0.0f;
    return colour / max(weight_sum, 1e-6f);
}

bool cloud_reproject(uint2 pixel, uint2 size, float4 current, float depth_km,
    out float4 history, out float history_depth)
{
    history = current;
    history_depth = depth_km;
    if (cloud_temporal_params.x < 0.5f || depth_km <= 0.0f)
        return false;

    float2 uv = cloud_unjittered_uv((float2(pixel) + 0.5f) / float2(size));
    float3 world = eye_position + sky_world_ray_direction_from_screen_uv(uv) * (depth_km / cloud_temporal_params.z);
    float4 clip = mul(cloud_previous_view_projection, float4(world, 1.0f));
    if (clip.w <= 1e-5f || !all(isfinite(clip)))
        return false;
    // The history was stored on last frame's jittered raster grid, not its trace grid.
    float2 previous_uv = clip.xy / clip.w * float2(0.5f, -0.5f) + 0.5f;
    previous_uv += cloud_previous_jitter.xy * float2(0.5f, -0.5f);
    float2 border = 0.5f / float2(size);
    if (any(previous_uv < border) || any(previous_uv > 1.0f - border))
        return false;

    float expected = length(world - cloud_previous_camera.xyz) * cloud_temporal_params.z;
    float tolerance = max(0.15f, expected * cloud_temporal_params.w);
    float2 sample_position = previous_uv * float2(size) - 0.5f;
    int2 base = int2(floor(sample_position));
    float2 blend = frac(sample_position);
    float4 colour = 0.0f;
    float distance_sum = 0.0f, weight_sum = 0.0f;
    [unroll]
    for (uint i = 0u; i < 4u; ++i)
    {
        int2 offset = int2(i & 1u, i >> 1u);
        int2 tap = clamp(base + offset, int2(0, 0), int2(size) - 1);
        float distance = s_cloud_history_depth.Load(int3(tap, 0));
        float4 value = s_cloud_history.Load(int3(tap, 0));
        // Validate each tap before filtering: do not blend depth with empty sky.
        if (!isfinite(distance) || !all(isfinite(value)) || distance <= 0.0f ||
            abs(distance - expected) > tolerance || abs(value.a - current.a) > 0.25f)
            continue;
        float2 weights = lerp(1.0f - blend, blend, float2(offset));
        float weight = weights.x * weights.y;
        colour += value * weight;
        distance_sum += distance * weight;
        weight_sum += weight;
    }
    if (weight_sum < 0.25f)
        return false;
    history = colour / weight_sum;

    // A retained history distance is measured from the PREVIOUS camera.
    // Reconstruct its representative point, then store distance from this camera.
    float2 previous_unjittered_uv = previous_uv - cloud_previous_jitter.xy * float2(0.5f, -0.5f);
    float4 far_point = mul(cloud_previous_inverse_view_projection,
        float4(previous_unjittered_uv * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 1.0f, 1.0f));
    if (!all(isfinite(far_point)) || abs(far_point.w) <= 1e-6f)
        return false;
    float3 previous_ray = safe_normalize(far_point.xyz / far_point.w - cloud_previous_camera.xyz);
    float3 history_world = cloud_previous_camera.xyz + previous_ray * (distance_sum / (weight_sum * cloud_temporal_params.z));
    history_depth = length(history_world - eye_position) * cloud_temporal_params.z;
    return isfinite(history_depth) && history_depth > 0.0f;
}

[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint width, height;
    u_cloud_resolved.GetDimensions(width, height);
    uint2 pixel = dispatch_id.xy;
    uint2 size = uint2(width, height);
    if (any(pixel >= size))
        return;

    float depth_km;
    float4 lo, hi;
    float4 current = cloud_reconstruct(pixel, size, depth_km, lo, hi);
    bool traced_now = all(pixel == cloud_trace_pixel(pixel / 2u, size));
    if (traced_now)
    {
        // A real empty measurement is authoritative; an untraced pixel is not empty.
        current = s_cloud_current.Load(int3(pixel / 2u, 0));
        depth_km = s_cloud_depth.Load(int3(pixel / 2u, 0));
    }

    float4 result = current;
    if (depth_km > 0.0f && current.a < 0.9999f)
    {
        float4 history;
        float history_depth;
        if (cloud_reproject(pixel, size, current, depth_km, history, history_depth))
        {
            // Loose current bounds preserve detail without retaining old silhouettes.
            float4 margin = (hi - lo) * 0.1f + 1e-4f;
            history = clamp(history, lo - margin, hi + margin);
            float current_weight = traced_now ? cloud_temporal_params.y : 0.0f;
            result = lerp(history, current, current_weight);
            depth_km = lerp(history_depth, depth_km, current_weight);
        }
    }
    if (!all(isfinite(result)) || !isfinite(depth_km) || depth_km <= 0.0f || result.a >= 0.9999f)
    {
        result = float4(0, 0, 0, 1);
        depth_km = 0.0f;
    }
    result = float4(max(result.rgb, 0.0f), saturate(result.a));
    u_cloud_resolved[pixel] = result;
    u_cloud_history[pixel] = result;
    u_cloud_history_depth[pixel] = depth_km;
}
