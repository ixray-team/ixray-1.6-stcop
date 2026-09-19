#include "common_clouds.hlsli"

// Single 2D layer: optical depth, interval start/end in light clip Z, transmittance.
RWTexture2D<float4> u_cloud_shadow_map : register(u0);
uniform float4x4 cloud_shadow_inverse_view_projection; // clip -> camera-relative km
uniform float4 cloud_shadow_params; // x: march budget; y/z: reserved; w: consumer mode

// Both roots are needed because the light near plane may lie inside the sphere.
bool cloud_sphere_interval(float3 origin, float3 direction, float radius, out float2 interval)
{
    float b = dot(origin, direction);
    float discriminant = b * b - (dot(origin, origin) - radius * radius);
    interval = 0.0f;
    if (discriminant < 0.0f)
        return false;
    float root = sqrt(discriminant);
    interval = float2(-b - root, -b + root);
    return true;
}

[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint width, height;
    u_cloud_shadow_map.GetDimensions(width, height);
    uint2 pixel = dispatch_id.xy;
    if (pixel.x >= width || pixel.y >= height)
        return;

    // Empty texels are explicitly written each frame, including invalid layers/night.
    float4 result = float4(0.0f, 0.0f, 0.0f, 1.0f);
    float2 uv = (float2(pixel) + 0.5f) / float2(width, height);
    float2 xy = uv * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f);
    float4 near_h = mul(cloud_shadow_inverse_view_projection, float4(xy, 0.0f, 1.0f));
    float4 far_h = mul(cloud_shadow_inverse_view_projection, float4(xy, 1.0f, 1.0f));
    float3 near_position = near_h.xyz / near_h.w;
    float3 far_position = far_h.xyz / far_h.w;
    float ray_length = length(far_position - near_position);
    float3 direction = (far_position - near_position) / ray_length; // away from Sun
    float3 planet_origin = cloud_planet_camera() + near_position;

    float2 interval;
    if (cloud_layer_params.y > cloud_layer_params.x &&
        cloud_sphere_interval(planet_origin, direction, SKY_EARTH_RADIUS + cloud_layer_params.y, interval))
    {
        float start = max(interval.x, 0.0f);
        float stop = min(interval.y, ray_length);
        // Do not integrate clouds on the far side of the opaque planet.
        float ground = sky_ray_sphere_intersection(planet_origin, direction, SKY_EARTH_RADIUS);
        if (ground >= 0.0f)
            stop = min(stop, ground);
        if (length(planet_origin) < SKY_EARTH_RADIUS)
            stop = start;

        // Exclude the inner empty sphere when the near shell segment ends there.
        float2 inner;
        if (cloud_sphere_interval(planet_origin, direction, SKY_EARTH_RADIUS + cloud_layer_params.x, inner))
        {
            // If the near plane is inside the inner sphere, start at its exit.
            if (inner.x <= start && inner.y > start)
                start = max(start, inner.y);
            // When a second shell segment is outside this volume/behind the planet,
            // avoid wasting the integration budget on the empty inner interval.
            else if (inner.x > start && inner.y >= stop)
                stop = min(stop, inner.x);
        }

        if (stop > start)
        {
            uint steps = uint(clamp(cloud_shadow_params.x, 1.0f, 128.0f));
            float ds = (stop - start) / float(steps);
            float tau = 0.0f;
            [loop]
            for (uint i = 0u; i < steps; ++i)
            {
                // Deterministic midpoint integration. No spatial or temporal jitter.
                float distance = start + (float(i) + 0.5f) * ds;
                float3 relative_position = near_position + direction * distance;
                float h = cloud_relative_height(cloud_planet_camera() + relative_position);
                float coverage;
                // Same full shape/erosion as the view; no screen-space FAST offset.
                float density = cloud_density(eye_position * cloud_layer_params.z + relative_position,
                    h, cloud_vertical_profile(h), coverage);
                tau += density * (CLOUD_EXTINCTION_KM_INV * ds);
            }
            result = float4(tau, start / ray_length, stop / ray_length, exp(-tau));
        }
    }
    u_cloud_shadow_map[pixel] = result;
}
