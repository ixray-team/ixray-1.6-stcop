#include "common_clouds.hlsli"
#include "common_aerial.hlsli"

// t0 is the shared cloud shape texture.
Texture2D<float4> s_transmittance_lut : register(t1);
Texture2D<float4> s_multi_scattering_lut : register(t2);
RWTexture3D<float4> u_aerial_ambient : register(u0);
RWTexture3D<float4> u_aerial_direct : register(u1);
RWTexture3D<float4> u_aerial_transmittance : register(u2);

// A/B controls for the preliminary source-occlusion approximation.
static const float AP_CLOUD_DIRECT_SHADOW_STRENGTH = 1.0f;
static const float AP_CLOUD_AMBIENT_SHADOW_STRENGTH = 1.0f;

// Coarse source occlusion only: shared shape/profile, no erosion, no camera-ray
// cloud extinction (the view cloud pass already integrates that).
float ap_cloud_visibility(float3 position, float3 direction)
{
    float outer = SKY_EARTH_RADIUS + cloud_layer_params.y;
    float b = dot(position, direction);
    float discriminant = b * b - dot(position, position) + outer * outer;
    if (discriminant <= 0.0f)
        return 1.0f;
    float start = max(0.0f, -b - sqrt(discriminant));
    float stop = -b + sqrt(discriminant);
    float inner = SKY_EARTH_RADIUS + cloud_layer_params.x;
    if (length(position) < inner)
        start = max(start, sky_ray_sphere_intersection(position, direction, inner));
    float ground = sky_ray_sphere_intersection(position, direction, SKY_EARTH_RADIUS);
    if (ground >= 0.0f)
        stop = min(stop, ground);
    if (stop <= start)
        return 1.0f;
    float ds = (stop - start) / 6.0f;
    float tau = 0.0f;
    [unroll]
    for (uint i = 0; i < 6; ++i)
    {
        float3 p = position + direction * (start + (float(i) + 0.5f) * ds);
        float h = cloud_relative_height(p);
        float unused_coverage;
        float3 world = eye_position * cloud_layer_params.z + p - cloud_planet_camera();
        tau += cloud_density_cheap(world, h, cloud_vertical_profile(h), unused_coverage) * ds;
    }
    return exp(-tau * CLOUD_EXTINCTION_KM_INV);
}

[numthreads(8, 4, 1)]
void main(uint3 id : SV_DispatchThreadID)
{
    uint width, height, depth;
    u_aerial_ambient.GetDimensions(width, height, depth);
    if (id.x >= width || id.y >= height)
        return;
    float2 uv = (float2(id.xy) + 0.5f) / float2(width, height);
    float3 direction = sky_world_ray_direction_from_screen_uv(uv);
    // Preserve the existing artistic atmosphere altitude mapping.
    float3 origin = float3(0, SKY_EARTH_RADIUS + max(0.002f * eye_position.y + 0.2f, 0.0f), 0);
    float3 sun = safe_normalize(-L_sun_dir_w);
    float begin, end;
    bool intersects = sky_atmosphere_ray_interval(origin, direction, begin, end);
    float limit = intersects ? min(end, SKY_AP_MAX_DISTANCE_KM) : 0.0f;
    float molecular_phase = sky_molecular_phase(dot(-direction, sun));
    float aerosol_phase = sky_aerosol_phase(dot(-direction, sun));

    // Nine visibility knots per column: avoid shadow rays at every AP sample.
    float2 visibility[9];
    [unroll]
    for (uint k = 0; k < 9; ++k)
    {
        float z = float(k) / 8.0f;
        float d = min(SKY_AP_MAX_DISTANCE_KM * z * z, limit);
        float3 p = cloud_planet_camera() + direction * d;
        float direct = lerp(1.0f, ap_cloud_visibility(p, sun), AP_CLOUD_DIRECT_SHADOW_STRENGTH);
        // Overhead AO proxy, not physical multiple scattering in a cloudy sky.
        float ambient = lerp(0.25f, 1.0f, ap_cloud_visibility(p, normalize(p)));
        visibility[k] = float2(direct, lerp(1.0f, ambient, AP_CLOUD_AMBIENT_SHADOW_STRENGTH));
    }

    float4 direct_sum = 0.0f, ambient_sum = 0.0f, throughput = 1.0f;
    float previous = 0.0f;
    [loop]
    for (uint slice = 0; slice < depth; ++slice)
    {
        float z = float(slice) / max(float(depth - 1), 1.0f);
        float distance = min(SKY_AP_MAX_DISTANCE_KM * z * z, limit);
        float start = max(previous, begin);
        float ds = max(distance - start, 0.0f) * 0.5f;
        [unroll]
        for (uint step = 0; step < 2; ++step)
        {
            if (ds <= 0.0f)
                break;
            float d = start + (float(step) + 0.5f) * ds;
            float3 p = origin + direction * d;
            float r = length(p);
            float altitude = max(r - SKY_EARTH_RADIUS, 0.0f);
            float4 aa, aerosol, ma, molecular, extinction;
            sky_get_collision_coefficients(altitude, aa, aerosol, ma, molecular, extinction);
            float4 sun_T = sky_transmittance_to_sun(s_transmittance_lut, smp_rtlinear, p, sun);
            float4 ms = 0.0f;
#if SKY_ENABLE_MULTIPLE_SCATTERING
            ms = sky_sample_multiscattering_lut(s_multi_scattering_lut, smp_rtlinear,
                dot(p / r, sun), altitude / SKY_ATMOSPHERE_THICKNESS);
#endif
            float knot = sqrt(saturate(d / SKY_AP_MAX_DISTANCE_KM)) * 8.0f;
            uint index = min(uint(knot), 7u);
            float2 v = lerp(visibility[index], visibility[index + 1], knot - float(index));
            float4 direct_source = SKY_SUN_SPECTRAL_IRRADIANCE * sun_T
                * (molecular * molecular_phase + aerosol * aerosol_phase) * v.x;
            float4 ambient_source = SKY_SUN_SPECTRAL_IRRADIANCE * ms * (molecular + aerosol) * v.y;
            float4 step_T = exp(-extinction * ds);
            float4 integral = throughput * (1.0f - step_T) / max(extinction, 1e-7f);
            direct_sum += integral * direct_source;
            ambient_sum += integral * ambient_source;
            throughput *= step_T;
        }
        // Reference-spectrum RGB approximation; do not convert bare spectral T as radiance.
        float3 T = sky_sun_transmittance_rgb(throughput);
        float scalar_T = dot(T, float3(0.2126f, 0.7152f, 0.0722f));
        uint3 voxel = uint3(id.xy, slice);
        u_aerial_ambient[voxel] = float4(SKY_RADIANCE_SCALE * sky_linear_srgb_from_spectral_samples(ambient_sum), scalar_T);
        u_aerial_direct[voxel] = float4(SKY_RADIANCE_SCALE * sky_linear_srgb_from_spectral_samples(direct_sum), scalar_T);
        u_aerial_transmittance[voxel] = float4(T, scalar_T);
        previous = distance;
    }
}
