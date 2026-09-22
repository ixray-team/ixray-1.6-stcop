#include "common_sky.hlsli"
#include "common_aerial.hlsli"

Texture2D<float4> s_transmittance_lut : register(t0);
Texture2D<float4> s_multi_scattering_lut : register(t1);
RWTexture3D<float4> u_aerial_ambient : register(u0);
RWTexture3D<float4> u_aerial_direct : register(u1);
RWTexture3D<float4> u_aerial_transmittance : register(u2);

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
    float3 origin = sky_atmosphere_camera_position();
    float3 sun = safe_normalize(-L_sun_dir_w);
    float begin, end;
    bool intersects = sky_atmosphere_ray_interval(origin, direction, begin, end);
    float limit = intersects ? min(end, SKY_AP_MAX_DISTANCE_KM) : 0.0f;
    float molecular_phase = sky_molecular_phase(dot(-direction, sun));
    float aerosol_phase = sky_aerosol_phase(dot(-direction, sun));

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
            float4 direct_source = SKY_SUN_SPECTRAL_IRRADIANCE * sun_T
                * (molecular * molecular_phase + aerosol * aerosol_phase);
            float4 ambient_source = SKY_SUN_SPECTRAL_IRRADIANCE * ms * (molecular + aerosol);
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
        u_aerial_ambient[voxel] = float4(SKY_RADIANCE_SCALE * sky_source_rgb(ambient_sum), scalar_T);
        u_aerial_direct[voxel] = float4(SKY_RADIANCE_SCALE * sky_source_rgb(direct_sum), scalar_T);
        u_aerial_transmittance[voxel] = float4(T, scalar_T);
        previous = distance;
    }
}
