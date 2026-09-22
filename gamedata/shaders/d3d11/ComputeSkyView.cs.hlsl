#include "common_sky.hlsli"

Texture2D<float4> s_transmittance_lut : register(t0);
Texture2D<float4> s_multi_scattering_lut : register(t1);

RWTexture2D<float4> u_sky_view : register(u0);

// Camera altitude above the atmospheric ground, in kilometers.
uniform float sky_camera_elevation;

float4 compute_sky_inscattering(float3 ray_origin, float3 ray_direction, float ray_length, float3 sun_direction)
{
    float cos_theta = dot(-ray_direction, sun_direction);
    float molecular_phase = sky_molecular_phase(cos_theta);
    float aerosol_phase = sky_aerosol_phase(cos_theta);

    float4 inscattering = float4(0.0f, 0.0f, 0.0f, 0.0f);

    float4 transmittance = float4(1.0f, 1.0f, 1.0f, 1.0f);

    [loop]
    for (uint i = 0; i < SKY_IN_SCATTERING_STEPS; ++i)
    {
        float segment_start_normalized = float(i) / float(SKY_IN_SCATTERING_STEPS);

        float segment_end_normalized = float(i + 1u) / float(SKY_IN_SCATTERING_STEPS);

        // Concentrate integration samples near the camera.
        segment_start_normalized *= segment_start_normalized;
        segment_end_normalized *= segment_end_normalized;
        float segment_start = ray_length * segment_start_normalized;
        float segment_end = ray_length * segment_end_normalized;
        float step_length = segment_end - segment_start;
        // The original clouds_v3 implementation samples at 30% of
        // the current integration segment.
        float sample_distance = lerp(segment_start, segment_end, 0.3f);
        float3 sample_position = ray_origin + ray_direction * sample_distance;
        float distance_to_earth_center = length(sample_position);
        float3 zenith_direction = sample_position / max(distance_to_earth_center, 1e-6f);
        float altitude = distance_to_earth_center - SKY_EARTH_RADIUS;
        float normalized_altitude = altitude / SKY_ATMOSPHERE_THICKNESS;
        float sun_zenith_cosine = dot(zenith_direction, sun_direction);
        
        float4 aerosol_absorption, aerosol_scattering, molecular_absorption, molecular_scattering, extinction;
        sky_get_collision_coefficients(altitude, aerosol_absorption, aerosol_scattering, molecular_absorption, molecular_scattering, extinction);

        float4 sun_transmittance = sky_transmittance_to_sun(s_transmittance_lut, smp_rtlinear, sample_position, sun_direction);

#if SKY_ENABLE_MULTIPLE_SCATTERING
        float4 multiple_scattering = sky_sample_multiscattering_lut(s_multi_scattering_lut, smp_rtlinear, sun_zenith_cosine, normalized_altitude );
#else
        float4 multiple_scattering = float4(0.0f, 0.0f, 0.0f, 0.0f);
#endif
        float4 scattering = molecular_scattering + aerosol_scattering;
        float4 single_scattering = molecular_scattering * molecular_phase + aerosol_scattering * aerosol_phase;
        float4 source = SKY_SUN_SPECTRAL_IRRADIANCE * (sun_transmittance * single_scattering + scattering * multiple_scattering);
        float4 step_transmittance = exp(-step_length * extinction);
        // Energy-conserving analytical integration from:
        // "Physically Based Sky, Atmosphere and Cloud Rendering
        // in Frostbite", Sebastien Hillaire.
        float4 integrated_source = source * (1.0f - step_transmittance) / max(extinction, 1e-7f);
        inscattering += transmittance * integrated_source;
        transmittance *= step_transmittance;
    }

    return inscattering;
}

[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint output_width;
    uint output_height;
    u_sky_view.GetDimensions(output_width, output_height);
    if (dispatch_id.x >= output_width ||dispatch_id.y >= output_height)
    {
        return;
    }
    float2 uv = (float2(dispatch_id.xy) + 0.5f) / float2(output_width, output_height);

    float camera_elevation = sky_get_camera_elevation();

    float3 up = float3(0.0f, 1.0f, 0.0f);
    float3 right;
    float3 forward;
    // X-Ray stores the direction in which sunlight travels.
    // Atmospheric functions expect the direction from the point
    // towards the Sun.
    float3 sun_direction = safe_normalize(-L_sun_dir_w);

    sky_build_basis(sun_direction, up, right, forward);

    // SkyView stores only one side of the symmetry plane formed by
    // the local up vector and the Sun direction.
    float relative_azimuth = sky_u_to_relative_azimuth(uv.x, SKY_VIEW_AZIMUTH_GAMMA);
    float3 ray_origin = float3(0.0f, SKY_EARTH_RADIUS + camera_elevation, 0.0f);
    float height = length(ray_origin);
    float horizon_cosine = sqrt(max(height * height - SKY_EARTH_RADIUS * SKY_EARTH_RADIUS, 0.0f)) / max(height, 1e-6f);
    float horizon_offset = acos(saturate(horizon_cosine)) - PI * 0.5f;
    // Non-linear latitude mapping allocates more texels near the
    // horizon, where the atmospheric gradient changes faster.
    float elevation = sky_v_to_latitude(uv.y, SKY_VIEW_LATITUDE_GAMMA) - horizon_offset;
    float cos_elevation = cos(elevation);
    float3 ray_direction = 
            forward * (cos_elevation * cos(relative_azimuth)) +
            right * (cos_elevation * sin(relative_azimuth)) +
            up * sin(elevation);

    ray_direction = safe_normalize(ray_direction);
    float atmosphere_distance = sky_ray_sphere_intersection(ray_origin, ray_direction, SKY_ATMOSPHERE_RADIUS);
    float ground_distance = sky_ray_sphere_intersection(ray_origin, ray_direction, SKY_EARTH_RADIUS);
    float ray_length = 0.0f;

    if (camera_elevation < SKY_ATMOSPHERE_THICKNESS)
    {
        // Camera is inside the atmosphere. Stop either at the ground
        // or at the outer atmosphere boundary.
        ray_length = ground_distance < 0.0f ? atmosphere_distance : ground_distance;
    }
    else
    {
        // Camera is outside the atmosphere.
        if (atmosphere_distance < 0.0f)
        {
            u_sky_view[dispatch_id.xy] = float4(0.0f, 0.0f, 0.0f, 1.0f);
            return;
        }
        // Move to the entry point to avoid integrating empty space.
        ray_origin += ray_direction * (atmosphere_distance + 1e-3f);
        if (ground_distance < 0.0f)
        {
            ray_length = sky_ray_sphere_intersection(ray_origin, ray_direction, SKY_ATMOSPHERE_RADIUS);
        }
        else
        {
            ray_length = ground_distance - atmosphere_distance;
        }
    }

    float4 spectral_radiance = float4(0.0f, 0.0f, 0.0f, 0.0f);
    if (ray_length > 0.0f)
    {
        spectral_radiance = compute_sky_inscattering(ray_origin, ray_direction, ray_length, sun_direction);
    }
#if SKY_ENABLE_SPECTRAL
    const float3 linear_rgb = SKY_RADIANCE_SCALE * sky_source_rgb(spectral_radiance);
#else
    float3 linear_rgb = SKY_RADIANCE_SCALE * max(celestial_source_color.rgb, 0.0f) * max(spectral_radiance.rgb, 0.0f);
#endif
    // SkyView remains linear HDR. Gamma and tone mapping are applied
    // only during final image composition.
    u_sky_view[dispatch_id.xy] = float4(linear_rgb, 1.0f);
}
