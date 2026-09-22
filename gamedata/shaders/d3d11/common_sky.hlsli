#include "common.hlsli"
#include "atmosphere_config.h"
#include "atmosphere_config.h"

// Weather source before atmospheric attenuation. Shared with surface lighting;
// includes source_intensity and the renderer's sun luminance scale exactly once.
uniform float4 celestial_source_color;

//-----------------------------------------------------------------------------
// Configuration
//-----------------------------------------------------------------------------
#ifndef SKY_IN_SCATTERING_STEPS
#define SKY_IN_SCATTERING_STEPS 30
#endif
#ifndef SKY_ENABLE_MULTIPLE_SCATTERING
#define SKY_ENABLE_MULTIPLE_SCATTERING 1
#endif
#ifndef SKY_ENABLE_SPECTRAL
#define SKY_ENABLE_SPECTRAL 1
#endif
#ifndef SKY_AEROSOL_TYPE
#define SKY_AEROSOL_TYPE 7
#endif
//-----------------------------------------------------------------------------
// General constants
//-----------------------------------------------------------------------------
static const float SKY_INV_PI = 0.31830988618379067154f;
static const float SKY_INV_4PI = 0.25f * SKY_INV_PI;
static const float SKY_PHASE_ISOTROPIC = SKY_INV_4PI;
static const float SKY_RAYLEIGH_PHASE_SCALE = (3.0f / 16.0f) * SKY_INV_PI;
static const float SKY_MIE_G = 0.8f;
static const float SKY_MIE_G_SQUARED = SKY_MIE_G * SKY_MIE_G;
// All atmospheric distances are expressed in kilometers.
static const float SKY_EARTH_RADIUS = 6371.0f;
static const float SKY_ATMOSPHERE_THICKNESS = 100.0f;
static const float SKY_ATMOSPHERE_RADIUS = SKY_EARTH_RADIUS + SKY_ATMOSPHERE_THICKNESS;

float sky_get_camera_elevation()
{
    return max(0.001f * eye_position.y + 0.2f, 0.0f);
}

float3 sky_atmosphere_camera_position()
{
    return float3(0.0f, SKY_EARTH_RADIUS + sky_get_camera_elevation(), 0.0f);
}
static const float SKY_AEROSOL_TURBIDITY = 1.0f; // make this depending on fog_amount
// Mean ozone concentration for August.
static const float SKY_OZONE_DOBSON = 317.0f;
static const float SKY_VIEW_AZIMUTH_GAMMA = 0.5f;
static const float SKY_VIEW_LATITUDE_GAMMA = 0.5f;
// Spectral wavelengths: 630, 560, 490 and 430 nm.
static const float4 SKY_SUN_SPECTRAL_IRRADIANCE = float4(1.679f, 1.828f, 1.986f, 1.307f);
static const float4 SKY_MOLECULAR_SCATTERING_BASE = float4(6.605e-3f, 1.067e-2f, 1.842e-2f, 3.156e-2f);
static const float4 SKY_OZONE_ABSORPTION_CROSS_SECTION = float4(3.472e-21f, 3.914e-21f, 1.349e-21f, 11.03e-23f) * 1e-4f;
//-----------------------------------------------------------------------------
// Aerosol model
//
// Parameters are taken from:
// "A Physically-Based Spatio-Temporal Sky Model"
// Guimera et al. (2018).
//-----------------------------------------------------------------------------
#if SKY_AEROSOL_TYPE == 0 // Background
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(4.5517e-19f, 5.9269e-19f, 6.9143e-19f, 8.5228e-19f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(1.8921e-26f, 1.6951e-26f, 1.7436e-26f, 2.1158e-26f);
    static const float SKY_AEROSOL_BASE_DENSITY = 2.584e17f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
#elif SKY_AEROSOL_TYPE == 1 // Desert Dust
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(4.6758e-16f, 4.4654e-16f, 4.1989e-16f, 4.1493e-16f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(2.9144e-16f, 3.1463e-16f, 3.3902e-16f, 3.4298e-16f);
    static const float SKY_AEROSOL_BASE_DENSITY = 1.8662e18f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 2.0f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
#elif SKY_AEROSOL_TYPE == 2 // Maritime Clean
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(6.3312e-19f, 7.5567e-19f, 9.2627e-19f, 1.0391e-18f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(4.6539e-26f, 2.7210e-26f, 4.1104e-26f, 5.6249e-26f);
    static const float SKY_AEROSOL_BASE_DENSITY = 2.0266e17f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 0.9f;
#elif SKY_AEROSOL_TYPE == 3 // Maritime Mineral
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(6.9365e-19f, 7.5951e-19f, 8.2423e-19f, 8.9101e-19f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(2.3699e-19f, 2.2439e-19f, 2.2126e-19f, 2.0210e-19f);
    static const float SKY_AEROSOL_BASE_DENSITY = 2.0266e17f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 2.0f;
#elif SKY_AEROSOL_TYPE == 4 // Polar Antarctic
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(1.3399e-16f, 1.3178e-16f, 1.2909e-16f, 1.3006e-16f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(1.5506e-19f, 1.8090e-19f, 2.3069e-19f, 2.5804e-19f);
    static const float SKY_AEROSOL_BASE_DENSITY = 2.3864e16f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 30.0f;
#elif SKY_AEROSOL_TYPE == 5 // Polar Arctic
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(1.0364e-16f, 1.0609e-16f, 1.0193e-16f, 1.0092e-16f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(2.1609e-17f, 2.2759e-17f, 2.5089e-17f, 2.6323e-17f);
    static const float SKY_AEROSOL_BASE_DENSITY = 2.3864e16f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 30.0f;
#elif SKY_AEROSOL_TYPE == 6 // Remote Continental
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(4.5307e-18f, 5.0662e-18f, 4.4877e-18f, 3.7917e-18f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(1.8764e-18f, 1.7460e-18f, 1.6902e-18f, 1.4790e-18f);
    static const float SKY_AEROSOL_BASE_DENSITY = 6.103e18f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 0.73f;
#elif SKY_AEROSOL_TYPE == 7 // Rural
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(5.0393e-23f, 8.0765e-23f, 1.3823e-22f, 2.3383e-22f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(2.6004e-22f, 2.4844e-22f, 2.8362e-22f, 2.7494e-22f);
    static const float SKY_AEROSOL_BASE_DENSITY = 8.544e18f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 0.73f;
#elif SKY_AEROSOL_TYPE == 8 // Urban
    static const float4 SKY_AEROSOL_ABSORPTION_CROSS_SECTION = float4(2.8722e-24f, 4.6168e-24f, 7.9706e-24f, 1.3578e-23f);
    static const float4 SKY_AEROSOL_SCATTERING_CROSS_SECTION = float4(1.5908e-22f, 1.7711e-22f, 2.0942e-22f, 2.4033e-22f);
    static const float SKY_AEROSOL_BASE_DENSITY = 1.3681e20f;
    static const float SKY_AEROSOL_BACKGROUND_DENSITY = 2.0e6f;
    static const float SKY_AEROSOL_HEIGHT_SCALE = 0.73f;
#else
    #error Unsupported SKY_AEROSOL_TYPE
#endif
static const float SKY_AEROSOL_BACKGROUND_RATIO = SKY_AEROSOL_BACKGROUND_DENSITY / SKY_AEROSOL_BASE_DENSITY;

//-----------------------------------------------------------------------------
// Atmospheric density and collision coefficients
//-----------------------------------------------------------------------------

float sky_get_aerosol_density(float altitude)
{
#if SKY_AEROSOL_TYPE == 0
    return SKY_AEROSOL_BASE_DENSITY * (1.0f + SKY_AEROSOL_BACKGROUND_RATIO);
#else
    return SKY_AEROSOL_BASE_DENSITY * (exp(-altitude / SKY_AEROSOL_HEIGHT_SCALE) + SKY_AEROSOL_BACKGROUND_RATIO);
#endif
}

float4 sky_get_molecular_absorption_coefficient(float altitude)
{
    // Avoid log(0).
    altitude = max(altitude, 0.0f) + 1e-4f;

    float t = log(altitude) - 3.22261f;
    float density = 3.78547397e20f * rcp(altitude) * exp(-t * t * 5.55555555f);

    return SKY_OZONE_ABSORPTION_CROSS_SECTION * SKY_OZONE_DOBSON * density;
}

float4 sky_get_molecular_scattering_coefficient(float altitude)
{
    altitude = max(altitude, 0.0f);

    return SKY_MOLECULAR_SCATTERING_BASE * exp(-0.07771971f * pow(altitude, 1.16364243f));
}

void sky_get_collision_coefficients(float altitude, out float4 aerosol_absorption, out float4 aerosol_scattering, out float4 molecular_absorption, out float4 molecular_scattering, out float4 extinction)
{
    altitude = max(altitude, 0.0f);
    float aerosol_density = sky_get_aerosol_density(altitude);
    aerosol_absorption = SKY_AEROSOL_ABSORPTION_CROSS_SECTION * aerosol_density * SKY_AEROSOL_TURBIDITY;
    aerosol_scattering = SKY_AEROSOL_SCATTERING_CROSS_SECTION * aerosol_density * SKY_AEROSOL_TURBIDITY;
    molecular_absorption = sky_get_molecular_absorption_coefficient(altitude);
    molecular_scattering = sky_get_molecular_scattering_coefficient(altitude);
    extinction = aerosol_absorption + aerosol_scattering + molecular_absorption + molecular_scattering;
}

//-----------------------------------------------------------------------------
// Phase functions
//-----------------------------------------------------------------------------

float sky_molecular_phase(float cos_theta)
{
    return SKY_RAYLEIGH_PHASE_SCALE * (1.0f + cos_theta * cos_theta);
}

// Cornette-Shanks aerosol phase function.
//
// cos_theta follows the convention used by the original clouds_v3 shaders:
// dot(-view_direction, direction_to_sun).
float sky_aerosol_phase(float cos_theta)
{
    float denominator = 1.0f + SKY_MIE_G_SQUARED + 2.0f * SKY_MIE_G * cos_theta;
    float numerator = (1.0f - SKY_MIE_G_SQUARED) * (1.0f + cos_theta * cos_theta);
    return (3.0f / (8.0f * PI)) * numerator / ((2.0f + SKY_MIE_G_SQUARED) * denominator * sqrt(max(denominator, 1e-7f)));
}

float cloud_henyey_greenstein(float cos_theta, float g)
{
    g = clamp(g, -0.99f, 0.99f);

    const float g2 = g * g;
    const float denominator = max(1.0f + g2 - 2.0f * g * cos_theta, 1e-4f);

    const float rsqrt_denom = rsqrt(denominator);
    return (1.0f - g2) * rsqrt_denom * rsqrt_denom * rsqrt_denom * (1.0f / (4.0f * PI));
}

float cloud_beer_powder(float optical_depth)
{
    const float beer = exp(-optical_depth);
    const float powder = 1.0f - exp(-2.0f * optical_depth);

    return 2.0f * beer * powder;
}

//-----------------------------------------------------------------------------
// Coordinate helpers
//-----------------------------------------------------------------------------

float sky_unit_range_to_texture_coord(float value, float texture_size)
{
    float inverse_size = rcp(texture_size);
    return 0.5f * inverse_size + saturate(value) * (1.0f - inverse_size);
}

float sky_texture_coord_to_unit_range(float texture_coord, float texture_size)
{
    float inverse_size = rcp(texture_size);
    return saturate((texture_coord - 0.5f * inverse_size) / (1.0f - inverse_size));
}

float sky_relative_azimuth_to_u(float relative_azimuth, float gamma_value)
{
    float normalized_azimuth = saturate(relative_azimuth * SKY_INV_PI);
    return pow(normalized_azimuth, gamma_value);
}

float sky_u_to_relative_azimuth(float u, float gamma_value)
{
    return pow(saturate(u), rcp(gamma_value)) * PI;
}

float sky_latitude_to_v(float latitude, float gamma_value)
{
    float normalized_latitude = latitude / (PI * 0.5f);
    return 0.5f + 0.5f * sign(normalized_latitude) * pow(abs(normalized_latitude), gamma_value);
}

float sky_v_to_latitude(float v, float gamma_value)
{
    float normalized_latitude = v * 2.0f - 1.0f;
    return sign(normalized_latitude) * pow(abs(normalized_latitude), rcp(gamma_value)) * PI * 0.5f;
}

//-----------------------------------------------------------------------------
// Sky coordinate system
//-----------------------------------------------------------------------------

void sky_build_basis(float3 sun_direction, float3 up, out float3 right, out float3 forward)
{
    up = safe_normalize(up);
    // Align the azimuth axis with the Sun projected onto the local horizon.
    forward = sun_direction - up * dot(sun_direction, up);
    // Sun is at zenith/nadir or the supplied direction is invalid.
    if (dot(forward, forward) < 1e-12f)
    {
        float3 fallback_axis = abs(up.y) < 0.999f ? float3(0.0f, 1.0f, 0.0f) : float3(0.0f, 0.0f, 1.0f);
        forward = fallback_axis - up * dot(fallback_axis, up);
    }
    forward = safe_normalize(forward);
    right = safe_normalize(cross(forward, up));
}

float2 sky_view_uv_from_direction(float3 ray_direction, float3 sun_direction, float camera_elevation)
{
    float height = SKY_EARTH_RADIUS + camera_elevation;
    float3 up = float3(0.0f, 1.0f, 0.0f);
    ray_direction = safe_normalize(ray_direction);
    sun_direction = safe_normalize(sun_direction);
    float view_up = clamp(dot(ray_direction, up), -1.0f, 1.0f);
    float horizon_cosine = sqrt(max(height * height - SKY_EARTH_RADIUS * SKY_EARTH_RADIUS, 0.0f)) / max(height, 1e-6f);
    float horizon_angle = acos(saturate(horizon_cosine));
    float elevation = horizon_angle - acos(view_up);

    float3 right;
    float3 forward;

    sky_build_basis(sun_direction, up, right, forward);
    float3 horizontal_direction = ray_direction - up * view_up;
    float relative_azimuth = PI * 0.5f;

    if (dot(horizontal_direction, horizontal_direction) > 1e-12f)
    {
        float forward_component = dot(horizontal_direction, forward);
        float right_component = dot(horizontal_direction, right);

        // The sky-view LUT stores one side of the Sun/up symmetry plane.
        relative_azimuth = atan2(abs(right_component), forward_component);
    }

    float u = sky_relative_azimuth_to_u(relative_azimuth, SKY_VIEW_AZIMUTH_GAMMA);
    float v = sky_latitude_to_v(elevation, SKY_VIEW_LATITUDE_GAMMA);
    return float2(u, v);
}

float3 sky_world_ray_direction_from_screen_uv(float2 screen_uv)
{
    float2 clip_xy = float2(screen_uv.x * 2.0f - 1.0f, 1.0f - screen_uv.y * 2.0f);
    float4 clip_position = float4(clip_xy,1.0f, 1.0f);
    float4 view_position_h = mul(m_invP,clip_position);
    float inverse_w = abs(view_position_h.w) > 1e-6f ? rcp(view_position_h.w) : 1.0f;
    float3 view_direction = safe_normalize(view_position_h.xyz * inverse_w);
    return safe_normalize(mul((float3x3) m_invV, view_direction));
}

//-----------------------------------------------------------------------------
// Sphere intersections
//-----------------------------------------------------------------------------

float sky_ray_sphere_intersection(float3 ray_origin, float3 ray_direction, float radius)
{
    float b = dot(ray_origin, ray_direction);
    float c = dot(ray_origin, ray_origin) - radius * radius;
    if (c > 0.0f && b > 0.0f)
        return -1.0f;
    float discriminant = b * b - c;
    if (discriminant < 0.0f)
        return -1.0f;
    float sqrt_discriminant = sqrt(discriminant);
    if (discriminant > b * b)
        return -b + sqrt_discriminant;
    return -b - sqrt_discriminant;
}

bool sky_atmosphere_ray_interval(float3 ray_origin, float3 ray_direction, out float interval_start, out float interval_end)
{
    float b = dot(ray_origin, ray_direction);
    float c = dot(ray_origin, ray_origin) - SKY_ATMOSPHERE_RADIUS * SKY_ATMOSPHERE_RADIUS;
    float discriminant = b * b - c;

    if (discriminant < 0.0f)
    {
        interval_start = 0.0f;
        interval_end = 0.0f;
        return false;
    }

    float sqrt_discriminant = sqrt(discriminant);
    interval_start = max(-b - sqrt_discriminant, 0.0f);
    interval_end = -b + sqrt_discriminant;
    float ground_distance = sky_ray_sphere_intersection(ray_origin, ray_direction, SKY_EARTH_RADIUS);
    if (ground_distance >= 0.0f)
        interval_end = min(interval_end, ground_distance);

    return interval_end > interval_start;
}

float sky_distance_to_top_atmosphere_boundary(float radius, float mu)
{
    float discriminant = radius * radius * (mu * mu - 1.0f) + SKY_ATMOSPHERE_RADIUS * SKY_ATMOSPHERE_RADIUS;
    return max(-radius * mu + sqrt(max(discriminant, 0.0f)), 0.0f);
}

//-----------------------------------------------------------------------------
// Transmittance LUT
//-----------------------------------------------------------------------------

float2 sky_transmittance_uv_from_r_mu(float radius, float mu, float2 texture_size)
{
    radius = clamp(radius,SKY_EARTH_RADIUS,SKY_ATMOSPHERE_RADIUS);
    mu = clamp(mu, -1.0f, 1.0f);
    float atmosphere_height = sqrt(SKY_ATMOSPHERE_RADIUS * SKY_ATMOSPHERE_RADIUS - SKY_EARTH_RADIUS * SKY_EARTH_RADIUS);
    float rho = sqrt(max(radius * radius - SKY_EARTH_RADIUS * SKY_EARTH_RADIUS, 0.0f));
    float distance = sky_distance_to_top_atmosphere_boundary(radius, mu);
    float minimum_distance = SKY_ATMOSPHERE_RADIUS - radius;
    float maximum_distance = rho + atmosphere_height;
    float x_mu = maximum_distance > minimum_distance ? (distance - minimum_distance) / (maximum_distance - minimum_distance) : 0.0f;
    float x_radius = rho / atmosphere_height;
    return float2(sky_unit_range_to_texture_coord(x_mu, texture_size.x), sky_unit_range_to_texture_coord(x_radius, texture_size.y));
}

float4 sky_sample_transmittance_lut(Texture2D<float4> transmittance_lut, SamplerState lut_sampler, float cos_theta, float normalized_altitude)
{
    uint texture_width;
    uint texture_height;
    transmittance_lut.GetDimensions(texture_width,texture_height);
    float radius = SKY_EARTH_RADIUS + saturate(normalized_altitude) * SKY_ATMOSPHERE_THICKNESS;
    float2 uv = sky_transmittance_uv_from_r_mu(radius, cos_theta, float2(texture_width, texture_height));
    return transmittance_lut.SampleLevel(lut_sampler, uv, 0.0f);
}

float4 sky_transmittance_to_sun(Texture2D<float4> transmittance_lut, SamplerState lut_sampler, float3 position, float3 sun_direction)
{
    if (sky_ray_sphere_intersection(
        position,
        sun_direction,
        SKY_EARTH_RADIUS) >= 0.0f)
    {
        return float4(0.0f, 0.0f, 0.0f, 0.0f);
    }
    float distance_to_earth_center = length(position);
    float3 zenith_direction = position / max(distance_to_earth_center, 1e-6f);
    float normalized_altitude = (distance_to_earth_center - SKY_EARTH_RADIUS) / SKY_ATMOSPHERE_THICKNESS;
    return sky_sample_transmittance_lut(transmittance_lut, lut_sampler, dot(zenith_direction, sun_direction), normalized_altitude);
}

//-----------------------------------------------------------------------------
// Multiple-scattering LUT
//-----------------------------------------------------------------------------

float4 sky_sample_multiscattering_lut(Texture2D<float4> multiscattering_lut, SamplerState lut_sampler, float mu, float normalized_altitude)
{
    uint texture_width;
    uint texture_height;
    multiscattering_lut.GetDimensions(texture_width, texture_height);
    float2 texture_size = float2(texture_width, texture_height);
    float2 encoded = saturate(float2(mu * 0.5f + 0.5f, normalized_altitude));
    float2 uv = (encoded * (texture_size - 1.0f) + 0.5f) / texture_size;
    return multiscattering_lut.SampleLevel(lut_sampler, uv, 0.0f);
}

//-----------------------------------------------------------------------------
// Sky-view LUT sampling
//-----------------------------------------------------------------------------

float4 sky_sample_view_lut(Texture2D<float4> sky_view_lut, SamplerState lut_sampler, float3 ray_direction, float3 sun_direction, float camera_elevation)
{
    float2 uv = sky_view_uv_from_direction(ray_direction, sun_direction, camera_elevation);
    uint texture_width;
    uint texture_height;
    sky_view_lut.GetDimensions(texture_width, texture_height);

    uv.x = sky_unit_range_to_texture_coord(uv.x, float(texture_width));
    uv.y = sky_unit_range_to_texture_coord(uv.y, float(texture_height));
    return sky_view_lut.SampleLevel(lut_sampler, uv, 0.0f);
}

//-----------------------------------------------------------------------------
// GT7 full-octahedral mapping
//-----------------------------------------------------------------------------

float sky_sign_not_zero(float value)
{
    return value >= 0.0f ? 1.0f : -1.0f;
}

float2 sky_sign_not_zero(float2 value)
{
    return float2(sky_sign_not_zero(value.x), sky_sign_not_zero(value.y));
}

float2 sky_gt7_octahedral_encode(float3 direction)
{
    direction = safe_normalize(direction);
    float2 horizontal = direction.xz;
    float horizontal_length = length(horizontal);
    float2 azimuth = horizontal_length > 1e-8f ? horizontal / horizontal_length : float2(1.0f, 0.0f);
    float polar_angle = acos(saturate(abs(direction.y)));
    float normalized_radius = polar_angle / (PI * 0.5f);
    normalized_radius *= normalized_radius;
    float azimuth_l1 = abs(azimuth.x) + abs(azimuth.y);
    float2 projected = azimuth * (normalized_radius / max(azimuth_l1, 1e-8f));
    if (direction.y >= 0.0f)
        return projected;
    return (1.0f - abs(projected.yx)) * sky_sign_not_zero(projected);
}

float3 sky_gt7_octahedral_decode(float2 encoded)
{
    float encoded_l1 = abs(encoded.x) + abs(encoded.y);
    bool lower_hemisphere = encoded_l1 > 1.0f;
    float2 projected = encoded;
    if (lower_hemisphere)
    {
        projected = (1.0f - abs(encoded.yx)) * sky_sign_not_zero(encoded);
    }
    // Do not clamp the radius. Padding extends the logical mapping into
    // adjacent octahedral triangles and prevents filtering seams.
    float normalized_radius = abs(projected.x) + abs(projected.y);
    float projected_length_squared = dot(projected, projected);
    float2 azimuth = projected_length_squared > 1e-12f ? projected * rsqrt(projected_length_squared) : float2(1.0f, 0.0f);
    // GT7 mapping: radius = (2 * polar_angle / PI)^2.
    float polar_angle = sqrt(normalized_radius) * PI * 0.5f;
    float sin_polar = sin(polar_angle);
    float cos_polar = cos(polar_angle);
    if (lower_hemisphere)
        cos_polar = -cos_polar;
    return safe_normalize(float3(azimuth.x * sin_polar, cos_polar, azimuth.y * sin_polar));
}

float2 sky_gt7_octahedral_texture_uv(float3 direction, float2 texture_resolution, float padding)
{
    float2 logical_resolution = texture_resolution - 2.0f * padding;
    float2 logical_uv = sky_gt7_octahedral_encode(direction) * 0.5f + 0.5f;
    return (padding + logical_uv * logical_resolution) / texture_resolution;
}

float4 sky_sample_gt7_octahedral_map(Texture2D<float4> octahedral_map, SamplerState map_sampler, float3 direction, float padding)
{
    uint texture_width;
    uint texture_height;
    octahedral_map.GetDimensions(texture_width, texture_height);
    float2 texture_resolution = float2(texture_width, texture_height);
    float2 uv = sky_gt7_octahedral_texture_uv(direction, texture_resolution, padding);
    return octahedral_map.SampleLevel(map_sampler, uv, 0.0f);
}


//-----------------------------------------------------------------------------
// Spectral conversion
//-----------------------------------------------------------------------------

// Fitted spectral weights at 630, 560, 490 and 430 nm.
// Keep the original coefficients; normalize with one scalar, never per channel.
static const float3x4 SKY_SPECTRAL_TO_REC709_RAW = float3x4(
    137.672389239975, 32.549094028629234, -38.91428392614275, 8.572844237945445,
    -8.632904716299537, 91.29801417199785, 34.31665471469816, -11.103384660054624,
    -1.7181567391931372, -12.005406444382531, 29.89044807197628, 117.47585277566478
);

// Unit reference: the unattenuated solar spectrum has linear Rec.709 Y = 1.
// This preserves chromaticity and HDR range, and is not a white-balance operation.
static const float SKY_SPECTRAL_NORMALIZATION = 1.0f / dot(float3(0.2126f, 0.7152f, 0.0722f), mul(SKY_SPECTRAL_TO_REC709_RAW, SKY_SUN_SPECTRAL_IRRADIANCE));
static const float3x4 SKY_SPECTRAL_TO_REC709 = SKY_SPECTRAL_TO_REC709_RAW * SKY_SPECTRAL_NORMALIZATION;

float3 sky_linear_srgb_from_spectral_samples(float4 spectral_radiance)
{
    // Apply artistic Sky/Sun brightness to the returned RGB, not to the weights.
    float3 linear_rgb = mul(SKY_SPECTRAL_TO_REC709, spectral_radiance);
    return max(linear_rgb, 0.0f);
}

float3 sky_source_rgb(float4 spectral_radiance)
{
    // RGB weather tint is an artistic approximation applied after spectral integration.
    return max(celestial_source_color.rgb, 0.0f) * sky_linear_srgb_from_spectral_samples(spectral_radiance);
}

float3 sky_sun_transmittance_rgb(float4 spectral_transmittance)
{
    float3 unattenuated_sun = sky_linear_srgb_from_spectral_samples(SKY_SUN_SPECTRAL_IRRADIANCE);
    float3 transmitted_sun = sky_linear_srgb_from_spectral_samples(SKY_SUN_SPECTRAL_IRRADIANCE * saturate(spectral_transmittance));
    return saturate(transmitted_sun / max(unattenuated_sun, 1e-5f));
}

//-----------------------------------------------------------------------------
// Clouds helpers
//-----------------------------------------------------------------------------

float Remap(float original_value, float original_min, float original_max, float new_min, float new_max)
{
    return new_min + ((original_value - original_min) / (original_max - original_min)) * (new_max - new_min);
}
