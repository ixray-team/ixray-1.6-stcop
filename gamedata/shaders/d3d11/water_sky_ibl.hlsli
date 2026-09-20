#ifndef WATER_SKY_IBL_INCLUDED
#define WATER_SKY_IBL_INCLUDED

#include "common_sky.hlsli"

Texture2D<float4> s_sky_octo_ibl;
Texture2D<float4> s_sky_octo_ibl_diffuse;
Texture2D<float4> s_transmittance_lut;

float3 water_sun_transmittance()
{
    // Match accum_sun's atmospheric position and spectral-to-RGB filter.
    float camera_elevation_km = max(0.002f * eye_position.y + 0.2f, 0.0f);
    float3 position = float3(0.0f, SKY_EARTH_RADIUS + camera_elevation_km, 0.0f);
    return sky_sun_transmittance_rgb(sky_transmittance_to_sun(
        s_transmittance_lut, smp_rtlinear, position, safe_normalize(-L_sun_dir_w)));
}

// Water directions are already world-space. Return linear radiance/irradiance.
float3 water_sky_reflection(float3 direction)
{
    return sky_sample_gt7_octahedral_map(s_sky_octo_ibl, smp_rtlinear, direction, 4.0f).rgb;
}

float3 water_sky_diffuse(float3 normal)
{
    return sky_sample_gt7_octahedral_map(s_sky_octo_ibl_diffuse, smp_rtlinear, normal, 1.0f).rgb;
}

#endif
