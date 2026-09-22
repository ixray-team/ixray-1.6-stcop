#ifndef COMMON_CELESTIAL_HLSLI
#define COMMON_CELESTIAL_HLSLI

#include "common_sky.hlsli"

// x: mode (sun=0, moon=1, moonless=2), y: inverse squared chord radius,
// z: disk luminance scale / solid angle, w: unused (legacy corona intensity).
// Angular-size-dependent constants are prepared by the CPU binder.
uniform float4 celestial_params;

// Cloud alpha includes the artistic distance fade, so it is not physical optical
// depth. Use a separate artistic visibility curve for the very bright source.
static const float SKY_SUN_CLOUD_OPAQUE_OPACITY = 0.98f;

float sky_cloud_sun_visibility(float cloud_transmittance)
{
    float opacity = 1.0f - saturate(cloud_transmittance);
    float visibility = saturate(1.0f - opacity / SKY_SUN_CLOUD_OPAQUE_OPACITY);
    // Power 8 suppresses HDR leakage and reaches zero smoothly at opaque opacity.
    // Clear sky remains exactly one. No pow/log is needed for this fixed exponent.
    visibility *= visibility;
    visibility *= visibility;
    return visibility * visibility;
}

// Pixel-shader function: analytic disk coverage, no trigonometry or limb darkening.
float3 sky_sun_disk(Texture2D<float4> transmittance_lut, SamplerState lut_sampler, float3 ray_direction, float3 source_direction)
{
    // Squared chord distance is stable near the source and needs no sqrt/acos.
    float3 delta = ray_direction - source_direction;
    float radius_squared = dot(delta, delta) * celestial_params.y;
    // Approximately one pixel of linear coverage instead of a broad, dim rim.
    float coverage = saturate(0.5f + (1.0f - radius_squared) / max(fwidth(radius_squared), 1e-6f));

    float profile = 0.0001 * coverage * celestial_params.z * (1.0f - saturate(celestial_params.x));

    // Includes planet occlusion along THIS pixel's ray, not the disk centre.
    float4 T = sky_transmittance_to_sun(transmittance_lut, lut_sampler, sky_atmosphere_camera_position(), ray_direction);
    return SKY_RADIANCE_SCALE * sky_source_rgb(SKY_SUN_SPECTRAL_IRRADIANCE * T) * profile;
}

#endif
