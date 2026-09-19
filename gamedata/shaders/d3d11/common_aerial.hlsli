#ifndef COMMON_AERIAL_HLSLI
#define COMMON_AERIAL_HLSLI
#include "atmosphere_config.h"
float3 sky_aerial_uv(float2 uv, float distance_km, uint depth)
{
    float z = sqrt(saturate(distance_km / SKY_AP_MAX_DISTANCE_KM));
    return float3(saturate(uv), (z * (float(depth) - 1.0f) + 0.5f) / float(depth));
}
#endif
