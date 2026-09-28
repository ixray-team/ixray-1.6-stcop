#ifndef ATMOSPHERE_CONFIG_H
#define ATMOSPHERE_CONFIG_H
// Shared by C++ and HLSL. Distances are kilometres, resolution is internal viewport.
#ifdef __cplusplus
#define SKY_CONSTANT static constexpr
#else
#define SKY_CONSTANT static const
#endif
SKY_CONSTANT float SKY_AP_MAX_DISTANCE_KM = 120.0f;
SKY_CONSTANT int SKY_AP_DOWNSAMPLE = 20;
SKY_CONSTANT int SKY_AP_DEPTH = 64;
SKY_CONSTANT float SKY_RADIANCE_SCALE = 4.0f;
SKY_CONSTANT float SKY_WORLD_TO_KM = 0.001f;
SKY_CONSTANT float SKY_CLOUD_BOTTOM_KM = 1.5f;
SKY_CONSTANT float SKY_CLOUD_TOP_KM = 4.0f;
#undef SKY_CONSTANT
#endif
