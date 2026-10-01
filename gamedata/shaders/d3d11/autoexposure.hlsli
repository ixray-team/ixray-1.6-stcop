#ifndef AUTOEXPOSURE_H
#define AUTOEXPOSURE_H

// Tonemap input scale, minimum/maximum adaptation EV100, compensation in stops.
float4 autoexposure_params;
// Scene luminance in cd/m2 per HDR unit, histogram minimum/maximum EV100, unused.
float4 autoexposure_metering;

float EV100ToNits(float ev100)
{
    return 0.125f * exp2(ev100); // Reflected-light meter, K = 12.5 at ISO 100.
}

float SceneLuminanceToEV100(float luminance)
{
    float nits = max(luminance, 0.0f) * autoexposure_metering.x;
    return clamp(log2(max(nits, EV100ToNits(autoexposure_metering.y)) * 8.0f),
        autoexposure_metering.y, autoexposure_metering.z);
}

float CameraEV100(float adaptedEV100)
{
    // Positive compensation brightens the image, after the automatic limits.
    return clamp(adaptedEV100, autoexposure_params.y, autoexposure_params.z) - autoexposure_params.w;
}

float ExposureMultiplierFromEV100(float cameraEV100)
{
    return autoexposure_metering.x * autoexposure_params.x / 1.2f * exp2(-cameraEV100);
}

float ExposedLuminanceFromEV100(float sceneEV100, float exposureMultiplier)
{
    return EV100ToNits(sceneEV100) / autoexposure_metering.x * exposureMultiplier;
}

#endif
