#pragma once

#include "RHI.h"

class RHI_API DLSSWrapper
{
public:
    struct ContextParameters
    {
        u32 flags = 0;
        Ivector2 renderSize = { 0, 0 };
        Ivector2 displaySize = { 0, 0 };
        u32 preset = 0;
        float scale = 1.f;

    };

    struct DrawParameters
    {

        // Inputs
        IRHISurface* exposureResource = nullptr;
        IRHISurface* unresolvedColorResource = nullptr;
        IRHISurface* motionvectorResource = nullptr;
        IRHISurface* depthbufferResource = nullptr;
        IRHISurface* reactiveMapResource = nullptr;
        IRHISurface* transparencyAndCompositionResource = nullptr;

        // Output
        IRHISurface* resolvedColorResource = nullptr;

        // Arguments
        int renderWidth = 0;
        int renderHeight = 0;

        bool cameraReset = false;
        float cameraJitterX = 0.f;
        float cameraJitterY = 0.f;

        float sharpness = 0.f;

        float frameTimeDelta = 0.f;

        float nearPlane = 1.f;
        float farPlane = 10.f;
        float fovH = 90.f;
    };

public:
    void Create();
    void Destroy();

    bool GetRenderScale(float& RenderScale, u32 preset, float scale, u32 width, u32 height);
    void Resize(const ContextParameters& Parameters);
    u32 GetOptimalPresetForScale(float scale, u32 preset);
    bool Draw(const DrawParameters& params);

    const Ivector2& GetDisplaySize() const;

    ~DLSSWrapper();

};

extern RHI_API DLSSWrapper g_DLSSWrapper;
