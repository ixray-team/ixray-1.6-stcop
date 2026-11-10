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
        u32 model = 0;
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

#if !defined(IXR_WINDOWS)
inline void DLSSWrapper::Create() {}
inline void DLSSWrapper::Destroy() {}
inline bool DLSSWrapper::GetRenderScale(float&, u32, float, u32, u32) { return false; }
inline void DLSSWrapper::Resize(const ContextParameters&) {}
inline u32 DLSSWrapper::GetOptimalPresetForScale(float, u32) { return 0; }
inline bool DLSSWrapper::Draw(const DrawParameters&) { return false; }
inline const Ivector2& DLSSWrapper::GetDisplaySize() const { static const Ivector2 Size{}; return Size; }
inline DLSSWrapper::~DLSSWrapper() {}

inline DLSSWrapper g_DLSSWrapper;
#endif
