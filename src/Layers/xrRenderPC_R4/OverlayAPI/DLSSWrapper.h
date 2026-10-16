#pragma once

// AI: NVIDIA's DLSS SDK is Windows-only, and the macOS plan leaves DLSS out. On macOS the vendor
// AI: header is never included and the wrapper keeps the same public surface as no-ops, so the
// AI: renderer code compiles unchanged. The Windows implementation below is untouched.
#if !defined(IXR_MACOS)
#include <ngx/nvsdk_ngx.h>
#include <ngx/nvsdk_ngx_helpers.h>
#endif

class DLSSWrapper
{
public:
    struct ContextParameters
    {
        u32 flags = 0;
        Ivector2 renderSize = { 0, 0 };
        Ivector2 displaySize = { 0, 0 };
        ID3D11Device* device = nullptr;
    };

    struct DrawParameters
    {
        ID3D11DeviceContext* deviceContext = nullptr;

        // Inputs
        ID3D11Texture2D* exposureResource = nullptr;
        ID3D11Texture2D* unresolvedColorResource = nullptr;
        ID3D11Texture2D* motionvectorResource = nullptr;
        ID3D11Texture2D* depthbufferResource = nullptr;
        ID3D11Texture2D* reactiveMapResource = nullptr;
        ID3D11Texture2D* transparencyAndCompositionResource = nullptr;

        // Output
        ID3D11Texture2D* resolvedColorResource = nullptr;

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

    bool GetRenderScale(float& RenderScale);
    void Resize(const ContextParameters& Parameters);
    u32 GetOptimalPresetForScale(float scale);
    bool Draw(const DrawParameters& params);

    const Ivector2& GetDisplaySize() const { return DisplaySize; }

    ~DLSSWrapper();

private:
#if !defined(IXR_MACOS)
    NVSDK_NGX_Parameter* NgxParameters = nullptr;
    NVSDK_NGX_Handle* Handle = nullptr;
#endif

    Ivector2 DisplaySize{};

    bool DLSSInited = false;
    bool Created = false;
};

extern DLSSWrapper g_DLSSWrapper;

#if defined(IXR_MACOS)
// AI: no-op bodies for the macOS build. DLSS is disabled there, so every entry point the renderer
// AI: calls simply reports "not available" instead of touching NVIDIA's SDK.
inline void DLSSWrapper::Create() {}
inline void DLSSWrapper::Destroy() {}
inline bool DLSSWrapper::GetRenderScale(float&) { return false; }
inline void DLSSWrapper::Resize(const ContextParameters&) {}
inline uint32_t DLSSWrapper::GetOptimalPresetForScale(float) { return 0; }
inline bool DLSSWrapper::Draw(const DrawParameters&) { return false; }
inline DLSSWrapper::~DLSSWrapper() {}

inline DLSSWrapper g_DLSSWrapper;
#endif