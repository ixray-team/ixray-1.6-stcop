#pragma once

#include <nvsdk_ngx.h>
#include <nvsdk_ngx_helpers.h>

class DLSSWrapper
{
public:
    struct ContextParameters
    {
        uint32_t flags = 0;
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
    void Create(const ContextParameters& Parameters);
    void Destroy();

    void Draw(const DrawParameters& params);

    const Ivector2& GetDisplaySize() const { return DisplaySize; }

    ~DLSSWrapper();

private:
    NVSDK_NGX_Parameter* NgxParameters = nullptr;
    NVSDK_NGX_Handle* Handle = nullptr;
    Ivector2 DisplaySize;
    bool m_created = false;
};

extern DLSSWrapper g_DLSSWrapper;