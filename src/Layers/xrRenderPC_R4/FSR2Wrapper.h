#pragma once
#include <ffx-fsr2-api/ffx_fsr2.h>
#include <ffx-fsr2-api/dx11/ffx_fsr2_dx11.h>

class Fsr2Wrapper
{
public:
    struct ContextParameters
    {
        uint32_t        flags = 0;
        FfxDimensions2D maxRenderSize = { 0, 0 };
        FfxDimensions2D displaySize = { 0, 0 };
        FfxFsr2Message fpMessage;
        ID3D11Device* device = nullptr;
    };

    struct DrawParameters
    {
        ID3D11DeviceContext* deviceContext = nullptr;

        // Inputs
        ID3DResource* unresolvedColorResource = nullptr;
        ID3DResource* unresolvedOpaqueResource = nullptr;

        ID3DResource* motionvectorResource = nullptr;
        ID3DResource* depthbufferResource = nullptr;
        ID3DResource* reactiveMapResource = nullptr;
        ID3DResource* transparencyAndCompositionResource = nullptr;

        // Output
        ID3DResource* resolvedColorResource = nullptr;

        // Arguments
        uint32_t renderWidth = 0;
        uint32_t renderHeight = 0;

        bool cameraReset = false;
        float cameraJitterX = 0.f;
        float cameraJitterY = 0.f;

        bool enableSharpening = true;
        float sharpness = 0.f;

        float frameTimeDelta = 0.f;

        float nearPlane = 1.f;
        float farPlane = 10.f;
        float fovH = 90.f;
    };

public:
    void Create(ContextParameters params);
    void Destroy();

    void Draw(const DrawParameters& params);

    bool IsCreated() const { return m_created; }
    FfxDimensions2D GetDisplaySize() const { return m_contextDesc.displaySize; }

    ~Fsr2Wrapper();

private:
    bool m_created = false;

    FfxFsr2Context m_context;
    FfxFsr2ContextDescription m_contextDesc;
    ContextParameters m_contextParams;

    xr_vector<char> m_scratchBuffer;
};

extern Fsr2Wrapper g_Fsr2Wrapper;