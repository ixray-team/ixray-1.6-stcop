#pragma once

#include <xess/xess.h>
#include <xess/xess_d3d11.h>

class XeSSWrapper
{
public:
    struct ContextParameters
    {
        ID3D11Device* device = nullptr;
        uint32_t outputWidth = 0;
        uint32_t outputHeight = 0;
        xess_quality_settings_t qualitySetting = XESS_QUALITY_SETTING_QUALITY;
        uint32_t initFlags = XESS_INIT_FLAG_NONE;
        void (*fpMessage)(const char* message) = nullptr;
    };

    struct DrawParameters
    {
        ID3D11DeviceContext* deviceContext = nullptr;

        // Input resources
        ID3D11Resource* pColorTexture = nullptr;
        ID3D11Resource* pVelocityTexture = nullptr;
        ID3D11Resource* pDepthTexture = nullptr;
        ID3D11Resource* pExposureScaleTexture = nullptr;
        ID3D11Resource* pResponsivePixelMaskTexture = nullptr;

        // Output resource
        ID3D11Resource* pOutputTexture = nullptr;

        // Camera parameters
        float jitterOffsetX = 0.0f;
        float jitterOffsetY = 0.0f;
        float exposureScale = 1.0f;
        uint32_t resetHistory = 0;

        // Resolution info
        uint32_t inputWidth = 0;
        uint32_t inputHeight = 0;

        // Camera settings
        float nearPlane = 0.1f;
        float farPlane = 1000.0f;
        float fovH = 60.0f; // Horizontal FOV in degrees
    };

public:
    XeSSWrapper() = default;
    ~XeSSWrapper();

    bool Create(const ContextParameters& params);
    void Destroy();
    bool Draw(const DrawParameters& params);

    bool IsCreated() const { return m_context != nullptr; }

private:
    shared_str XeSSResultToString(xess_result_t result);

    xess_context_handle_t m_context = nullptr;
    ContextParameters m_params;
};

extern XeSSWrapper g_XESSWrapper;