#include "stdafx.h"
#include "XeSSWrapper.h"

XeSSWrapper g_XESSWrapper;

XeSSWrapper::~XeSSWrapper()
{
    Destroy();
}

bool XeSSWrapper::Create(const ContextParameters& params)
{
    if (m_context)
        Destroy();

    // Create XeSS context
    xess_result_t result = xessD3D11CreateContext(params.device, &m_context);
    if (result != XESS_RESULT_SUCCESS)
    {
        if (params.fpMessage)
            params.fpMessage("[XeSS] Failed to create context");

        if (result != XESS_RESULT_ERROR_UNSUPPORTED_DEVICE)
            Msg("! Error XESS initial:%s", *XeSSResultToString(result));

        return false;
    }

    // Initialize XeSS
    xess_d3d11_init_params_t initParams = {};
    initParams.outputResolution = { params.outputWidth, params.outputHeight };
    initParams.qualitySetting = params.qualitySetting;
    initParams.initFlags = params.initFlags;

    result = xessD3D11Init(m_context, &initParams);
    if (result != XESS_RESULT_SUCCESS)
    {
        if (params.fpMessage)
            params.fpMessage("[XeSS] Failed to initialize");

        Msg("! Error XESS initial:%s", *XeSSResultToString(result));

        Destroy();
        return false;
    }

    m_params = params;
    return true;
}

void XeSSWrapper::Destroy()
{
    if (m_context)
    {
        xessDestroyContext(m_context);
        m_context = nullptr;
    }
}

bool XeSSWrapper::Draw(const DrawParameters& params)
{
    if (!m_context)
        return false;

    xess_d3d11_execute_params_t execParams = {};
    execParams.pColorTexture = params.pColorTexture;
    execParams.pVelocityTexture = params.pVelocityTexture;
    execParams.pDepthTexture = params.pDepthTexture;
    execParams.pExposureScaleTexture = params.pExposureScaleTexture;
    execParams.pResponsivePixelMaskTexture = params.pResponsivePixelMaskTexture;
    execParams.pOutputTexture = params.pOutputTexture;

    execParams.jitterOffsetX = params.jitterOffsetX;
    execParams.jitterOffsetY = params.jitterOffsetY;
    execParams.exposureScale = params.exposureScale;
    execParams.resetHistory = params.resetHistory;
    execParams.inputWidth = params.inputWidth;
    execParams.inputHeight = params.inputHeight;

    xess_result_t result = xessD3D11Execute(m_context, &execParams);

    if (result != XESS_RESULT_SUCCESS && result != XESS_RESULT_ERROR_UNSUPPORTED_DEVICE)
    {
        Msg("! Error XESS initial:%s", *XeSSResultToString(result));
    }

    return result == XESS_RESULT_SUCCESS;
}

inline shared_str XeSSWrapper::XeSSResultToString(xess_result_t result)
{
    switch (result)
    {
        case XESS_RESULT_WARNING_NONEXISTING_FOLDER: return " XESS_RESULT_WARNING_NONEXISTING_FOLDER";
        case XESS_RESULT_WARNING_OLD_DRIVER: return "XESS_RESULT_WARNING_OLD_DRIVER";
        case XESS_RESULT_SUCCESS: return "XESS_RESULT_SUCCESS";
        case XESS_RESULT_ERROR_UNSUPPORTED_DEVICE: return "XESS_RESULT_ERROR_UNSUPPORTED_DEVICE";
        case XESS_RESULT_ERROR_UNSUPPORTED_DRIVER: return "XESS_RESULT_ERROR_UNSUPPORTED_DRIVER";
        case XESS_RESULT_ERROR_UNINITIALIZED: return "XESS_RESULT_ERROR_UNINITIALIZED";
        case XESS_RESULT_ERROR_INVALID_ARGUMENT: return "XESS_RESULT_ERROR_INVALID_ARGUMENT";
        case XESS_RESULT_ERROR_DEVICE_OUT_OF_MEMORY: return "XESS_RESULT_ERROR_DEVICE_OUT_OF_MEMORY";
        case XESS_RESULT_ERROR_DEVICE: return "XESS_RESULT_ERROR_DEVICE";
        case XESS_RESULT_ERROR_NOT_IMPLEMENTED: return "XESS_RESULT_ERROR_NOT_IMPLEMENTED";
        case XESS_RESULT_ERROR_INVALID_CONTEXT: return "XESS_RESULT_ERROR_INVALID_CONTEXT";
        case XESS_RESULT_ERROR_OPERATION_IN_PROGRESS: return "XESS_RESULT_ERROR_OPERATION_IN_PROGRESS";
        case XESS_RESULT_ERROR_UNSUPPORTED: return "XESS_RESULT_ERROR_UNSUPPORTED";
        case XESS_RESULT_ERROR_CANT_LOAD_LIBRARY: return "XESS_RESULT_ERROR_CANT_LOAD_LIBRARY";
        case XESS_RESULT_ERROR_WRONG_CALL_ORDER: return "XESS_RESULT_ERROR_WRONG_CALL_ORDER";
        case XESS_RESULT_ERROR_UNKNOWN: return "XESS_RESULT_ERROR_UNKNOWN";
        default: return "Unknown error code";
    }
}