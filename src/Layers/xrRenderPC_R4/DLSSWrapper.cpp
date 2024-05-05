#include "stdafx.h"

#include "DLSSWrapper.h"

DLSSWrapper g_DLSSWrapper;

bool DLSSInited = false;

void DLSSWrapper::Create(const ContextParameters& Parameters)
{
    if (m_created) {
        Destroy();
    }
    
    if (RFeatureLevel < D3D_FEATURE_LEVEL_11_1) {
        return;
    }

#ifdef IXR_X64
    NVSDK_NGX_Result result;
    if (!DLSSInited) {
        result = NVSDK_NGX_D3D11_Init(1602, L"", RDevice);
        if (result != NVSDK_NGX_Result_Success) {
            return;
        }

        DLSSInited = true;
    }

    result = NVSDK_NGX_D3D11_GetCapabilityParameters(&NgxParameters);
    if (result != NVSDK_NGX_Result_Success) {
        return;
    }

    uint32_t needsUpdatedDriver = 1;
    result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_NeedsUpdatedDriver, &needsUpdatedDriver);
    if (needsUpdatedDriver) {
        Msg("! PLEASE UPDATE YOUR DRIVER");
    }

    uint32_t dlssAvailable = 0;
    result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_Available, &dlssAvailable);
    if (!dlssAvailable) {
        NVSDK_NGX_D3D11_DestroyParameters(NgxParameters);
        return;
    }

    int32_t flags = 0;
    flags |= NVSDK_NGX_DLSS_Feature_Flags_MVLowRes;
    flags |= NVSDK_NGX_DLSS_Feature_Flags_IsHDR;
    //flags |= NVSDK_NGX_DLSS_Feature_Flags_DoSharpening;

    NVSDK_NGX_DLSS_Create_Params dlssCreateParams = {};
    dlssCreateParams.Feature.InWidth = Parameters.renderSize.x;
    dlssCreateParams.Feature.InHeight = Parameters.renderSize.y;
    dlssCreateParams.Feature.InTargetWidth = Parameters.displaySize.x;
    dlssCreateParams.Feature.InTargetHeight = Parameters.displaySize.y;
    dlssCreateParams.InFeatureCreateFlags = flags;
    result = NGX_D3D11_CREATE_DLSS_EXT(RContext, &Handle, NgxParameters, &dlssCreateParams);
    if (result != NVSDK_NGX_Result_Success) {
        return;
    }

    m_created = true;
#endif // WIN32
}

void DLSSWrapper::Destroy() 
{
#ifdef IXR_X64
    //if (!m_created) {
    //    return;
    //}

    if (Handle != nullptr) {
        NVSDK_NGX_D3D11_ReleaseFeature(Handle);
        Handle = nullptr;
    }

    if (NgxParameters != nullptr) {
        NVSDK_NGX_D3D11_DestroyParameters(NgxParameters);
        NgxParameters = nullptr;
    }

    if (DLSSInited) {
        NVSDK_NGX_D3D11_Shutdown1(nullptr);
        DLSSInited = false;
    }

    m_created = false;
#endif // IXR_X64
}

void DLSSWrapper::Draw(const DrawParameters& params)
{
#ifdef IXR_X64
    if (!m_created) {
        return;
    }

    ID3D11Resource* resourceInput = params.unresolvedColorResource;
    ID3D11Resource* resourceMv = params.motionvectorResource;
    ID3D11Resource* resourceDepth = params.depthbufferResource;
    ID3D11Resource* resourceOutput = params.resolvedColorResource;

    NVSDK_NGX_D3D11_DLSS_Eval_Params dlssEvalParams = {};
    dlssEvalParams.Feature.pInColor = resourceInput;
    dlssEvalParams.Feature.pInOutput = resourceOutput;
    //dlssEvalParams.Feature.InSharpness = 0.5f;
    dlssEvalParams.pInDepth = resourceDepth;
    dlssEvalParams.pInMotionVectors = resourceMv;
    dlssEvalParams.InRenderSubrectDimensions.Width = params.renderWidth;
    dlssEvalParams.InRenderSubrectDimensions.Height = params.renderHeight;
    dlssEvalParams.InJitterOffsetX = params.cameraJitterX;
    dlssEvalParams.InJitterOffsetY = params.cameraJitterY;
    dlssEvalParams.InReset = params.cameraReset;
    dlssEvalParams.InMVScaleX = -(float)params.renderWidth * 0.5f;    // adjust the x direction in motion vector to fit FSR2's requirement
    dlssEvalParams.InMVScaleY = (float)params.renderHeight * 0.5f;
    dlssEvalParams.pInTransparencyMask = params.transparencyAndCompositionResource;

    NVSDK_NGX_Result result = NGX_D3D11_EVALUATE_DLSS_EXT(RContext, Handle, NgxParameters, &dlssEvalParams);
    VERIFY(result == NVSDK_NGX_Result_Success);
#endif
}

DLSSWrapper::~DLSSWrapper()
{
    Destroy();
}
