#include "stdafx.h"

extern int ps_r4_sharp_enable;
extern float ps_r4_motion_scale;
DLSSWrapper g_DLSSWrapper;

bool DLSSWrapper::Create(const ContextParameters& Parameters)
{
    VERIFY(!m_created);

    NVSDK_NGX_Result result = NVSDK_NGX_D3D11_Init(1602, L"", HW.pDevice);
    if (result != NVSDK_NGX_Result_Success) {
        return false;
    }

    result = NVSDK_NGX_D3D11_GetCapabilityParameters(&NgxParameters);
    if (result != NVSDK_NGX_Result_Success) {
        return false;
    }

    uint32_t needsUpdatedDriver = 1;
    result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_NeedsUpdatedDriver, &needsUpdatedDriver);
    if (needsUpdatedDriver) {
        MessageBoxA(NULL, "PLEASE UPDATE YOUR FUCKING DRIVER!!!", "ERROR INVALID", MB_OK | MB_ICONWARNING);
    }

    uint32_t dlssAvailable = 0;
    result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_Available, &dlssAvailable);
    if (!dlssAvailable) {
        NVSDK_NGX_D3D11_DestroyParameters(NgxParameters);
        return false;
    }

    int32_t flags = 0;
    if (ps_r4_sharp_enable) {
        flags |= NVSDK_NGX_DLSS_Feature_Flags_DoSharpening;
    }

    flags |= NVSDK_NGX_DLSS_Feature_Flags_MVLowRes;
    flags |= NVSDK_NGX_DLSS_Feature_Flags_IsHDR;
    flags |= NVSDK_NGX_DLSS_Feature_Flags_AutoExposure;

    NVSDK_NGX_DLSS_Create_Params dlssCreateParams = {};
    dlssCreateParams.Feature.InWidth = Parameters.renderSize.x;
    dlssCreateParams.Feature.InHeight = Parameters.renderSize.y;
    dlssCreateParams.Feature.InTargetWidth = Parameters.displaySize.x;
    dlssCreateParams.Feature.InTargetHeight = Parameters.displaySize.y;
    dlssCreateParams.InFeatureCreateFlags = flags;
    result = NGX_D3D11_CREATE_DLSS_EXT(HW.pContext, &Handle, NgxParameters, &dlssCreateParams);
    if (result != NVSDK_NGX_Result_Success) {
        return false;
    }

    return true;
}

void DLSSWrapper::Destroy()
{
}

void DLSSWrapper::Draw(const DrawParameters& params)
{
    ID3D11Resource* resourceInput = params.unresolvedColorResource;
    ID3D11Resource* resourceMv = params.motionvectorResource;
    ID3D11Resource* resourceDepth = params.depthbufferResource;
    ID3D11Resource* resourceOutput = params.resolvedColorResource;

    NVSDK_NGX_D3D11_DLSS_Eval_Params dlssEvalParams = {};
    dlssEvalParams.Feature.pInColor = resourceInput;
    dlssEvalParams.Feature.pInOutput = resourceOutput;
    dlssEvalParams.Feature.InSharpness = params.sharpness;
    dlssEvalParams.pInDepth = resourceDepth;
    dlssEvalParams.pInMotionVectors = resourceMv;
    dlssEvalParams.InRenderSubrectDimensions.Width = params.renderWidth;
    dlssEvalParams.InRenderSubrectDimensions.Height = params.renderHeight;
    dlssEvalParams.InJitterOffsetX = params.cameraJitterX;
    dlssEvalParams.InJitterOffsetY = params.cameraJitterY;
    dlssEvalParams.InReset = params.cameraReset;
    dlssEvalParams.InMVScaleX = -(float)params.renderWidth * ps_r4_motion_scale;    // adjust the x direction in motion vector to fit FSR2's requirement
    dlssEvalParams.InMVScaleY = (float)params.renderHeight * ps_r4_motion_scale;
    dlssEvalParams.pInTransparencyMask = params.transparencyAndCompositionResource;

    NVSDK_NGX_Result result = NGX_D3D11_EVALUATE_DLSS_EXT(HW.pContext, Handle, NgxParameters, &dlssEvalParams);
    VERIFY(result == NVSDK_NGX_Result_Success);
}

DLSSWrapper::~DLSSWrapper()
{
}
