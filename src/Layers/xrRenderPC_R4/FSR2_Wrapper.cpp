#include "stdafx.h"

Fsr2Wrapper g_Fsr2Wrapper;

extern float ps_r4_motion_scale;

bool Fsr2Wrapper::Create(Fsr2Wrapper::ContextParameters params)
{
    VERIFY(!m_created);

    m_contextParams = params;

    // Setup DX11 interface.
    m_scratchBuffer.resize(ffxFsr2GetScratchMemorySizeDX11());
    FfxErrorCode errorCode = ffxFsr2GetInterfaceDX11(&m_contextDesc.callbacks, params.device, m_scratchBuffer.data(), m_scratchBuffer.size());
    if (errorCode != FFX_OK) {
        return false;
    }

    // This adds a ref to the device. The reference will get freed in ffxFsr2ContextDestroy
    m_contextDesc.device = ffxGetDeviceDX11(params.device);
    m_contextDesc.maxRenderSize = params.maxRenderSize;
    m_contextDesc.fpMessage = params.fpMessage;
    m_contextDesc.displaySize = params.displaySize;

    // You should config the flags you need based on your own project
    m_contextDesc.flags =
#ifdef DEBUG
        FFX_FSR2_ENABLE_DEBUG_CHECKING |
#endif
        FFX_FSR2_ENABLE_HIGH_DYNAMIC_RANGE |
        FFX_FSR2_ENABLE_AUTO_EXPOSURE;
    //| FFX_FSR2_ENABLE_HIGH_DYNAMIC_RANGE
    //| FFX_FSR2_ENABLE_DEPTH_INVERTED
    //| FFX_FSR2_ENABLE_AUTO_EXPOSURE;

#if COMPILE_FROM_HLSL
    // Override the shader creation so we can compile from HLSL source.
    FfxFsr2Interface d3dInterface = {};
    errorCode = ffxFsr2GetInterfaceDX12(&d3dInterface, m_d3d11Device->GetDevice(), scratchBuffer, scratchBufferSize);
    initializationParameters.callbacks.fpCreateRenderPass = s_bUseHLSLShaders ? createRenderPassFromSource : d3dInterface.fpCreateRenderPass;

    // copy for use by on-demand shader compile from HLSL
    memcpy(&s_initializationParameters, &initializationParameters, sizeof(FfxFsr2ContextDescription));
#endif // #if COMPILE_FROM_HLSL

    ffxFsr2ContextCreate(&m_context, &m_contextDesc);

    m_created = true;
}

void Fsr2Wrapper::Destroy()
{
    VERIFY(m_created);

    ffxFsr2ContextDestroy(&m_context);

    m_created = false;
}

void Fsr2Wrapper::Draw(const DrawParameters& params)
{
    R_ASSERT(m_created);

    FfxFsr2DispatchDescription dispatchParameters = {};
    dispatchParameters.commandList = params.deviceContext;
    dispatchParameters.color = ffxGetResourceDX11(&m_context, params.unresolvedColorResource, L"FSR2_InputColor");
    dispatchParameters.depth = ffxGetResourceDX11(&m_context, params.depthbufferResource, L"FSR2_InputDepth");
    dispatchParameters.motionVectors = ffxGetResourceDX11(&m_context, params.motionvectorResource, L"FSR2_InputMotionVectors");
    dispatchParameters.exposure = ffxGetResourceDX11(&m_context, params.exposureResource, L"FSR2_InputExposure");
    dispatchParameters.reactive = ffxGetResourceDX11(&m_context, params.reactiveMapResource, L"FSR2_InputReactiveMap");
    dispatchParameters.transparencyAndComposition = ffxGetResourceDX11(&m_context, params.transparencyAndCompositionResource, L"FSR2_TransparencyAndCompositionMap");
    dispatchParameters.output = ffxGetResourceDX11(&m_context, params.resolvedColorResource, L"FSR2_OutputUpscaledColor", FFX_RESOURCE_STATE_UNORDERED_ACCESS);
    dispatchParameters.jitterOffset.x = params.cameraJitterX;
    dispatchParameters.jitterOffset.y = params.cameraJitterY;
    dispatchParameters.motionVectorScale.x = -(float)params.renderWidth * ps_r4_motion_scale;    // adjust the x direction in motion vector to fit FSR2's requirement
    dispatchParameters.motionVectorScale.y = (float)params.renderHeight * ps_r4_motion_scale;
    dispatchParameters.reset = params.cameraReset;
    dispatchParameters.enableSharpening = params.enableSharpening;
    dispatchParameters.sharpness = params.sharpness;
    dispatchParameters.frameTimeDelta = params.frameTimeDelta;
    dispatchParameters.preExposure = 1.0f;
    dispatchParameters.renderSize.width = params.renderWidth;
    dispatchParameters.renderSize.height = params.renderHeight;
    dispatchParameters.cameraFar = params.farPlane;
    dispatchParameters.cameraNear = params.nearPlane;;
    dispatchParameters.cameraFovAngleVertical = params.fovH;
    dispatchParameters.viewSpaceToMetersFactor = 1.0f;
    dispatchParameters.autoTcThreshold = 1.0f;
    dispatchParameters.autoTcScale = 1.0f;
    dispatchParameters.autoReactiveScale = 1.0f;
    dispatchParameters.autoReactiveMax = 1.0f;

    // EXPERIMENTAL feature, auto-generate reactive mask
    // Turn it off if you don't need it
    dispatchParameters.enableAutoReactive = true;
    dispatchParameters.colorOpaqueOnly = ffxGetResourceDX11(&m_context, params.unresolvedColorResource, L"FSR2_InputColor");

#if COMPILE_FROM_HLSL
    memcpy(&s_initializationParameters, &initializationParameters, sizeof(FfxFsr2ContextDescriptor));
#endif // #if COMPILE_FROM_HLSL

    FfxErrorCode errorCode = ffxFsr2ContextDispatch(&m_context, &dispatchParameters);
    R_ASSERT(errorCode == FFX_OK);
}

Fsr2Wrapper::~Fsr2Wrapper()
{
    if (IsCreated())
        Destroy();
}
