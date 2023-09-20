#include "stdafx.h"

extern float g_CameraJitterX;
extern float g_CameraJitterY; 
extern float ps_r4_sharp_factor;
extern int ps_r4_sharp_enable;

void set_viewport(ID3DDeviceContext* dev, float w, float h);

void  CRenderTarget::phase_copy_depth()
{
    PIX_EVENT(Copy_Depth);

    u32 Offset = 0;
    float d_Z = EPS_S;
    float d_W = 1.0f;
    u32 C = color_rgba(0, 0, 0, 255);

    u32 w = RCache.get_target_width();
    u32 h = RCache.get_target_height();

    set_viewport(HW.pContext, RCache.get_target_width(), RCache.get_target_height());
    u_setrt(w, h, rt_Depth->pRT, nullptr, nullptr, nullptr);

    u32 CullMode = RCache.get_CullMode();
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(false);

    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(0, h, d_Z, d_W, C, 0, 1); pv++;
    pv->set(0, 0, d_Z, d_W, C, 0, 0); pv++;
    pv->set(w, h, d_Z, d_W, C, 1, 1); pv++;
    pv->set(w, 0, d_Z, d_W, C, 1, 0); pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    RCache.set_Element(s_output_scale->E[SCALEPHASE_COPY_DEPTH]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}

void CRenderTarget::phase_fsr2_combine()
{
    PIX_EVENT(FSR2);

    phase_copy_depth();

    Fsr2Wrapper::DrawParameters fsr2Params;
    fsr2Params.deviceContext = HW.pContext;
    fsr2Params.exposureResource = nullptr;
    fsr2Params.unresolvedColorResource = rt_Target->pSurface;
    fsr2Params.motionvectorResource = rt_MotionVectors->pSurface;
    fsr2Params.depthbufferResource = rt_Depth->pSurface;
    fsr2Params.reactiveMapResource = nullptr;
    fsr2Params.transparencyAndCompositionResource = nullptr;
    fsr2Params.resolvedColorResource = rt_UpscaleOutput->pSurface;
    fsr2Params.renderWidth = RCache.get_width();
    fsr2Params.renderHeight = RCache.get_height();
    fsr2Params.cameraReset = false;
    fsr2Params.cameraJitterX = g_CameraJitterX;
    fsr2Params.cameraJitterY = g_CameraJitterY;
    fsr2Params.enableSharpening = !!ps_r4_sharp_enable;
    fsr2Params.sharpness = ps_r4_sharp_factor;
    fsr2Params.frameTimeDelta = Device.fTrueTimeDelta * 1000.0f;
    fsr2Params.nearPlane = VIEWPORT_NEAR;
    fsr2Params.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
    fsr2Params.fovH = deg2rad(Device.fFOV);

    g_Fsr2Wrapper.Draw(fsr2Params);
    HW.pContext->CopyResource(rt_Output->pSurface, rt_UpscaleOutput->pSurface);
}