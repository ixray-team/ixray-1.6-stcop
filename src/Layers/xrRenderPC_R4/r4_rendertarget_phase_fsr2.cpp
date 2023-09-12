#include "stdafx.h"

extern Fvector2 g_current_jitter;
extern Fvector2 g_prev_jitter;

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

    RCache.set_Element(s_output_scale->E[2]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}

void CRenderTarget::phase_fsr2_combine()
{
    PIX_EVENT(FSR2);

    phase_copy_depth();

    Fsr2Wrapper::DrawParameters fsr2Params = {
        HW.pContext,
        rt_Generic_0->pSurface,
        rt_MotionVectors->pSurface,
        rt_Depth->pSurface,
        nullptr,
        nullptr,
        rt_UpscaleOutput->pSurface,
        RCache.get_width(),
        RCache.get_height(),
        false,
        g_current_jitter.x,
        g_current_jitter.y,
        0.5,
        0.5,
        Device.fTimeDelta * 1000.0f,
        0.02f,
        0.99999f,
        Device.fFOV,
    };

    g_Fsr2Wrapper.Draw(fsr2Params);
    HW.pContext->CopyResource(rt_Output->pSurface, rt_UpscaleOutput->pSurface);
}