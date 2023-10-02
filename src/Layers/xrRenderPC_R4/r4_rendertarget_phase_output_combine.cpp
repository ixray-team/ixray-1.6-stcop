#include "stdafx.h"

extern void set_viewport(ID3DDeviceContext* dev, float w, float h);

void CRenderTarget::phase_output_scale(bool linear)
{
    PIX_EVENT(OUTPUT_UPSCALE);

    u32 Offset = 0;
    float d_Z = EPS_S;
    float d_W = 1.0f;
    u32 C = color_rgba(0, 0, 0, 255);

    u32 w = RCache.get_target_width();
    u32 h = RCache.get_target_height();

	u_setrt(w, h, rt_Output->pRT, nullptr, nullptr, rt_HWDepth->pZRT);

    set_viewport(HW.pContext, RCache.get_target_width(), RCache.get_target_height());
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(false);

    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(0, h, d_Z, d_W, C, 0, 1); pv++;
    pv->set(0, 0, d_Z, d_W, C, 0, 0); pv++;
    pv->set(w, h, d_Z, d_W, C, 1, 1); pv++;
    pv->set(w, 0, d_Z, d_W, C, 1, 0); pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    RCache.set_Element(s_output_scale->E[linear ? SCALEPHASE_SCALE_LINEAR : SCALEPHASE_SCALE_NEAREST]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}

void CRenderTarget::phase_depth_scale()
{
    PIX_EVENT(DEPTH_UPSCALE);

    u32 Offset = 0;
    float d_Z = EPS_S;
    float d_W = 1.0f;

    u32 w = RCache.get_target_width();
    u32 h = RCache.get_target_height();
    u_setrt(w, h, rt_HWCopyDepth->pRT, nullptr, nullptr, nullptr);

    set_viewport(HW.pContext, RCache.get_target_width(), RCache.get_target_height());
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(false);

    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(0, h, d_Z, d_W, 0, 0, 1); pv++;
    pv->set(0, 0, d_Z, d_W, 0, 0, 0); pv++;
    pv->set(w, h, d_Z, d_W, 0, 1, 1); pv++;
    pv->set(w, 0, d_Z, d_W, 0, 1, 0); pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    RCache.set_Element(s_output_scale->E[SCALEPHASE_SCALE_DEPTH]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

    HW.pContext->CopyResource(rt_HWScaledTargetDepth->pSurface, rt_HWCopyDepth->pSurface);
}
