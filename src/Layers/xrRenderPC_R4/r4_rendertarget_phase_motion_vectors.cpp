#include "stdafx.h"

void CRenderTarget::phase_motion_vectors()
{
    PIX_EVENT(MOTION_VECTORS);

    u32 Offset = 0;
    float d_Z = EPS_S;
    float d_W = 1.0f;
    u32 C = color_rgba(0, 0, 0, 255);

    u32 w = RCache.get_width();
    u32 h = RCache.get_height();

    u_setrt(w, h, rt_MotionVectors->pRT, nullptr, nullptr, rt_HWDepth->pZRT);

    u32 CullMode = RCache.get_CullMode();
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(false);

    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(0, h, d_Z, d_W, C, 0, 1); pv++;
    pv->set(0, 0, d_Z, d_W, C, 0, 0); pv++;
    pv->set(w, h, d_Z, d_W, C, 1, 1); pv++;
    pv->set(w, 0, d_Z, d_W, C, 1, 0); pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    RCache.set_Element(s_output_scale->E[1]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}