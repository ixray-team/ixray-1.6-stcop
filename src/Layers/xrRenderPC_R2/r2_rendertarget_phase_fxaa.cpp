#include "stdafx.h"

#include "r2_rendertarget.h"

void CRenderTarget::phase_fxaa(u32 pass) {
    u32 Offset = 0;
    float _w = float(RCache.get_width());
    float _h = float(RCache.get_height());
    float ddw = 1.0f / _w;
    float ddh = 1.0f / _h;

    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

    FVF::V* pv = (FVF::V*)RCache.Vertex.Lock(4, g_fxaa->vb_stride, Offset);
    pv->set(ddw - 0.5f, ddh + _h - 0.5f, 0.0f, 0.0f, 1.0f);
    pv++;
    pv->set(ddw - 0.5f, ddh - 0.5f, 0.0f, 0.0f, 0.0f);
    pv++;
    pv->set(ddw + _w - 0.5f, ddh + _h - 0.5f, 0.0f, 1.0f, 1.0f);
    pv++;
    pv->set(ddw + _w - 0.5f, ddh - 0.5f, 0.0f, 1.0f, 0.0f);
    pv++;
    RCache.Vertex.Unlock(4, g_fxaa->vb_stride);

    RCache.set_Element(s_fxaa->E[pass]);
    RCache.set_Geometry(g_fxaa);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}
