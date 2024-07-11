#include "stdafx.h"

void CRenderTarget::phase_fxaa()
{
    u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);

    u32 Offset = 0;
    Fvector2 p0, p1;

    auto C = color_rgba(255, 255, 255, 255);
    float d_Z = EPS_S;
    float d_W = 1.f;

    float _w = RCache.get_width();
    float _h = RCache.get_height();
    float ddw = 1.f / _w;
    float ddh = 1.f / _h;

    p0.set(.5f / _w, .5f / _h);
    p1.set((_w + .5f) / _w, (_h + .5f) / _h);

    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);

    pv->set(EPS, float(_h + EPS), d_Z, d_W, C, p0.x, p1.y);	pv++;
    pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);	pv++;
    pv->set(float(_w + EPS), float(_h + EPS), d_Z, d_W, C, p1.x, p1.y);	pv++;
    pv->set(float(_w + EPS), EPS, d_Z, d_W, C, p1.x, p0.y);	pv++;

    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    // Set pass
    RCache.set_Element(s_fxaa->E[0]);

    // Set geometry
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

    // Resolve RT
    RContext->CopyResource(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
}
