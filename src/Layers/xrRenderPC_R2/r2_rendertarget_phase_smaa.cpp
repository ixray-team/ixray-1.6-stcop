#include "stdafx.h"

void CRenderTarget::phase_smaa() {
    u32 Offset;
    Fvector2 p0, p1;
    float d_Z = EPS_S;
    float d_W = 1.0f;
    constexpr u32 C = color_rgba(0, 0, 0, 255);
    FLOAT ColorRGBA[4] = { 0.0f, 0.0f, 0.0f, 0.0f };

    float _w = float(Device.TargetWidth);
    float _h = float(Device.TargetHeight);

    p0.set(.5f / _w, .5f / _h);
    p1.set((_w + .5f) / _w, (_h + .5f) / _h);

    // Phase 0: edge detection ////////////////////////////////////////////////
    u_setrt(rt_smaa_edgetex, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    //RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
    CHK_DX(HW.pDevice->Clear(0, 0, D3DCLEAR_TARGET, color_rgba(0, 0, 0, 0), 1, 0));
    //HW.pDevice->Clear(RCache.get_RT(), ColorRGBA);

    // Fill vertex buffer
    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(EPS, _h + EPS, d_Z, d_W, C, p0.x, p1.y);
    pv++;
    pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);
    pv++;
    pv->set(_w + EPS, _h + EPS, d_Z, d_W, C, p1.x, p1.y);
    pv++;
    pv->set(_w + EPS, EPS, d_Z, d_W, C, p1.x, p0.y);
    pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    // Draw COLOR
    RCache.set_Element(s_smaa->E[0]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

    // Phase 1: blend weights calculation ////////////////////////////////////
    u_setrt(rt_smaa_blendtex, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(TRUE, D3DCMP_EQUAL, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);

    CHK_DX(HW.pDevice->Clear(0, 0, D3DCLEAR_TARGET, color_rgba(0, 0, 0, 0), 1, 0));

    // Fill vertex buffer
    pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(EPS, _h + EPS, d_Z, d_W, C, p0.x, p1.y);
    pv++;
    pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);
    pv++;
    pv->set(_w + EPS, _h + EPS, d_Z, d_W, C, p1.x, p1.y);
    pv++;
    pv->set(_w + EPS, EPS, d_Z, d_W, C, p1.x, p0.y);
    pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    // Draw COLOR
    RCache.set_Element(s_smaa->E[1]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

    // Phase 2: neighbour blend //////////////////////////////////////////////
    u_setrt(rt_Color, nullptr, nullptr, nullptr);

    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

    // Fill vertex buffer
    pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(EPS, _h + EPS, d_Z, d_W, C, p0.x, p1.y);
    pv++;
    pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);
    pv++;
    pv->set(_w + EPS, _h + EPS, d_Z, d_W, C, p1.x, p1.y);
    pv++;
    pv->set(_w + EPS, EPS, d_Z, d_W, C, p1.x, p0.y);
    pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    // Draw COLOR
    RCache.set_Element(s_smaa->E[2]);
    RCache.set_Geometry(g_combine);
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

    // Resolve RT
    D3DXLoadSurfaceFromSurface(rt_Generic_0->pRT, 0, 0, rt_Color->pRT, 0, 0, D3DX_DEFAULT, 0);
}
