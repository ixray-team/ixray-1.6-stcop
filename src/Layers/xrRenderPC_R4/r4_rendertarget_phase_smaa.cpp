#include "stdafx.h"

void CRenderTarget::phase_smaa()
{
    u32 Offset;
    Fvector2 p0, p1;
    float d_Z = EPS_S;
    float d_W = 1.0f;
    u32 C = color_rgba(0, 0, 0, 255);
    FLOAT ColorRGBA[4] = { 0.0f, 0.0f, 0.0f, 0.0f };

    float _w = float(Device.dwWidth);
    float _h = float(Device.dwHeight);

    p0.set(0.0f, 0.0f);
    p1.set(1.0f, 1.0f);

    // Phase 0: edge detection ////////////////////////////////////////////////
    u_setrt(rt_smaa_edgetex, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
    HW.pContext->ClearRenderTargetView(RCache.get_RT(), ColorRGBA);

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

    HW.pContext->ClearRenderTargetView(RCache.get_RT(), ColorRGBA);

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
    u_setrt(rt_Back_Buffer, nullptr, nullptr, nullptr);

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
    ref_rt& dest_rt = rt_Color;
    HW.pContext->CopyResource(dest_rt->pTexture->surface_get(), rt_Back_Buffer->pTexture->surface_get());
}