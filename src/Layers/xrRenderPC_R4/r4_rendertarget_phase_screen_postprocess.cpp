#include "stdafx.h"

void CRenderTarget::RenderEffect(ScreenPostProcessType postProcessType) {
    u32 Offset = 0;
    float d_Z = EPS_S;
    float d_W = 1.0f;
    constexpr u32 color = color_rgba(0, 0, 0, 255);

    // Set render target
    u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);

    // Configure rendering settings
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(false);

    // Lock and set vertices
    FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
    pv->set(0, Device.TargetHeight, d_Z, d_W, color, 0, 1); pv++;
    pv->set(0, 0, d_Z, d_W, color, 0, 0); pv++;
    pv->set(Device.TargetWidth, Device.TargetHeight, d_Z, d_W, color, 1, 1); pv++;
    pv->set(Device.TargetWidth, 0, d_Z, d_W, color, 1, 0); pv++;
    RCache.Vertex.Unlock(4, g_combine->vb_stride);

    // Set shader and geometry
    RCache.set_Element(s_spp->E[postProcessType]);
    RCache.set_Geometry(g_combine);

    // Render
    RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

    // Copy resource
    RContext->CopyResource(rt_Back_Buffer->pTexture->surface_get(), rt_Back_Buffer_AA->pTexture->surface_get());
}

void CRenderTarget::PhaseAberration() {
    RenderEffect(ScreenPostProcessType::Aberration);
}

void CRenderTarget::PhaseVignette() {
    RenderEffect(ScreenPostProcessType::Vignette);
}
