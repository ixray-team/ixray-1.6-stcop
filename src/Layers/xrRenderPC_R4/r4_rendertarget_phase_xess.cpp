#include "stdafx.h"
#include "OverlayAPI/XESSWrapper.h"
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

void CRenderTarget::init_xess()
{
    g_XESSWrapper.Destroy();
    if (ps_r_scale_mode != 4)
    {
        return;
    }
    XeSSWrapper::ContextParameters params;
    params.outputWidth = (u32)RCache.get_target_width();
    params.outputHeight = (u32)RCache.get_target_height();
    params.preset = ps_render_scale_preset;
    params.scale = ps_render_scale;
    g_XESSWrapper.Create(params);
}

bool CRenderTarget::phase_xess()
{
    GPU_EVENT(XESS);
    XeSSWrapper::DrawParameters params;
    params.pColorTexture = rt_Generic_0->pSurface;
    params.pVelocityTexture = rt_Velocity->pSurface;
    params.pDepthTexture = rt_Position->pSurface;
    params.pOutputTexture = rt_Generic->pSurface;
    params.inputWidth = (u32)RCache.get_width();
    params.inputHeight = (u32)RCache.get_height();
    params.jitterOffsetX = ps_r_taa_jitter_full.x;
    params.jitterOffsetY = ps_r_taa_jitter_full.y;
    return g_XESSWrapper.Draw(params);
}
