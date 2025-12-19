#include "stdafx.h"

void CRenderTarget::phase_smaa()
{
    // Phase 0: edge detection ////////////////////////////////////////////////
    u_setrt(rt_smaa_edgetex, nullptr, nullptr, nullptr);

    GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
    RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
    GRHI->ClearTarget(RCache.get_RT());

    // Draw COLOR
    RCache.set_Element(s_smaa->E[0]);
    RCache.set_Geometry(FSTriangleGeom);
    RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

    // Phase 1: blend weights calculation ////////////////////////////////////
    u_setrt(rt_smaa_blendtex, nullptr, nullptr, nullptr);

    GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
    RCache.set_Stencil(TRUE, D3DCMP_EQUAL, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
    GRHI->ClearTarget(RCache.get_RT());

    // Draw COLOR
    RCache.set_Element(s_smaa->E[1]);
    RCache.set_Geometry(FSTriangleGeom);
    RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

    // Phase 2: neighbour blend //////////////////////////////////////////////
    u_setrt(rt_Generic_2, nullptr, nullptr, nullptr);

    GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
    RCache.set_Stencil(FALSE);

    // Draw COLOR
    RCache.set_Element(s_smaa->E[2]);
    RCache.set_Geometry(FSTriangleGeom);
    RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

    // Resolve RT
    GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
}