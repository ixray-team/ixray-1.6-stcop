// r4_rendertarget_phase_new_dof.cpp
#include "stdafx.h"
#include "r4_rendertarget.h"

static D3D_VIEWPORT VP_DOF = {
    0.0f,
    0.0f,
    1.0f,
    1.0f,
    0.0f,
    1.0f
};

void CRenderTarget::phase_new_dof()
{
    // -------------------------------
    // 0) Focus (1x1)
    // -------------------------------
    {
        float W = 1.0f, H = 1.0f;
        float rW = 1.0f / W, rH = 1.0f / H;

        VP_DOF.Width  = W;
        VP_DOF.Height = H;
        RContext->RSSetViewports(1, &VP_DOF);

        u_setrt(rt_dof_focus, nullptr, nullptr, nullptr);
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(FALSE);

        // element 0 = dof_focus
        RCache.set_Element(s_dof_coc->E[0]);

        // TODO: set your focus params here (target focus, lerp alpha, etc.)
        //RCache.set_c("dof_focus_params", ...);

        RCache.set_Geometry(FSTriangleGeom);
        RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
    }

    // -------------------------------
    // Common size for the rest (dim)
    // -------------------------------
    float W = (float)Device.TargetWidth;
    float H = (float)Device.TargetHeight;
    float rW = 1.0f / W;
    float rH = 1.0f / H;

    VP_DOF.Width  = W;
    VP_DOF.Height = H;
    RContext->RSSetViewports(1, &VP_DOF);

    // -------------------------------
    // 1) CoC
    // -------------------------------
    {
        u_setrt(rt_dof_coc, nullptr, nullptr, nullptr);
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(FALSE);

        // element 1 = dof_coc
        RCache.set_Element(s_dof_coc->E[1]);

        // TODO: set your CoC params (focus, aperture scale, max coc, temporal alpha, etc.)
        // RCache.set_c("dof_coc_params", ...);

        // If your shader expects tex sizes:
        RCache.set_c("dof_params", 10.f, 16.5f, 0.024f, rH);

        RCache.set_Geometry(FSTriangleGeom);
        RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
    }
    
    // -------------------------------
    // 2) Blur pass 1 (Vertical)
    // -------------------------------
    {
        u_setrt(rt_dof_blur1, nullptr, nullptr, nullptr);
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(FALSE);

        // element 2 = dof_blur1
        RCache.set_Element(s_dof_coc->E[2]);

        // TODO: set blur params (dir/step/taps); for hex basic: vertical dir
        // RCache.set_c("dof_blur_params", ...);
        RCache.set_c("dof_rt_size", W, H, rW, rH);

        RCache.set_Geometry(FSTriangleGeom);
        RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
    }
    /*
    // -------------------------------
    // 3) Blur pass 2 (Diagonal)
    // -------------------------------
    {
        u_setrt(rt_dof_blur2, nullptr, nullptr, nullptr);
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(FALSE);

        // element 3 = dof_blur2
        RCache.set_Element(s_dof_blur2->E[3]);

        // TODO: set blur params; hex diagonal dir
        // RCache.set_c("dof_blur_params", ...);
        RCache.set_c("dof_rt_size", W, H, rW, rH);

        RCache.set_Geometry(FSTriangleGeom);
        RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
    }
    
    // -------------------------------
    // 4) Blur pass 3 (Rhomboid / Final DOF)
    // -------------------------------
    {
        u_setrt(rt_dof_blur3, nullptr, nullptr, nullptr);
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(FALSE);

        // element 4 = dof_blur3
        RCache.set_Element(s_dof_blur3->E[4]);

        // TODO: set blur params; rhomboid dir and final weights
        // RCache.set_c("dof_blur_params", ...);
        RCache.set_c("dof_rt_size", W, H, rW, rH);

        RCache.set_Geometry(FSTriangleGeom);
        RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
    }
    */
    // Copy current focus to previous for next frame
    GRHI->CopySurface(rt_dof_focus_prev->pSurface, rt_dof_focus->pSurface);
    // Copy current coc to previous for next frame
    GRHI->CopySurface(rt_dof_coc_prev->pSurface, rt_dof_coc->pSurface);
}