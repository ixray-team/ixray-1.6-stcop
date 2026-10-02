// r4_rendertarget_phase_new_dof.cpp
#include "stdafx.h"
#include "r4_rendertarget.h"

static RHIViewport VP_DOF = {
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

        VP_DOF.Width  = W;
        VP_DOF.Height = H;
        GRHI->SetViewport(VP_DOF);

        DrawSQ(s_dof_coc, rt_dof_focus, 0, []
        {
            GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
            RCache.set_Stencil(false);
        });
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
    GRHI->SetViewport(VP_DOF);

    // -------------------------------
    // 1) CoC
    // -------------------------------
    {
        DrawSQ(s_dof_coc, rt_dof_coc, 1, [&]
        {
            GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
            RCache.set_Stencil(false);
            RCache.set_c("dof_params", 10.f, 16.5f, 0.024f, rH);
        });
    }
    
    // -------------------------------
    // 2) Blur pass 1 (Vertical)
    // -------------------------------
    {
        DrawSQ(s_dof_coc, rt_dof_blur1, 2, [&]
        {
            GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
            RCache.set_Stencil(false);
            RCache.set_c("dof_rt_size", W, H, rW, rH);
        });
    }
    /*
    // -------------------------------
    // 3) Blur pass 2 (Diagonal)
    // -------------------------------
    {
        u_setrt(rt_dof_blur2, nullptr, nullptr, nullptr);
        GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
        RCache.set_Stencil(false);

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
        RCache.set_Stencil(false);

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
    ResolveSurface(rt_dof_focus_prev, rt_dof_focus);
    ResolveSurface(rt_dof_coc_prev, rt_dof_coc);
}