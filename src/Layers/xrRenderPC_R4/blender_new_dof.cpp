#include "stdafx.h"
#include "blender_new_dof.h"

CBlender_new_dof::CBlender_new_dof()
{
    description.CLS = 0;
}

CBlender_new_dof::~CBlender_new_dof()
{
}

void CBlender_new_dof::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    // 0) Write rt_dof_focus (and lerp with rt_dof_focus_prev in shader)
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "dof_focus", false, false, false);

        // If you actually sample previous focus from a texture, uncomment and bind:
        C.r_dx10Texture("t_focus_prev", r2_RT_dof_focus_prev);
        C.r_dx10Texture("s_position", r2_RT_P);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");
        C.r_End();
        break;

    // 1) Build CoC (optionally lerp with rt_dof_coc_prev in shader)
    case 1:
        C.r_Pass("stub_fullscreen_triangle", "dof_coc", false, false, false);

        // Typical inputs (adjust to your engine conventions):
        // C.r_dx10Texture("t_depth",   r2_RT_depth);          // or whatever your depth SRV is
        C.r_dx10Texture("t_focus",   r2_RT_dof_focus);      // if focus sampled as 1x1
        C.r_dx10Texture("t_coc_prev",r2_RT_dof_coc_prev);   // if doing simple temporal lerp in same UV
        C.r_dx10Texture("s_position", r2_RT_P);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");
        C.r_End();
        break;
    
    // 2) Hex blur pass1 (vertical)
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "dof_blur1", false, false, false);

        // Source HDR color + CoC
        C.r_dx10Texture("t_image", r2_RT_generic);   // your HDR scene color
		C.r_dx10Texture("t_bimage", r2_RT_bloomC);   // if you want to read original unblurred color for better quality (optional)
        C.r_dx10Texture("t_coc",   r2_RT_dof_coc);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");
        C.r_End();
        break;
    }
}