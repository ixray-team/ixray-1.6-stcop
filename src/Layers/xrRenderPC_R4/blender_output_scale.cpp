#include "stdafx.h"
#include "blender_output_scale.h"

CBlender_OutputScale::CBlender_OutputScale() { description.CLS = 0; }
CBlender_OutputScale::~CBlender_OutputScale() {}

void CBlender_OutputScale::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);
    switch (C.iElement)
    {
    case SCALEPHASE_SCALE_LINEAR:
        C.r_Pass("stub_notransform_t_target", "scale_linear", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_image", r2_RT_target);
        C.r_dx10Sampler("smp_nofilter");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_End();
        break;  
    case SCALEPHASE_SCALE_NEAREST:
        C.r_Pass("stub_notransform_t_target", "scale_nearest", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_image", r2_RT_target);
        C.r_dx10Sampler("smp_nofilter");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_End();
        break; 
    case SCALEPHASE_MOTION_VECTORS:
        C.r_Pass("stub_notransform_t_scaled", "copy_4to2", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_image", r4_motion);
        C.r_dx10Sampler("smp_nofilter");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_End();
        break;  
    case SCALEPHASE_COPY_DEPTH:
        C.r_Pass("stub_notransform_t_scaled", "copy_depth", FALSE, FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_depth", r2_RT_P);
        C.r_dx10Sampler("smp_nofilter");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_End();
        break;  
    }
}