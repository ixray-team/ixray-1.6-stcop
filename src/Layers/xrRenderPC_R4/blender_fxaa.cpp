#include "stdafx.h"
#include "../xrRender/blender_fxaa.h"

CBlender_FXAA::CBlender_FXAA() { description.CLS = 0; }
CBlender_FXAA::~CBlender_FXAA() {}

void CBlender_FXAA::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);
    switch (C.iElement)
    {
        case 0:
            C.r_Pass("stub_notransform_aa_AA", "fxaa_main", FALSE, FALSE, FALSE);
            C.r_dx10Texture("s_image", r2_RT_albedo);
            C.r_dx10Sampler("smp_nofilter");
            C.r_dx10Sampler("smp_rtlinear");
            C.r_End();
            break;
    }
}