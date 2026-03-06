#include "stdafx.h"
#include "blender_new_adaptation.h"

CBlender_new_adaptation::CBlender_new_adaptation() { description.CLS = 0; }
CBlender_new_adaptation::~CBlender_new_adaptation() {}

void CBlender_new_adaptation::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    if (!ps_r2_new_autoexposure)
    {
        RImplementation.addShaderOption("USE_CLASSIQUE_TONEMAP", "1");
    }

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "bloom_lum_copy", false, false, false);
        C.r_dx10Texture("s_image", r2_RT_bloomA);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 1:
        C.r_Pass("stub_fullscreen_triangle", "bloom_lum_downsample", false, false, false);
        C.r_dx10Texture("s_image", r2_RT_lumA);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "bloom_lum_downsample", false, false, false);
        C.r_dx10Texture("s_image", r2_RT_lumB);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 3:
		if (ps_r2_autoexposure_center_weight)
        {
            RImplementation.addShaderOption("USE_CENTER_WEIGHTED_LUMA", "1");
        }

        if (ps_r2_autoexposure_soft_log)
        {
            RImplementation.addShaderOption("USE_SOFT_LOG", "1");
        }
		
        C.r_Pass("stub_fullscreen_triangle", "bloom_lum_calc", false, false, false, TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA);
        C.r_dx10Texture("s_image", r2_RT_lumC);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    }

    RImplementation.clearAllShaderOptions();
}