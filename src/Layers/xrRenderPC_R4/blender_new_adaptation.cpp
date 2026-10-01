#include "stdafx.h"
#include "blender_new_adaptation.h"

void CBlender_histogram_debug::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);
    if (C.iElement == 0)
    {
        C.r_ComputePass("combine_histogram");
        C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
        C.r_End();
    }
    else if (C.iElement == 1)
    {
        C.r_Pass("stub_fullscreen_triangle", "combine_histogram_debug", false, false, false);
        C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
        C.r_dx10Texture("s_histogram", r4_RT_lum_histogram);
        C.r_dx10Texture("s_output_histogram", "$user$combine_histogram");
        C.r_dx10Texture("s_tonemap_compute", r4_RT_lum_compute);
        C.r_dx10Texture("s_tonemap_state", r4_RT_tonemap_state);
        C.r_dx10Texture("s_tonemap_lut", r4_RT_tonemap_lut);
        C.r_dx10Sampler("smp_rtlinear");
        C.r_End();
    }
}

CBlender_new_adaptation::CBlender_new_adaptation() { description.CLS = 0; }
CBlender_new_adaptation::~CBlender_new_adaptation() {}

void CBlender_new_adaptation::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "bloom_lum_copy", false, false, false);
        // Both metering paths use the same unexposed scene, in the same units.
        C.r_dx10Texture("s_image", r2_RT_generic);

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
    case 4:
        C.r_ComputePass("bloom_lum_histogram");
        C.r_dx10Texture("s_image", r2_RT_generic);
        C.r_End();
        break;
    case 5:
        C.r_ComputePass("bloom_lum_reduce");
        C.r_dx10Texture("s_histogram", r4_RT_lum_histogram);
        C.r_End();
        break;
    }

    RImplementation.clearAllShaderOptions();
}
