#include "stdafx.h"
#include "blender_bloom_upsample.h"

CBlender_bloom_upsample::CBlender_bloom_upsample() { description.CLS = 0; }
CBlender_bloom_upsample::~CBlender_bloom_upsample() {}

void CBlender_bloom_upsample::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
        C.r_dx10Texture("b_image", r2_RT_bloomG);
        C.r_dx10Texture("t_image", r2_RT_bloomF);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 1:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
        C.r_dx10Texture("b_image", r2_RT_bloomF2);
        C.r_dx10Texture("t_image", r2_RT_bloomE);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
        C.r_dx10Texture("b_image", r2_RT_bloomE2);
        C.r_dx10Texture("t_image", r2_RT_bloomD);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 3:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
        C.r_dx10Texture("b_image", r2_RT_bloomD2);
        C.r_dx10Texture("t_image", r2_RT_bloomC);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 4:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
        C.r_dx10Texture("b_image", r2_RT_bloomC2);
        C.r_dx10Texture("t_image", r2_RT_bloomB);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 5:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
        C.r_dx10Texture("b_image", r2_RT_bloomB2);
        C.r_dx10Texture("t_image", r2_RT_bloomA);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    }
}