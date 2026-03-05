#include "stdafx.h"
#include "blender_bloom_downsample.h"

CBlender_bloom_downsample::CBlender_bloom_downsample() { description.CLS = 0; }
CBlender_bloom_downsample::~CBlender_bloom_downsample() {}

void CBlender_bloom_downsample::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_generic);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 1:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_bloomA);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_bloomB);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 3:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_bloomC);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 4:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_bloomD);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 5:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_bloomE);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 6:
        C.r_Pass("stub_fullscreen_triangle", "bloom_downsample", FALSE, FALSE, FALSE);
        C.r_dx10Texture("b_image", r2_RT_bloomF);

        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    }
}