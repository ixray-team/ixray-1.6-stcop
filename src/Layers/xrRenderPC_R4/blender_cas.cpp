#include "stdafx.h"
#include "blender_cas.h"

CBlender_Sharpening::CBlender_Sharpening() { description.CLS = 0; }
CBlender_Sharpening::~CBlender_Sharpening() {}

void CBlender_Sharpening::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

	if (C.iElement == 0)
	{
		C.r_Pass("stub_fullscreen_triangle", "sharpening_amd_cas", false, false, false);
		C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_nofilter");
		C.r_End();
	}
	else if (C.iElement == 1)
	{
		C.r_Pass("stub_fullscreen_triangle", "sharpening_tinysharpening", false, false, false);
		C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_nofilter");
		C.r_End();
	}
	else if (C.iElement == 2)
	{
		C.r_Pass("stub_fullscreen_triangle", "ui_postprocess_fill", false, false, false);
		C.r_dx10Texture("s_image", r2_RT_backbuffer_lut);
		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_nofilter");
		C.r_End();
	}
	else if (C.iElement == 3)
	{
		C.r_Pass("stub_fullscreen_triangle", "ui_postprocess", false, false, false, true);
		C.r_dx10Texture("s_image", r2_RT_ui_color);
		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_nofilter");
		C.r_End();
	}
}