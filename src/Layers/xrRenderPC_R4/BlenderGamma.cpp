#include "stdafx.h"

#include "BlenderGamma.h"

CBlender_gamma::CBlender_gamma()
{
	description.CLS = 0;
}

void CBlender_gamma::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	switch (C.iElement)
	{
	case 0: // applying
		C.r_Pass("stub_fullscreen_triangle", "gamma_apply", false, false, false);
		C.r_dx10Texture("s_image", r2_RT_backbuffer_lut);
		C.r_dx10Texture("s_gamma_lut", r2_RT_gamma_lut);
		C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");

		C.r_dx10Sampler("smp_nofilter");
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
		break;
	}
}
