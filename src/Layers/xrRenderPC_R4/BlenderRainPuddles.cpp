#include "stdafx.h"
#include "BlenderRainPuddles.h"

BlenderRainPuddles::BlenderRainPuddles()
{
	description.CLS = 0;
}

void BlenderRainPuddles::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);
	if (C.iElement != 0)
	{
		return;
	}

	C.r_Pass("stub_fullscreen_triangle", "rain_puddles", false, false, false, true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA);
	C.r_ColorWriteEnable(true, true, true, false);
	C.r_dx10Texture("s_position", r2_RT_P);
	C.r_dx10Texture("s_normal", r2_RT_N);
	C.r_dx10Texture("s_diffuse", r2_RT_albedo);
	C.r_dx10Texture("s_surface", r2_RT_S);
	// Water SSR reprojects the hit into the previous frame's scene image.
	C.r_dx10Texture("s_image", r2_RT_generic);
	C.r_dx10Texture("s_env0", r2_T_sky0);
	C.r_dx10Texture("s_env1", r2_T_sky1);
	C.r_dx10Sampler("smp_nofilter");
	C.r_dx10Sampler("smp_rtlinear");
	C.r_dx10Sampler("smp_linear");
	C.r_End();
}
