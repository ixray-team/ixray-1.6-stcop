#include "stdafx.h"
#include "blender_nvg.h"

CBlender_nvg::CBlender_nvg() { description.CLS = 0; }
CBlender_nvg::~CBlender_nvg() {}

void CBlender_nvg::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
		C.r_Pass("stub_fullscreen_triangle", "nvg_render", FALSE, FALSE, FALSE);
		C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
		C.r_dx10Texture("s_tonemap", r2_RT_luminance_cur);
        C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");
		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_nofilter");
		C.r_End();
		break;
    }
}