#include "stdafx.h"
#include "blender_taa.h"

CBlender_taa::CBlender_taa() { description.CLS = 0; }
CBlender_taa::~CBlender_taa() {}

void CBlender_taa::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "taa_render", FALSE, FALSE, FALSE);
		C.r_dx10Texture("s_image", r2_RT_generic0);
		C.r_dx10Texture("s_image_prev", r2_RT_generic0_prev);	
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_velocity", r2_RT_velocity);
        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");
        C.r_End();
        break;
	}
}