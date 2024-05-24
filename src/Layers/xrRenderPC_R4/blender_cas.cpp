#include "stdafx.h"
#include "blender_cas.h"

CBlender_cas::CBlender_cas() { description.CLS = 0; }
CBlender_cas::~CBlender_cas() {}

void CBlender_cas::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);
	C.r_Pass("stub_notransform_t", "contrast_adaptive_sharpening", FALSE, FALSE, FALSE);
	C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
	C.r_dx10Sampler("smp_rtlinear");
	C.r_End();
}