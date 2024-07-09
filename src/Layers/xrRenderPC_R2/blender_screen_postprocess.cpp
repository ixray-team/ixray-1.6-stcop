#include "stdafx.h"

#include "../xrRender/blender_screen_postprocess.h"

CBlender_SPP::CBlender_SPP() { description.CLS = 0; }
CBlender_SPP::~CBlender_SPP() {}

void CBlender_SPP::Compile(CBlender_Compile& C) {
	IBlender::Compile(C);
	switch (C.iElement)
	{
	case ScreenPostProcessType::Vignette:
		C.r_Pass("null", "vignette", FALSE, FALSE, FALSE);
		break;
	case ScreenPostProcessType::Aberration:
		C.r_Pass("null", "chromatic_aberration", FALSE, FALSE, FALSE);
		break;
	}

	C.r_Sampler_clf("s_image", r2_RT_albedo);
	C.r_End();
}