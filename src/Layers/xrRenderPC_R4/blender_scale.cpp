#include "stdafx.h"
#include "blender_scale.h"

CBlender_scale::CBlender_scale() { description.CLS = 0; }
CBlender_scale::~CBlender_scale() {}

void CBlender_scale::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

	if(C.iElement == 1) {
		RImplementation.addShaderOption("FILTER_TYPE", "smp_nofilter");
		RImplementation.addShaderOption("USE_POINT_FILTER", "1");
	}
	else {
		RImplementation.addShaderOption("FILTER_TYPE", "smp_rtlinear");
		RImplementation.addShaderOption("USE_LINEAR_FILTER", "1");
	}

	C.r_Pass("stub_notransform_t", "copy_image", FALSE, FALSE, FALSE);
	C.r_dx10Texture("s_image", r2_RT_generic0);

	C.r_dx10Sampler("smp_nofilter");
	C.r_dx10Sampler("smp_rtlinear");

	C.r_End();

	RImplementation.clearAllShaderOptions();
}