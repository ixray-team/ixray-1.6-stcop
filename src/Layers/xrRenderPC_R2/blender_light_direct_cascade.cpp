#include "stdafx.h"
#pragma hdrstop

#include "blender_light_direct_cascade.h"

CBlender_accum_direct_cascade::CBlender_accum_direct_cascade() {
	description.CLS = 0;
}

CBlender_accum_direct_cascade::~CBlender_accum_direct_cascade() {}

void CBlender_accum_direct_cascade::Compile(CBlender_Compile& C) {
	IBlender::Compile(C);

	if(C.iElement > SE_SUN_FAR) {
		return;
	}

	if(C.iElement == SE_SUN_FAR) {
		RImplementation.addShaderOption("USE_FAR_ATTENTION", "1");
	}

	C.r_Pass("accum_volume", "accum_sun", false, TRUE, FALSE);

	C.r_Sampler_rtf("s_position", r2_RT_P);
	C.r_Sampler_rtf("s_normal", r2_RT_N);
	C.r_Sampler_clw("s_material", r2_material);
	C.r_Sampler_rtf("s_accumulator", r2_RT_accum);
	C.r_Sampler("s_lmap", r2_sunmask);

	if(RImplementation.o.HW_smap) {
		if(RImplementation.o.HW_smap_PCF) {
			C.r_Sampler_clf("s_smap", r2_RT_smap_depth);
		}
		else {
			C.r_Sampler_rtf("s_smap", r2_RT_smap_depth);
		}
	}
	else {
		C.r_Sampler_rtf("s_smap", r2_RT_smap_surf);
	}

	jitter(C);

	u32 s = C.i_Sampler("s_smap");
	C.i_Address(s, D3DTADDRESS_BORDER);
	C.i_BorderColor(s, color_argb(255, 255, 255, 255));

	C.r_End();
}
