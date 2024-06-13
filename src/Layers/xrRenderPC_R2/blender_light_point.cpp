#include "stdafx.h"
#pragma hdrstop

#include "Blender_light_point.h"

CBlender_accum_point::CBlender_accum_point() {
	description.CLS = 0;
}

CBlender_accum_point::~CBlender_accum_point() {}

void CBlender_accum_point::Compile(CBlender_Compile& C) {
	IBlender::Compile(C);

	if(C.iElement == SE_L_FILL) {
		C.r_Pass("null", "copy", false, FALSE, FALSE);
		C.r_Sampler("s_base", C.L_textures[0]);
		C.r_End();

		return;
	}

	if(C.iElement > SE_L_TRANSLUENT) {
		return;
	}

	if(C.iElement != SE_L_UNSHADOWED) {
		RImplementation.addShaderOption("USE_SHADOW", "1");
	}
	
	if(C.iElement == SE_L_TRANSLUENT) {
		RImplementation.addShaderOption("USE_LMAP", "1");
	}

	C.r_Pass("accum_volume", "accum_base", false, FALSE, FALSE, TRUE, D3DBLEND_ONE, D3DBLEND_ONE);

	C.r_Sampler_rtf("s_position", r2_RT_P);
	C.r_Sampler_rtf("s_normal", r2_RT_N);

	C.r_Sampler_clw("s_material", r2_material);

	if(C.iElement != SE_L_TRANSLUENT) {
		C.r_Sampler_clf("s_lmap", r2_RT_smap_surf);
	}
	else {
		C.r_Sampler("s_lmap", C.L_textures[0]);
	}

	if(C.iElement != SE_L_UNSHADOWED) {
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
	}

	jitter(C);
	C.r_End();
}
