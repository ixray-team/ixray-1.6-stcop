#include "stdafx.h"
#pragma hdrstop

#include "Blender_light_direct.h"

CBlender_accum_direct::CBlender_accum_direct	()	{ description.CLS		= 0;	}
CBlender_accum_direct::~CBlender_accum_direct	()	{	}

void CBlender_accum_direct::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if(C.iElement > SE_SUN_FAR) {
		return;
	}

	if(C.iElement == SE_SUN_FAR) {
		RImplementation.addShaderOption("USE_FAR_ATTENTION", "1");
	}

	C.r_Pass("accum_volume", "accum_sun", false, TRUE, FALSE);
	C.r_CullMode(D3DCULL_NONE);

	if(C.iElement != SE_SUN_FAR) {
		C.PassSET_ZB(TRUE, FALSE, TRUE);
	}

	C.r_dx10Texture("s_diffuse", r2_RT_albedo);
	C.r_dx10Texture("s_position", r2_RT_P);
	C.r_dx10Texture("s_normal", r2_RT_N);
	C.r_dx10Texture("s_material", r2_material);
	C.r_dx10Texture("s_accumulator", r2_RT_accum);
	C.r_dx10Texture("s_lmap", r2_sunmask);
	C.r_dx10Texture("s_smap", r2_RT_smap_depth);

	jitter(C);

	C.r_dx10Sampler("smp_nofilter");
	C.r_dx10Sampler("smp_material");
	C.r_dx10Sampler("smp_linear");
	C.r_dx10Sampler("smp_smap");

	C.r_End();
}
