#include "stdafx.h"
#pragma hdrstop

#include "Blender_light_direct.h"

CBlender_accum_direct::CBlender_accum_direct	()	{ description.CLS		= 0;	}
CBlender_accum_direct::~CBlender_accum_direct	()	{	}

void CBlender_accum_direct::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	C.r_Pass("stub_fullscreen_triangle", "accum_sun", false, TRUE, FALSE);
	C.r_CullMode(D3DCULL_NONE);
	C.RS.SetRS(D3DRS_ZFUNC, D3D11_COMPARISON_GREATER);
	C.r_Stencil(FALSE);

	C.r_dx10Texture("s_diffuse", r2_RT_albedo);
	C.r_dx10Texture("s_surface", r2_RT_S);
	C.r_dx10Texture("s_position", r2_RT_P);
	C.r_dx10Texture("s_normal", r2_RT_N);
	C.r_dx10Texture("s_material", r2_material);
	C.r_dx10Texture("s_accumulator", r2_RT_accum);
	C.r_dx10Texture("s_lmap", r2_sunmask);
	C.r_dx10Texture("s_smap_array", "$user$shadow_atlas_srv");

	jitter(C);

	C.r_dx10Sampler("smp_nofilter");
	C.r_dx10Sampler("smp_material");
	C.r_dx10Sampler("smp_linear");
	C.r_dx10Sampler("smp_smap");

	C.r_End();
}
