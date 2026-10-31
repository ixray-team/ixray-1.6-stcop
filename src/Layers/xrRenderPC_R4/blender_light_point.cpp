#include "stdafx.h"


#include "blender_light_point.h"

CBlender_accum_point::CBlender_accum_point() {
	description.CLS = 0;
}

CBlender_accum_point::~CBlender_accum_point() {}

void CBlender_accum_point::Compile(CBlender_Compile& C) 
{
	IBlender::Compile(C);

	if(C.iElement == SE_L_FILL)
	{
		C.r_Pass("stub_notransform", "copy", false, false, false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_End();

		return;
	}

	if(C.iElement > SE_L_TRANSLUENT)
	{
		return;
	}

	if(C.iElement != SE_L_UNSHADOWED) 
	{
		C.AddDefine("USE_SHADOW", "1");
	}

	if(C.iElement == SE_L_TRANSLUENT) 
	{
		C.AddDefine("USE_SMAP", "1");
	}

	C.r_Pass("accum_volume", "accum_base", false, false, false, true, D3DBLEND_ONE, D3DBLEND_ONE);

	C.r_dx10Texture("s_diffuse", r2_RT_albedo);
	C.r_dx10Texture("s_surface", r2_RT_S);
	C.r_dx10Texture("s_position", r2_RT_P);
	C.r_dx10Texture("s_normal", r2_RT_N);
	C.r_dx10Texture("s_material", r2_material);
	C.r_dx10Texture("s_lmap", C.L_textures[0]);


	jitter(C);

	if(C.iElement != SE_L_UNSHADOWED) 
	{
		C.r_dx10Texture("s_smap", r2_RT_smap_depth);
		C.r_dx10Texture("s_mask", r2_RT_smap_surf);
	}

	C.r_End();
}

