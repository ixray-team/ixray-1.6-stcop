#include "stdafx.h"


#include "blender_combine.h"

CBlender_combine::CBlender_combine	()	{	description.CLS		= 0;	}
CBlender_combine::~CBlender_combine	()	{	}

void	CBlender_combine::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	switch (C.iElement)
	{
	case 2:
	{
		auto is_loot_present = !!FS.exist(_game_textures_, "shaders\\lut.dds");
		is_loot_present |= !!FS.exist("$level$", "shaders\\lut.dds");

		if (is_loot_present) 
		{
			C.AddDefine("USE_LUT_TEXTURE", "1");
		}

		C.r_Pass("stub_fullscreen_triangle", "combine_2", false, false, false);
		
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_image", r2_RT_generic);
		C.r_dx10Texture("s_dof_near", R4_RT_DOF_DOWNSAMPLE);
		C.r_dx10Texture("s_dof_far", r2_RT_dof_blur3);
		C.r_dx10Texture("s_dof_metadata", R4_RT_DOF_DOWNSAMPLE_DEPTH);
		C.r_dx10Texture("s_dof_coc", r2_RT_dof_coc);
		C.r_dx10Texture("s_dof_scope_mask", r2_RT_dof_scope_mask);
		C.r_dx10Texture("s_tonemap", r2_RT_luminance_cur);
		C.r_dx10Texture("s_bloom", r2_RT_bloomA2);

		if (is_loot_present)
		{
			C.r_dx10Texture("s_lut", "shaders\\lut");
		}

		C.r_End();
		break;
	}
	case 3:
		C.r_Pass("pfx_volumetric_light", "pfx_volumetric_light", false, false, false, true, D3DBLEND_ONE, D3DBLEND_ONE);
		
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_smap", r2_RT_smap_depth);
		C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");
		
		C.r_End();
	}
	C.Defines.clear();
}
