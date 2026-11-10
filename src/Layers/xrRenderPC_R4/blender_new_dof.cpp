#include "stdafx.h"
#include "blender_new_dof.h"

CBlender_new_dof::CBlender_new_dof(bool IsBlur) : IsBlur(IsBlur)
{
	description.CLS = 0;
}
CBlender_new_dof::~CBlender_new_dof() {}

void CBlender_new_dof::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	// Separate groups fit SHADER_ELEMENTS_MAX without changing the material shader API.
	if (IsBlur)
	{
		if (C.iElement > 3)
		{
			return;
		}
		const bool IsNear = C.iElement >= 2;
		const bool IsResolve = (C.iElement & 1) != 0;
		if (IsNear)
		{
			C.AddDefine("DOF_NEAR", "1");
		}
		C.r_Pass("stub_fullscreen_triangle", IsResolve ? "dof_blur2" : "dof_blur1", false, false, false);
		C.r_dx10Texture("t_metadata", R4_RT_DOF_DOWNSAMPLE_DEPTH);
		C.r_dx10Texture("t_dof_tiles", R4_RT_DOF_TILE_DILATED);
		if (IsResolve)
		{
			C.r_dx10Texture("t_dof_real0", r2_RT_dof_blur1);
			C.r_dx10Texture("t_dof_imaginary0", r2_RT_dof_blur2);
			if (!IsNear)
			{
				C.r_dx10Texture("t_dof_real1", R4_RT_DOF_REAL1);
				C.r_dx10Texture("t_dof_imaginary1", R4_RT_DOF_IMAGINARY1);
			}
		}
		else
		{
			C.r_dx10Texture(IsNear ? "t_near" : "t_far", IsNear ? R4_RT_DOF_DOWNSAMPLE : r2_RT_dof_blur3);
			C.r_dx10Texture("s_position", r2_RT_P);
		}
		C.r_End();
		return;
	}

	switch (C.iElement)
	{
	case 0:
		C.r_Pass("stub_fullscreen_triangle", "dof_focus", false, false, false);
		C.r_dx10Texture("t_focus_prev", r2_RT_dof_focus_prev);
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_End();
		break;
	case 1:
	case 4:
		if (C.iElement == 1)
		{
			C.AddDefine("USE_DOF_PHYSICAL", "1");
		}
		C.r_Pass("stub_fullscreen_triangle", "dof_coc", false, false, false);
		C.r_dx10Texture("t_focus", r2_RT_dof_focus);
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_dof_scope_mask", r2_RT_dof_scope_mask);
		C.r_End();
		break;
	case 2:
	case 5:
		if (C.iElement == 5)
		{
			C.AddDefine("DOF_TILE_DILATE", "1");
		}
		C.r_Pass("stub_fullscreen_triangle", "dof_tiles", false, false, false);
		if (C.iElement == 5)
		{
			C.r_dx10Texture("t_dof_tiles", R4_RT_DOF_TILE_MAX);
		}
		else
		{
			C.r_dx10Texture("t_metadata", R4_RT_DOF_DOWNSAMPLE_DEPTH);
		}
		C.r_End();
		break;
	case 3:
		C.r_Pass("stub_fullscreen_triangle", "dof_downsample", false, false, false);
		C.r_dx10Texture("t_image", r2_RT_generic);
		C.r_dx10Texture("t_coc", r2_RT_dof_coc);
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_End();
		break;
	}
}
