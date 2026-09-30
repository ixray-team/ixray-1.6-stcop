#include "stdafx.h"
#include "blender_tonemap_lut_bake.h"

void CBlender_tonemap_lut_bake::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);
	if (C.iElement == 0)
	{
		C.r_ComputePass("tonemap_lut_bake");
		C.r_dx10Texture("s_tonemap_state", r4_RT_tonemap_state);
		C.r_End();
	}
}
