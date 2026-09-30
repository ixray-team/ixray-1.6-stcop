#include "stdafx.h"
#include "blender_compute_bloom.h"

void CBlender_compute_bloom::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);
	switch (C.iElement)
	{
	case 0:
		C.r_ComputePass("bloom_downsample");
		C.r_dx10Texture("s_image", C.L_textures[0]);
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
		break;
	case 1:
		C.r_ComputePass("bloom_upsample");
		C.r_dx10Texture("s_image", C.L_textures[1]);
		C.r_dx10Texture("t_image", C.L_textures[2]);
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
		break;
	}
}
