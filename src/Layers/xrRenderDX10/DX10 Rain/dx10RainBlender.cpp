#include "stdafx.h"
#include "dx10RainBlender.h"

void CBlender_rain::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if (C.iElement > 1)
	{
		return;
	}

	RImplementation.addShaderOption("DISABLE_MOTION_VECTORS", "1");

	C.r_ComputePass("rain_render");

	C.r_dx10Texture("s_diffuse", r2_RT_albedo);
	C.r_dx10Texture("s_position", r2_RT_P);
	C.r_dx10Texture("s_surface", r2_RT_S);
	C.r_dx10Texture("s_normal", r2_RT_N);

	C.r_dx10Texture("s_smap", r2_RT_smap_depth);
	C.r_dx10Texture("s_lmap", r2_sunmask);

	C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");

	C.r_dx10Texture("s_waterFall", "water\\water_flowing_nmap");
	C.r_dx10Texture("s_water", "water\\water_SBumpVolume");

	C.r_dx10Texture("s_mask", "rain_mask");

	C.r_dx10Sampler("smp_nofilter");
	C.r_dx10Sampler("smp_rtlinear");
	C.r_dx10Sampler("smp_linear");
	C.r_dx10Sampler("smp_smap");

	C.r_End();
}

