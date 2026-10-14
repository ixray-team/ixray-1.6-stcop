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

	// _ParseList substitutes $null for a missing texture list; it is not a level mask.
	const bool UseRainMask = !RImplementation.o.dx11_use_legacy_light && RImplementation.o.HasRainPuddleMask && !C.L_textures.empty() && xr_strcmp(C.L_textures[0].c_str(), "$null") != 0;
	if (UseRainMask)
	{
		RImplementation.addShaderOption("USE_RAIN_MASK", "1");
	}

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
	if (!RImplementation.o.dx11_use_legacy_light && RImplementation.o.ProceduralRainPuddles)
	{
		C.r_dx10Texture("s_puddles_perlin", "shaders\\rain_puddles_perlin");
		C.r_dx10Texture("s_puddles_normal", "shaders\\rain_puddles_normal");
		C.r_dx10Texture("s_puddles_ripples", "shaders\\rain_puddles_ripples");
		C.r_dx10Texture("s_puddles_water_height", "$user$rain_puddles_water_height");
		C.r_dx10Sampler("smp_base");
	}

	if (UseRainMask)
	{
		C.r_dx10Texture("s_mask", C.L_textures[0]);
	}

	C.r_dx10Sampler("smp_nofilter");
	C.r_dx10Sampler("smp_rtlinear");
	C.r_dx10Sampler("smp_linear");
	C.r_dx10Sampler("smp_smap");

	C.r_End();
}

