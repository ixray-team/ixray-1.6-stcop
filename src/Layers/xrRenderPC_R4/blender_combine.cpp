#include "stdafx.h"


#include "blender_combine.h"

CBlender_combine::CBlender_combine	()	{	description.CLS		= 0;	}
CBlender_combine::~CBlender_combine	()	{	}

void	CBlender_combine::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	switch (C.iElement)
	{
	case 0:	// combine
		C.r_Pass			("stub_fullscreen_triangle",		"combine_1",		FALSE,	FALSE,	FALSE, TRUE, D3DBLEND_INVSRCALPHA, D3DBLEND_SRCALPHA);	//. MRT-blend?
		C.r_Stencil			(TRUE,D3DCMP_LESSEQUAL,0xff,0x00);	// stencil should be >= 1
		C.r_StencilRef		(0x01);

		C.r_dx10Texture		("s_position",		r2_RT_P				);
		C.r_dx10Texture		("s_normal",		r2_RT_N				);
		C.r_dx10Texture		("s_diffuse",		r2_RT_albedo		);
		C.r_dx10Texture		("s_surface",		r2_RT_S);
		C.r_dx10Texture		("s_accumulator",	r2_RT_accum			);
		C.r_dx10Texture		("s_material",		r2_material			);
		C.r_dx10Texture		("env_s0",			r2_T_envs0			);
		C.r_dx10Texture		("env_s1",			r2_T_envs1			);
		C.r_dx10Texture		("sky_s0",			r2_T_sky0			);
		C.r_dx10Texture		("sky_s1",			r2_T_sky1			);
		C.r_dx10Texture		("s_env",			r2_RT_env_temp		);
		C.r_dx10Texture		("s_occ",			r2_RT_ssao_temp		);
		C.r_dx10Texture		("s_half_depth",	r2_RT_half_depth	);
		C.r_dx10Texture		("s_refl",			r2_RT_sslr			);
		jitter(C);

		C.r_dx10Sampler		("smp_nofilter");
		C.r_dx10Sampler		("smp_material");
		C.r_dx10Sampler		("smp_rtlinear");
		C.r_dx10Sampler		("smp_linear");

		C.r_End				();
		break;
	case 1:
		C.r_Pass("stub_fullscreen_triangle", "combine_distort", FALSE, FALSE, FALSE);

		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_image", r2_RT_generic0);
		C.r_dx10Texture("s_distort", r2_RT_generic1);

		C.r_dx10Sampler("smp_nofilter");
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
		break;

	case 2:
	{
		auto is_loot_present = !!FS.exist(_game_textures_, "shaders\\lut.dds");
		is_loot_present |= !!FS.exist("$level$", "shaders\\lut.dds");

		if (is_loot_present) 
		{
			RImplementation.addShaderOption("USE_LUT_TEXTURE", "1");
		}
		if (ps_r2_new_autoexposure)
		{
			RImplementation.addShaderOption("USE_NEW_ADAPT", "1");
		}
		if (ps_r2_new_bloom_tonemap)
		{
			RImplementation.addShaderOption("USE_NEW_BLOOM_TONEMAP", "1");
		}
		if (ps_r2_crossfeed)
		{
			RImplementation.addShaderOption("USE_CROSSFEED", "1");
		}
		if (ps_r2_vibrance)
		{
			RImplementation.addShaderOption("USE_VIBRANCE", "1");
		}
		C.r_Pass("stub_fullscreen_triangle", "combine_2", FALSE, FALSE, FALSE);

		C.r_dx10Texture		("s_position",		r2_RT_upscaled_depth);
		C.r_dx10Texture		("s_normal",		r2_RT_N);
		C.r_dx10Texture		("s_bloom",			r2_RT_bloom1);
		C.r_dx10Texture		("s_image",			r2_RT_generic);
		C.r_dx10Texture		("s_tonemap",		r2_RT_luminance_cur);
		C.r_dx10Texture		("n_bloom",			r2_RT_bloomA2);
		C.r_dx10Texture		("s_adapt",			r2_RT_lumD);

		if (is_loot_present)
		{
			C.r_dx10Texture("s_lut", "shaders\\lut");
		}

		C.r_dx10Sampler("smp_nofilter");
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
		break;
	}
	case 3:
		C.r_Pass("pfx_volumetric_light", "pfx_volumetric_light", false, FALSE, FALSE, TRUE, D3DBLEND_ONE, D3DBLEND_ONE);
		
		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_smap", r2_RT_smap_depth);
		
		C.r_dx10Sampler("smp_rtlinear");
		C.r_dx10Sampler("smp_smap");
		C.r_dx10Sampler("smp_nofilter");
		C.r_End();
	}
	RImplementation.clearAllShaderOptions();
}
