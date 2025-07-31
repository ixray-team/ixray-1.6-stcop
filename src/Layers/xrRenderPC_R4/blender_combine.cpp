#include "stdafx.h"
#pragma hdrstop

#include "blender_combine.h"

CBlender_combine::CBlender_combine	()	{	description.CLS		= 0;	}
CBlender_combine::~CBlender_combine	()	{	}

void	CBlender_combine::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	switch (C.iElement)
	{
	case 0:	// combine
		C.r_Pass			("combine_1",		"combine_1",		FALSE,	FALSE,	FALSE, TRUE, D3DBLEND_INVSRCALPHA, D3DBLEND_SRCALPHA);	//. MRT-blend?
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
		C.r_dx10Texture		("s_occ",			r2_RT_ssao_temp		);
		C.r_dx10Texture		("s_half_depth",	r2_RT_half_depth	);
		jitter(C);

		C.r_dx10Sampler		("smp_nofilter");
		C.r_dx10Sampler		("smp_material");
		C.r_dx10Sampler		("smp_rtlinear");
		C.r_dx10Sampler		("smp_linear");

		C.r_End				();
		break;
	case 1:
		C.r_Pass("stub_notransform_t_scaled", "combine_distort", FALSE, FALSE, FALSE);

		C.r_dx10Texture("s_position", r2_RT_P);
		C.r_dx10Texture("s_image", r2_RT_generic0);
		C.r_dx10Texture("s_distort", r2_RT_generic1);

		C.r_dx10Sampler("smp_nofilter");
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
		break;
	case 2:
		C.r_Pass ("stub_notransform_aa_AA","combine_2",	FALSE,	FALSE,	FALSE);

		C.r_dx10Texture		("s_position",		r2_RT_P);
		C.r_dx10Texture		("s_normal",		r2_RT_N);
		C.r_dx10Texture		("s_bloom",			r2_RT_bloom1);
		C.r_dx10Texture		("s_image",			r2_RT_generic);
		C.r_dx10Texture		("s_tonemap",		r2_RT_luminance_cur);

		C.r_dx10Sampler		("smp_nofilter");
		C.r_dx10Sampler		("smp_rtlinear");
		C.r_End				();
		break;
	}
	RImplementation.clearAllShaderOptions();
}
