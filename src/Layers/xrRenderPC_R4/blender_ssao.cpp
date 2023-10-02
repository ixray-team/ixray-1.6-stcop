#include "stdafx.h"
#pragma hdrstop

#include "Blender_SSAO.h"

CBlender_SSAO::CBlender_SSAO	()	{	description.CLS		= 0;	}
CBlender_SSAO::~CBlender_SSAO	()	{	}

void	CBlender_SSAO::Compile			(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	switch (C.iElement)
	{
	case 0:		// calculate SSAO
		C.r_Pass			("combine_1",		"ssao_calc",	FALSE,	FALSE,	FALSE);
		C.r_Stencil			(TRUE, D3DCMP_LESSEQUAL, 0xFF);	// stencil should be >= 1
		C.r_StencilRef		(0x01);
		C.r_CullMode		(D3DCULL_NONE);

		C.r_dx10Texture		("s_position",	r2_RT_P);
		C.r_dx10Texture		("s_normal",	r2_RT_N);
		C.r_dx10Texture		("s_tonemap",	r2_RT_luminance_cur	);
		C.r_dx10Texture		("s_half_depth",r2_RT_half_depth	);

		jitter(C);

		C.r_dx10Sampler		("smp_nofilter");
		C.r_dx10Sampler		("smp_material");
		C.r_dx10Sampler		("smp_rtlinear");
		C.r_End				();
		break;
	case 1:		// depth downsample for HBAO
		C.r_Pass			("combine_1",		"depth_downs",	FALSE,	FALSE,	FALSE);
//		C.r_Stencil			(TRUE, D3DCMP_LESSEQUAL, 0xFF);	// stencil should be >= 1
//		C.r_StencilRef		(0x01);
		C.r_CullMode		(D3DCULL_NONE);

		C.r_dx10Texture		("s_position",	r2_RT_P);
		C.r_dx10Texture		("s_normal",	r2_RT_N);
		C.r_dx10Texture		("s_tonemap",	r2_RT_luminance_cur	);

		C.r_dx10Sampler		("smp_nofilter");
		C.r_dx10Sampler		("smp_material");
		C.r_dx10Sampler		("smp_rtlinear");
		C.r_End				();
		break;
	}
}
