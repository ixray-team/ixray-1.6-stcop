#include "stdafx.h"


#include "blender_ssao.h"

CBlender_SSAO::CBlender_SSAO	()	{	description.CLS		= 0;	}
CBlender_SSAO::~CBlender_SSAO	()	{	}

void	CBlender_SSAO::Compile			(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	switch (C.iElement)
	{
		case 0:		// calculate SSAO
		{
			C.r_Pass("combine_1", "ssao_calc", FALSE, FALSE, FALSE);
			C.r_Stencil(TRUE, D3DCMP_LESSEQUAL, 0xFF);
			C.r_StencilRef(0x01);
			C.r_CullMode(D3DCULL_NONE);

			C.r_dx10Texture("s_position", r2_RT_P);
			C.r_dx10Texture("s_normal", r2_RT_N);
			C.r_dx10Texture("s_half_depth", r2_RT_half_depth);

			jitter(C);

			C.r_dx10Sampler("smp_nofilter");
			C.r_dx10Sampler("smp_material");
			C.r_dx10Sampler("smp_rtlinear");
			C.r_End();
			break;
		}
		case 1:		// depth downsample for HBAO
		{
			C.r_Pass("combine_1", "depth_downs", FALSE, FALSE, FALSE);
			C.r_CullMode(D3DCULL_NONE);

			C.r_dx10Texture("s_position", r2_RT_P);
			C.r_dx10Texture("s_normal", r2_RT_N);

			C.r_dx10Sampler("smp_nofilter");
			C.r_dx10Sampler("smp_material");
			C.r_dx10Sampler("smp_rtlinear");
			C.r_End();
			break;
		}
		case 2: 
		{
			C.r_Pass("stub_fullscreen_triangle", "ssgi_render", FALSE, FALSE, FALSE);
			C.r_dx10Texture("s_position", r2_RT_P);
			C.r_dx10Texture("s_surface", r2_RT_S);
			C.r_dx10Texture("s_normal", r2_RT_N);
			C.r_dx10Texture("s_diffuse", r2_RT_albedo);

			C.r_dx10Texture("sky_s0", r2_T_sky0);
			C.r_dx10Texture("sky_s1", r2_T_sky1);
			C.r_dx10Texture("env_s0", r2_T_envs0);
			C.r_dx10Texture("env_s1", r2_T_envs1);

			C.r_dx10Texture("s_env", r2_RT_env);
			C.r_dx10Texture("s_env_depth", r2_RT_env_depth);

			C.r_dx10Texture("s_image", r2_RT_generic);
			C.r_dx10Texture("s_velocity", r2_RT_velocity);

			C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");

			C.r_dx10Sampler("smp_linear");
			C.r_dx10Sampler("smp_rtlinear");
			C.r_dx10Sampler("smp_nofilter");

			C.r_End();

			break;
		}
		case 3: 
		{
			C.r_Pass("stub_fullscreen_triangle", "ssgi_filter", FALSE, FALSE, FALSE);
			C.r_dx10Texture("s_position", r2_RT_P);
			C.r_dx10Texture("s_surface", r2_RT_S);
			C.r_dx10Texture("s_normal", r2_RT_N);
			C.r_dx10Texture("s_diffuse", r2_RT_albedo);

			C.r_dx10Texture("s_base", r2_RT_ssgi_old);
			C.r_dx10Texture("s_image", r2_RT_sslr_temp);

			C.r_dx10Texture("s_velocity", r2_RT_velocity);

			C.r_dx10Sampler("smp_linear");
			C.r_dx10Sampler("smp_rtlinear");
			C.r_dx10Sampler("smp_nofilter");

			C.r_End();

			break;
		}
	}
}
