#include "stdafx.h"
#include "blender_gtao.h"

CBlender_gtao::CBlender_gtao() { description.CLS = 0; }
CBlender_gtao::~CBlender_gtao() {}

void CBlender_gtao::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "gtao_render", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_half_depth", r2_RT_half_depth);
        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_normal", r2_RT_N);
		C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 1:
        C.r_Pass("stub_fullscreen_triangle", "gtao_filter", FALSE, FALSE, FALSE);
        C.r_dx10Texture("t_gtao_packed", "$user$gtao_0");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "sslr_render", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_surface", r2_RT_S);
        C.r_dx10Texture("s_normal", r2_RT_N);
        C.r_dx10Texture("s_diffuse", r2_RT_albedo);

        C.r_dx10Texture("s_image", r2_RT_generic);
        C.r_dx10Texture("s_velocity", r2_RT_velocity);
        C.r_dx10Texture("s_half_depth", r2_RT_half_depth);

        C.r_dx10Texture("s_env", r2_RT_env);
        C.r_dx10Texture("s_env_depth", r2_RT_env_depth);
        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);
        C.r_dx10Texture("env_s0", r2_T_envs0);
        C.r_dx10Texture("env_s1", r2_T_envs1);
		C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");
        C.r_dx10Sampler("smp_linear");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 3:
        C.r_Pass("stub_fullscreen_triangle", "sslr_filter", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_surface", r2_RT_S);
        C.r_dx10Texture("s_normal", r2_RT_N);
        C.r_dx10Texture("s_diffuse", r2_RT_albedo);

        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);
        C.r_dx10Texture("env_s0", r2_T_envs0);
        C.r_dx10Texture("env_s1", r2_T_envs1);

        C.r_dx10Texture("s_refl", r2_RT_sslr);
        C.r_dx10Texture("s_env", r2_RT_env);
        C.r_dx10Texture("s_env_depth", r2_RT_env_depth);

        C.r_dx10Texture("s_image", r2_RT_generic);
        C.r_dx10Texture("s_velocity", r2_RT_velocity);

        C.r_dx10Sampler("smp_linear");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 4:
        C.r_Pass("stub_fullscreen_triangle", "sslr_temporal", FALSE, FALSE, FALSE);
        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_surface", r2_RT_S);
        C.r_dx10Texture("s_normal", r2_RT_N);
        C.r_dx10Texture("s_diffuse", r2_RT_albedo);

        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);
        C.r_dx10Texture("env_s0", r2_T_envs0);
        C.r_dx10Texture("env_s1", r2_T_envs1);

        C.r_dx10Texture("s_refl", r2_RT_sslr_old);
        C.r_dx10Texture("s_env", r2_RT_env);

        C.r_dx10Texture("s_image", r2_RT_sslr_temp);
        C.r_dx10Texture("s_velocity", r2_RT_velocity);

        C.r_dx10Sampler("smp_linear");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    case 5:
        C.r_Pass("stub_fullscreen_triangle", "vslr_combine", FALSE, FALSE, FALSE);

        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);

        C.r_dx10Texture("s_env_depth", r2_RT_env_depth);
        C.r_dx10Texture("s_env", r2_RT_env_temp);

        C.r_dx10Sampler("smp_linear");
        C.r_dx10Sampler("smp_rtlinear");
        C.r_dx10Sampler("smp_nofilter");

        C.r_End();

        break;
    }
}