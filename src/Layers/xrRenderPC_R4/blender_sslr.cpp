#include "stdafx.h"
#include "blender_sslr.h"

CBlender_sslr::CBlender_sslr() { description.CLS = 0; }
CBlender_sslr::~CBlender_sslr() {}

void CBlender_sslr::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_ComputePass("sslr_render");

        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_surface", r2_RT_S);
        C.r_dx10Texture("s_normal", r2_RT_N);
        C.r_dx10Texture("s_diffuse", r2_RT_albedo);

        C.r_dx10Texture("s_image", r2_RT_generic);
        C.r_dx10Texture("s_velocity", r2_RT_velocity);
        C.r_dx10Texture("s_hiz", r2_RT_sslr_hiz);
        C.r_dx10Texture("s_occ", r2_RT_ssao_temp);

        C.r_dx10Texture("s_env_dist", r2_RT_env_temp);
        C.r_dx10Texture("s_env", r2_RT_env);

        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);
        C.r_dx10Texture("env_s0", r2_T_envs0);
        C.r_dx10Texture("env_s1", r2_T_envs1);

		C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");

        C.r_End();

        break;
    case 1:
		C.r_ComputePass("sslr_filter");

        C.r_dx10Texture("s_blue_noise", "shaders\\blue_noise_3x3");

        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_surface", r2_RT_S);
        C.r_dx10Texture("s_normal", r2_RT_N);
        C.r_dx10Texture("s_diffuse", r2_RT_albedo);

        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);
        C.r_dx10Texture("env_s0", r2_T_envs0);
        C.r_dx10Texture("env_s1", r2_T_envs1);

        C.r_dx10Texture("s_refl", r2_RT_sslr_data);

        C.r_dx10Texture("s_image", r2_RT_sslr_temp);
        C.r_dx10Texture("s_velocity", r2_RT_velocity);


        C.r_End();

        break;
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "combine_vslr", false, false, false);

        C.r_dx10Texture("sky_s0", r2_T_sky0);
        C.r_dx10Texture("sky_s1", r2_T_sky1);

        C.r_dx10Texture("s_env_dist", r2_RT_env_temp);
        C.r_dx10Texture("s_env", r2_RT_env);


        C.r_End();
        break;
    case 3:
        C.r_ComputePass("depth_pyramid");
        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_env_dist", r2_RT_env_temp);
        C.r_End();
        break;
    case 4:
        C.r_ComputePass("sslr_hiz_reduce");
        C.r_End();
        break;
    }
}