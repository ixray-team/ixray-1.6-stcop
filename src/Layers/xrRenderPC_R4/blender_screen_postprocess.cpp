#include "stdafx.h"

#include "../xrRender/blender_screen_postprocess.h"
#include "../xrRender/uber_deffer.h"

CBlender_SPP::CBlender_SPP() { description.CLS = 0; }
CBlender_SPP::~CBlender_SPP() {}

void CBlender_SPP::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    if (C.iElement == ScreenPostProcessType::Winter)
    {
        static auto texture_name = pSettings->section_exist("custom_textures")
            && pSettings->line_exist("custom_textures", "snow_texture")
            ? pSettings->r_string("custom_textures", "snow_texture") : "shaders\\snowmask\\snow";

        C.L_textures.resize(std::max(C.L_textures.size(), (size_t)1));
        C.L_textures[0]._set(texture_name);

        uber_deffer(C, true, "stub_screen_space", "snowing_main", FALSE, NULL, true);

        C.PassSET_ZB(FALSE, FALSE);
        C.PassSET_Blend(TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);

        C.r_dx10Texture("s_position", r2_RT_P);
        C.r_dx10Texture("s_normal", r2_RT_N"_temp");
        C.r_dx10Texture("s_surface", r2_RT_S"_temp");
        C.r_dx10Texture("s_diffuse", r2_RT_albedo);

        C.r_dx10Sampler("smp_nofilter");
        C.r_dx10Sampler("smp_rtlinear");

        C.r_End();
        return;
    }

    switch (C.iElement)
    {
        case ScreenPostProcessType::Vignette:
            C.r_Pass("stub_screen_space", "vignette", FALSE, FALSE, FALSE);
            break;
        case ScreenPostProcessType::Aberration:
            C.r_Pass("stub_screen_space", "chromatic_aberration", FALSE, FALSE, FALSE);
            break;
        case ScreenPostProcessType::Saturation:
            C.r_Pass("stub_screen_space", "saturation", FALSE, FALSE, FALSE);
            break;
        case ScreenPostProcessType::Raindrops:
        {
            C.r_Pass("stub_screen_space", "raindrops", FALSE, FALSE, FALSE);
            C.r_dx10Texture("s_droplets", "shaders\\fx_hud_droplets");
            break;
        }
    }

    C.r_dx10Texture("s_image", r2_RT_backbuffer_final);

    C.r_dx10Sampler("smp_nofilter");
    C.r_dx10Sampler("smp_rtlinear");

    C.r_End();
}