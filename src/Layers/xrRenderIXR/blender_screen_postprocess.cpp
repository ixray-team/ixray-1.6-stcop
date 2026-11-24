#include "stdafx.h"

#include "blender_screen_postprocess.h"

CBlender_SPP::CBlender_SPP() { description.CLS = 0; }
CBlender_SPP::~CBlender_SPP() {}

void CBlender_SPP::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
        case ScreenPostProcessType::Aberration:
        {
            C.r_Pass("stub_fullscreen_triangle", "chromatic_aberration", false, false, false);
            break;
        }
        case ScreenPostProcessType::Raindrops:
        {
            C.r_Pass("stub_fullscreen_triangle", "raindrops", false, false, false);
            C.r_dx10Texture("s_droplets", "shaders\\fx_hud_droplets");
            break;
        }
		case ScreenPostProcessType::ProceduralRaindrops:
		{
			C.r_Pass("stub_fullscreen_triangle", "procedural_raindrops", false, false, false);
			break;
		}
    }

    C.r_dx10Texture("s_image", r2_RT_backbuffer_final);


    C.r_End();
}