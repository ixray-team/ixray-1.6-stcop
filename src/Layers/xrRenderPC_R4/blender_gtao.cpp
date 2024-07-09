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
    }
}