#include "stdafx.h"
#include "../xrRender/blender_fxaa.h"

CBlender_FXAA::CBlender_FXAA() {
    description.CLS = 0;
}

CBlender_FXAA::~CBlender_FXAA() = default;

void CBlender_FXAA::Compile(CBlender_Compile& C) {
    IBlender::Compile(C);

    switch (C.iElement) {
    case 0:
        C.r_Pass("fxaa_main", "fxaa_luma", false, FALSE, FALSE);
        C.r_Sampler("s_image", r2_RT_generic0);
        C.r_End();
        break;
    case 1:
        C.r_Pass("fxaa_main", "fxaa_main", false, FALSE, FALSE);
        C.r_Sampler("s_image", r2_RT_accum);
        C.r_End();
        break;
    }
}