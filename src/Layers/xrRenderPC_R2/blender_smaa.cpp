#include "stdafx.h"
#include "../xrRender/blender_smaa.h"

CBlender_SMAA::CBlender_SMAA() { description.CLS = 0; }
CBlender_SMAA::~CBlender_SMAA() {}

void CBlender_SMAA::Compile(CBlender_Compile& C) {
    IBlender::Compile(C);

    switch (C.iElement) {
    case 0:
        C.r_Pass("null", "smaa_edge_detect", FALSE, FALSE, FALSE);

        C.r_Sampler_clf("s_image", r2_RT_generic0);

        C.r_End();
        break;
    case 1:
        C.r_Pass("null", "smaa_bweight_calc", FALSE, FALSE, FALSE);

        C.r_Sampler("s_edgetex", r2_RT_smaa_edgetex, false, D3DTADDRESS_CLAMP);
        C.r_Sampler("s_areatex", "smaa\\smaa_area_tex_dx9", false, D3DTADDRESS_CLAMP);
        C.r_Sampler("s_searchtex", "smaa\\smaa_search_tex", false, D3DTADDRESS_CLAMP, D3DTEXF_POINT, D3DTEXF_POINT, D3DTEXF_POINT);

        C.r_End();

        break;
    case 2:
        C.r_Pass("null", "smaa_neighbour_blend", FALSE, FALSE, FALSE);

        C.r_Sampler_clf("s_image", r2_RT_generic0);
        C.r_Sampler("s_blendtex", r2_RT_smaa_blendtex, false, D3DTADDRESS_CLAMP);

        C.r_End();

        break;
    }
}
