/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "blender_bloom_upsample.h"

CBlender_bloom_upsample::CBlender_bloom_upsample() { description.CLS = 0; }
CBlender_bloom_upsample::~CBlender_bloom_upsample() {}

void CBlender_bloom_upsample::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
		
        C.r_dx10Texture("s_image", r2_RT_bloomG);
        C.r_dx10Texture("t_image", r2_RT_bloomF);


        C.r_End();

        break;
    case 1:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
		
        C.r_dx10Texture("s_image", r2_RT_bloomF2);
        C.r_dx10Texture("t_image", r2_RT_bloomE);


        C.r_End();

        break;
    case 2:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
		
        C.r_dx10Texture("s_image", r2_RT_bloomE2);
        C.r_dx10Texture("t_image", r2_RT_bloomD);


        C.r_End();

        break;
    case 3:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
		
        C.r_dx10Texture("s_image", r2_RT_bloomD2);
        C.r_dx10Texture("t_image", r2_RT_bloomC);


        C.r_End();

        break;
    case 4:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
		
        C.r_dx10Texture("s_image", r2_RT_bloomC2);
        C.r_dx10Texture("t_image", r2_RT_bloomB);


        C.r_End();

        break;
    case 5:
        C.r_Pass("stub_fullscreen_triangle", "bloom_upsample", false, false, false);
		
        C.r_dx10Texture("s_image", r2_RT_bloomB2);
        C.r_dx10Texture("t_image", r2_RT_bloomA);


        C.r_End();

        break;
    }
}