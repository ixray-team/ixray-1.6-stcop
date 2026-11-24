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
#include "BlenderGasMask.h"

CBlenderGasMask::CBlenderGasMask(VisorGlassType GlassType)
	: GlassType(GlassType)
{
	description.CLS = 0;
}

void CBlenderGasMask::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if (GlassType == VisorGlassType::Flat)
	{
		if (C.iElement != 0)
		{
			return;
		}

		C.r_Pass("stub_fullscreen_triangle", "flat_gasmask", false, false, false);
		C.r_dx10Texture("s_image", r2_RT_backbuffer_final);
		C.r_dx10Texture("s_droplets", "shaders\\fx_hud_droplets");
		for (u32 Index = 1; Index <= 10; ++Index)
		{
			string64 SamplerName;
			string128 TextureName;
			xr_sprintf(SamplerName, "s_mask_nm_%u", Index);
			xr_sprintf(TextureName, "shaders\\gasmasks\\mask_nm_%u", Index);
			C.r_dx10Texture(SamplerName, TextureName);
		}
		C.r_End();
		return;
	}

	if (C.iElement < 0 || C.iElement > 3)
	{
		return;
	}

	C.r_Pass("stub_fullscreen_triangle", "effector_gasmask", false, false, false);
	C.r_dx10Texture("s_image", r2_RT_backbuffer_final);

	shared_str TexturePassName;
	switch (C.iElement)
	{
		case 0: TexturePassName = "shaders\\gasmask\\Good"; break;
		case 1: TexturePassName = "shaders\\gasmask\\Medium"; break;
		case 2: TexturePassName = "shaders\\gasmask\\Bad"; break;
		case 3: TexturePassName = "shaders\\gasmask\\Broken"; break;
	}

	C.r_dx10Texture("s_breath", "shaders\\gasmask\\Condensation");
	C.r_dx10Texture("s_gasmask", TexturePassName);
	C.r_End();
}