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