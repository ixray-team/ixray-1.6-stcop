#include "stdafx.h"
#include "BlenderGasMask.h"

CBlenderGasMask::CBlenderGasMask()
{
	description.CLS = 0;
}

void CBlenderGasMask::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if (C.iElement < 0 || C.iElement > 3)
	{
		return;
	}

	C.r_Pass("stub_screen_space", "effector_gasmask", FALSE, FALSE, FALSE);
	C.r_dx10Texture("s_image", r2_RT_generic);

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
	C.r_dx10Sampler("smp_rtlinear");
	C.r_End();
}