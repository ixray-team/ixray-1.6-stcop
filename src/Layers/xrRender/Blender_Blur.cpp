#include "stdafx.h"


#include "Blender_Blur.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Blur::CBlender_Blur()
{
	description.CLS		= B_BLUR;
}

CBlender_Blur::~CBlender_Blur()
{

}

void	CBlender_Blur::Save	( IWriter& fs	)
{
	IBlender::Save	(fs);
}

void	CBlender_Blur::Load	( IReader& fs, u16 version	)
{
	IBlender::Load	(fs,version);
}

void CBlender_Blur::Compile	(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	C.r_Pass("r1_shadow_blur", "r1_shadow_blur", false, false, false);
	C.r_dx10Texture("s_base", C.L_textures[0]);
	C.r_dx10Sampler("smp_rtlinear");
	C.r_End();
}
