#include "stdafx.h"


#include "Blender_Screen_GRAY.h"

// Y =  0.299*Red+0.587*Green+0.114*Blue
// Y =  76.544*R +150.272*G  +29.184*B
// Y =  76(4C)    150(96)     29(1D)
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Screen_GRAY::CBlender_Screen_GRAY()
{
	description.CLS		= B_SCREEN_GRAY;
}

CBlender_Screen_GRAY::~CBlender_Screen_GRAY()
{

}

void	CBlender_Screen_GRAY::Save	( IWriter& fs	)
{
	IBlender::Save	(fs);
}

void	CBlender_Screen_GRAY::Load	( IReader& fs, u16 version	)
{
	IBlender::Load	(fs,version);
}

void	CBlender_Screen_GRAY::Compile	(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	C.r_Pass("stub_notransform_t", "stub_default", false);
	if (C.L_textures.size())
		C.r_dx10Texture("s_base", C.L_textures[0]);
	C.PassSET_ZB(false, false);
	C.r_End();
}
