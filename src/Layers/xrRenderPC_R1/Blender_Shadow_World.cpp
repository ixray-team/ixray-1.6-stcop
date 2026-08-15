#include "stdafx.h"


#include "Blender_Shadow_World.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_ShWorld::CBlender_ShWorld()
{
	description.CLS		= B_SHADOW_WORLD;
}

CBlender_ShWorld::~CBlender_ShWorld()
{

}

void	CBlender_ShWorld::Save	( IWriter& fs	)
{
	IBlender::Save	(fs);
}

void	CBlender_ShWorld::Load	( IReader& fs, u16 version	)
{
	IBlender::Load	(fs,version);
}

void CBlender_ShWorld::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

#ifdef USE_DX11
	C.r_Pass("stub_default", "stub_default", false);

	VERIFY(C.L_textures.size() > 0);
	C.r_dx10Texture("s_base", C.L_textures[0]);
	C.r_dx10Sampler("smp_base");

	C.PassSET_ZB(true, false);
	C.PassSET_Blend(true, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO, false, 0);
	C.PassSET_LightFog(false, false);

	C.r_End();
#else
	C.PassBegin();
	{
		C.PassSET_ZB(true, false);
		C.PassSET_Blend_MUL();
		C.PassSET_LightFog(false, false);

		// Stage0 - Base texture
		C.StageBegin();
		C.StageSET_Color(D3DTA_TEXTURE, D3DTOP_ADD, D3DTA_DIFFUSE);
		C.StageSET_Alpha(D3DTA_TEXTURE, D3DTOP_ADD, D3DTA_DIFFUSE);
		C.Stage_Texture("$base0");
		C.Stage_Matrix("$null", 0);
		C.Stage_Constant("$null");
		C.StageEnd();
	}
	C.PassEnd();
#endif
}
