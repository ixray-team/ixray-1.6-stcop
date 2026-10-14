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

	C.r_Pass("r1_shadow_world", "r1_shadow_world", false, true, false, true, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO);

	VERIFY(C.L_textures.size() > 0);
	C.r_dx10Texture("s_base", C.L_textures[0]);
	C.r_dx10Sampler("smp_base");

	C.PassSET_ZB(true, false);
	C.PassSET_Blend(true, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO, false, 0);
	C.PassSET_LightFog(false, false);

	C.r_End();
}
