// Blender_Vertex_aref.cpp: implementation of the CBlender_Detail_Still class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r1_blender_tex.h"
#include "../../xrEngine/EngineAPI.h"


#include "Blender_detail_still.h"
#include "uber_deffer.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Detail_Still::CBlender_Detail_Still()
{
	description.CLS		= B_DETAIL;
	description.version	= 0;
}

CBlender_Detail_Still::~CBlender_Detail_Still()
{

}

void	CBlender_Detail_Still::Save		(IWriter& fs )
{
	IBlender::Save		(fs);
	xrPWRITE_PROP		(fs,"Alpha-blend",	xrPID_BOOL,		oBlend);
}

void	CBlender_Detail_Still::Load		(IReader& fs, u16 version )
{
	IBlender::Load		(fs,version);
	xrPREAD_PROP		(fs,xrPID_BOOL,		oBlend);
}

//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
#include "uber_deffer.h"
void CBlender_Detail_Still::Compile	(CBlender_Compile& C)
{
	IBlender::Compile(C);

	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
		C.r_Pass("detail_wave", "detail", true, true, true, false, D3DBLEND_ONE, D3DBLEND_ZERO, oBlend.value ? true : false, oBlend.value ? 200 : 0);
		r1_tex(C, "s_base", C.L_textures[0]);
		C.r_End		();
		break;
	case ShaderVariant::NormalLQ:
		C.r_Pass("detail_still", "detail", true, true, true, false, D3DBLEND_ONE, D3DBLEND_ZERO, oBlend.value ? true : false, oBlend.value ? 200 : 0);
		r1_tex(C, "s_base", C.L_textures[0]);
		C.r_End		();
		break;
	case ShaderVariant::ModelLighting:
		break;
	}
}
