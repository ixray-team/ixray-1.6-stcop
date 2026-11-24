// BlenderDefault.cpp: implementation of the CBlender_LmEbB class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r1_blender_tex.h"
#include "../../xrEngine/EngineAPI.h"


#include "Blender_Lm(EbB).h"
#include "uber_deffer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_LmEbB::CBlender_LmEbB	()
{
	description.CLS		= B_LmEbB;
	description.version	= 0x1;
	xr_strcpy				(oT2_Name,	"$null");
	xr_strcpy				(oT2_xform,	"$null");
	oBlend.value		= false;
}

CBlender_LmEbB::~CBlender_LmEbB	()
{
}

void	CBlender_LmEbB::Save(	IWriter& fs )
{
	description.version	= 0x1;
	IBlender::Save		(fs);
	xrPWRITE_MARKER		(fs,"Environment map");
	xrPWRITE_PROP		(fs,"Name",				xrPID_TEXTURE,	oT2_Name);
	xrPWRITE_PROP		(fs,"Transform",		xrPID_MATRIX,	oT2_xform);
	xrPWRITE_PROP		(fs,"Alpha-Blend",		xrPID_BOOL,		oBlend);
}

void	CBlender_LmEbB::Load(	IReader& fs, u16 version )
{
	IBlender::Load	(fs,version);
	xrPREAD_MARKER	(fs);
	xrPREAD_PROP	(fs,xrPID_TEXTURE,	oT2_Name);
	xrPREAD_PROP	(fs,xrPID_MATRIX,	oT2_xform);
	if (version>=0x1)	{
		xrPREAD_PROP	(fs,xrPID_BOOL,	oBlend);
	}
}
#pragma todo("Hozar to ???: Cheak using this!")

//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
void CBlender_LmEbB::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if (C.L_textures.size()<2)	Debug.fatal	(DEBUG_INFO,"Not enought textures for shader, base tex: %s",*C.L_textures[0]);
	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
	case ShaderVariant::NormalLQ:

		if (oBlend.value)	C.r_Pass	("lmapE","lmapE",true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
		else				C.r_Pass	("lmapE","lmapE",true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", C.L_textures[1]);
		r1_tex(C, "s_hemi", *C.L_textures[2], true);
		r1_tex(C, "s_env", oT2_Name, true);
		C.r_End			();
		
		break;
	case ShaderVariant::Shadow:
		C.r_Pass("shadow_base", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	}
}
