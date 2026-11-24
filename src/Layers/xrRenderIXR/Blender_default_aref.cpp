// Blender_default_aref.cpp: implementation of the CBlender_default_aref class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "Blender_default_aref.h"
#include "r1_blender_tex.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_default_aref::CBlender_default_aref()
{
	description.CLS		= B_DEFAULT_AREF;
	description.version	= 1;
	oAREF.value			= 32;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oBlend.value		= false;
}

CBlender_default_aref::~CBlender_default_aref()
{

}

void	CBlender_default_aref::Save(IWriter& fs)
{
	IBlender::Save	(fs);
	xrPWRITE_PROP	(fs,"Alpha ref",	xrPID_INTEGER,	oAREF);
	xrPWRITE_PROP	(fs,"Alpha-blend",	xrPID_BOOL,		oBlend);
}

void	CBlender_default_aref::Load(	IReader& fs , u16 version)
{
	IBlender::Load	(fs,version);

	switch (version)	
	{
	case 0: 
		xrPREAD_PROP	(fs,xrPID_INTEGER,	oAREF);
		oBlend.value	= false;
		break;
	case 1:
	default:
		xrPREAD_PROP	(fs,xrPID_INTEGER,	oAREF);
		xrPREAD_PROP	(fs,xrPID_BOOL,		oBlend);
		break;
	}
}

void CBlender_default_aref::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);
	if (C.L_textures.size()<2)	Debug.fatal	(DEBUG_INFO,"Not enought textures for shader, base tex: %s",*C.L_textures[0]);
	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
		{
			const R1Bump Bump(C, C.bDetail_Diffuse ? "lmap_dt" : "lmap");
			const char*					sname	= Bump.Name;
			if (oBlend.value)	C.r_Pass	(sname,sname,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
			else				C.r_Pass	(sname,sname,true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,oAREF.value);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_detail", C.detail_texture);
			r1_tex(C, "s_hemi", *C.L_textures[2], true);
			Bump.Bind(C);
			C.r_End		();
		}
		break;
	case ShaderVariant::NormalLQ:
		{
			const char*					sname	= "lmap";
			if (oBlend.value)	C.r_Pass	(sname,sname,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
			else				C.r_Pass	(sname,sname,true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,oAREF.value);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_hemi", *C.L_textures[2], true);
			C.r_End		();
		}
		break;
	case ShaderVariant::Shadow:
		C.AddDefine("USE_AREF", "1");
		C.r_Pass("shadow_base", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	}
}
