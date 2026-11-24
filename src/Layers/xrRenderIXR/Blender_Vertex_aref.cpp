// Blender_Vertex_aref.cpp: implementation of the CBlender_Vertex_aref class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "Blender_Vertex_aref.h"
#include "r1_blender_tex.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Vertex_aref::CBlender_Vertex_aref()
{
	description.CLS		= B_VERT_AREF;
	description.version	= 1;
	oAREF.value			= 32;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oBlend.value		= false;
}

CBlender_Vertex_aref::~CBlender_Vertex_aref()
{

}

void	CBlender_Vertex_aref::Save(	IWriter& fs )
{
	IBlender::Save		(fs);
	xrPWRITE_PROP		(fs,"Alpha ref",	xrPID_INTEGER,	oAREF);
	xrPWRITE_PROP		(fs,"Alpha-blend",	xrPID_BOOL,		oBlend);
}

void	CBlender_Vertex_aref::Load(	IReader& fs, u16 version )
{
	IBlender::Load		(fs,version);

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

void CBlender_Vertex_aref::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	
	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
		// Level view
		{
			const char*					sname	= "vert";
			if (C.bDetail_Diffuse)	sname	= "vert_dt";
			if (oBlend.value)	C.r_Pass(sname, sname, true, true, false, true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, oAREF.value);
			else				C.r_Pass(sname, sname, true, true, true, true, D3DBLEND_ONE, D3DBLEND_ZERO, true, oAREF.value);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_detail", C.detail_texture);
			C.r_End			();
		}
		break;
	case ShaderVariant::NormalLQ:
		// Level view
		{
			const char*				sname		= "vert";
			if (oBlend.value)	C.r_Pass(sname, sname, true, true, false, true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, oAREF.value);
			else				C.r_Pass(sname, sname, true, true, true, true, D3DBLEND_ONE, D3DBLEND_ZERO, true, oAREF.value);
			r1_tex(C, "s_base", C.L_textures[0]);
			C.r_End			();
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
