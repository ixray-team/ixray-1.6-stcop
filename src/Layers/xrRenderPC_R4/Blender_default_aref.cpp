// Blender_default_aref.cpp: implementation of the CBlender_default_aref class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "Blender_default_aref.h"
#include "uber_deffer.h"
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
	if (C.bEditor)
	{
		if(!!oBlend.value)
		{
			RImplementation.addShaderOption("FORWARD_ONLY", "1");
		}

		uber_deffer(C, true, "deffer_base", "deffer_base", !oBlend.value, nullptr, true);

		if(!!oBlend.value)
		{
			C.PassSET_Blend(true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);
		}

		C.r_End();
	} 
	else 
	{
		if (C.L_textures.size()<2)	Debug.fatal	(DEBUG_INFO,"Not enought textures for shader, base tex: %s",*C.L_textures[0]);
		switch (C.iElement)
		{
		case SE_R1_NORMAL_HQ:
			{
				const char*					sname	= "lmap";
				if (C.bDetail_Diffuse)	sname	= "lmap_dt";
				if (oBlend.value)	C.r_Pass	(sname,sname,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
				else				C.r_Pass	(sname,sname,true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,oAREF.value);
				r1_tex(C, "s_base", C.L_textures[0]);
				r1_tex(C, "s_lmap", C.L_textures[1]);
				r1_tex(C, "s_detail", C.detail_texture);
				r1_tex(C, "s_hemi", *C.L_textures[2], true);
				C.r_End		();
			}
			break;
		case SE_R1_NORMAL_LQ:
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
		case SE_R1_LPOINT:
			if (!oBlend.value)	
			{
				C.r_Pass		("lmap_point","add_point",false,true,false,true,D3DBLEND_ONE,D3DBLEND_ONE,true,oAREF.value);
				r1_tex(C, "s_base", C.L_textures[0]);
				r1_tex(C, "s_lmap", TEX_POINT_ATT, true);
				r1_tex(C, "s_att", TEX_POINT_ATT, true);
				C.r_End			();
			}
			break;
		case SE_R1_LSPOT:
			if (!oBlend.value)	
			{
				C.r_Pass		("lmap_spot","add_spot",false,true,false,true,D3DBLEND_ONE,D3DBLEND_ONE,true,oAREF.value);
				r1_tex(C, "s_base", C.L_textures[0]);
				r1_tex(C, "s_lmap", "internal\\internal_light_att", true, true);
				r1_tex(C, "s_att", TEX_SPOT_ATT, true);
				C.r_End			();
			}
			break;
		case SE_R1_LMODELS:
			// Lighting only, not use alpha-channel
			C.r_Pass		("lmap_l","lmap_l",false);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_hemi", *C.L_textures[2], true);
			C.r_End			();
			break;
		}
	}
}
