// BlenderDefault.cpp: implementation of the CBlender_default class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "BlenderDefault.h"
#include "uber_deffer.h"
#include "r1_blender_tex.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_default::CBlender_default	()
{
	description.CLS		        = B_DEFAULT;
	description.version         = 1;
	oTessellation.Count         = 4;
	oTessellation.IDselected	= 0;
}

CBlender_default::~CBlender_default	()
{

}
void	CBlender_default::Save(	IWriter& fs )
{
	IBlender::Save	(fs);
	xrP_TOKEN::Item	I;
	xrPWRITE_PROP	(fs,"Tessellation",	xrPID_TOKEN, oTessellation);
	I.ID = 0; xr_strcpy(I.str,"NO_TESS");	fs.w		(&I,sizeof(I));
	I.ID = 1; xr_strcpy(I.str,"TESS_PN");	fs.w		(&I,sizeof(I));
	I.ID = 2; xr_strcpy(I.str,"TESS_HM");	fs.w		(&I,sizeof(I));
	I.ID = 3; xr_strcpy(I.str,"TESS_PN+HM");	fs.w		(&I,sizeof(I));
}
void	CBlender_default::Load(	IReader& fs, u16 version )
{
	IBlender::Load	(fs,version);
	if (version>0)
	{
		xrPREAD_PROP(fs,xrPID_TOKEN,oTessellation);
	}
}
void CBlender_default::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);
	
	if (C.L_textures.size() < 3)
	{
		Debug.fatal(DEBUG_INFO, "Not enought textures for shader, base tex: %s", *C.L_textures[0]);
		return;
	}

	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
		// Level view
		if (C.bDetail_Diffuse)
		{
			const R1Bump Bump(C, "lmap_dt");
			C.r_Pass	(Bump.Name, Bump.Name, true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_detail", C.detail_texture);
			r1_tex(C, "s_hemi", *C.L_textures[2], true);
			Bump.Bind(C);
			C.r_End		();
		} else
		{
			const R1Bump Bump(C, "lmap");
			C.r_Pass	(Bump.Name, Bump.Name, true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_hemi", *C.L_textures[2], true);
			Bump.Bind(C);
			C.r_End		();
		}
		break;
	case ShaderVariant::NormalLQ:
		C.r_Pass		("lmap","lmap",true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", C.L_textures[1]);
		r1_tex(C, "s_hemi", *C.L_textures[2], true);
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
