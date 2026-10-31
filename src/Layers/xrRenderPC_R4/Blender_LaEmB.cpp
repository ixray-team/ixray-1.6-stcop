// BlenderDefault.cpp: implementation of the CBlender_LaEmB class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "Blender_LaEmB.h"
#include "r1_blender_tex.h"
#include "uber_deffer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_LaEmB::CBlender_LaEmB	()
{
	description.CLS		= B_LaEmB;
	xr_strcpy				(oT2_Name,	"$null");
	xr_strcpy				(oT2_xform,	"$null");
	xr_strcpy				(oT2_const,	"$null");
}

CBlender_LaEmB::~CBlender_LaEmB	()
{
	
}

void	CBlender_LaEmB::Save(	IWriter& fs )
{
	IBlender::Save	(fs);
	xrPWRITE_MARKER	(fs,"Environment map");
	xrPWRITE_PROP	(fs,"Name",				xrPID_TEXTURE,	oT2_Name);
	xrPWRITE_PROP	(fs,"Transform",		xrPID_MATRIX,	oT2_xform);
	xrPWRITE_PROP	(fs,"Constant",			xrPID_CONSTANT,	oT2_const);
}

void	CBlender_LaEmB::Load(	IReader& fs, u16 version )
{
	IBlender::Load	(fs,version);
	xrPREAD_MARKER	(fs);
	xrPREAD_PROP	(fs,xrPID_TEXTURE,	oT2_Name);
	xrPREAD_PROP	(fs,xrPID_MATRIX,	oT2_xform);
	xrPREAD_PROP	(fs,xrPID_CONSTANT,	oT2_const);
}

void	CBlender_LaEmB::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	if (C.bEditor)
	{
		uber_deffer(C, true, "deffer_base", "deffer_base", false, 0, true);
		C.r_End();
		return;
	}
	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
	case ShaderVariant::NormalLQ:
	{
		const bool emission = _stricmp(oT2_Name, "$null") != 0;
		if (emission)
			C.AddDefine("USE_R1_EMISSION", "1");
		if (C.L_textures.size() >= 3)
		{
			C.r_Pass("lmap", "lmap", true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_hemi", *C.L_textures[2], true);
		}
		else
		{
			C.r_Pass("vert", "vert", true);
			r1_tex(C, "s_base", C.L_textures[0]);
		}
		if (emission)
		{
			C.r_dx10Texture("s_emission", oT2_Name);
			C.Defines.clear();
		}
		C.r_End();
		break;
	}
	case ShaderVariant::PointLight:
		C.r_Pass("lmap_point", "add_point", false, true, false, true, D3DBLEND_ONE, D3DBLEND_ONE, true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", TEX_POINT_ATT, true);
		r1_tex(C, "s_att", TEX_POINT_ATT, true);
		C.r_End();
		break;
	case ShaderVariant::SpotLight:
		C.r_Pass("lmap_spot", "add_spot", false, true, false, true, D3DBLEND_ONE, D3DBLEND_ONE, true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", "internal\\internal_light_att", true, true);
		r1_tex(C, "s_att", TEX_SPOT_ATT, true);
		C.r_End();
		break;
	case ShaderVariant::ModelLighting:
		C.r_Pass("lmap_l", "lmap_l", false);
		r1_tex(C, "s_base", C.L_textures[0]);
		if (C.L_textures.size() >= 2)
			r1_tex(C, "s_lmap", C.L_textures[1]);
		C.r_End();
		break;
	}
}
