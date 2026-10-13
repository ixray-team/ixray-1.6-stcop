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
	case SE_R1_NORMAL_HQ:
	case SE_R1_NORMAL_LQ:
	{
		const bool emission = _stricmp(oT2_Name, "$null") != 0;
		if (emission)
			RImplementation.addShaderOption("USE_R1_EMISSION", "1");
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
			RImplementation.clearAllShaderOptions();
		}
		C.r_End();
		break;
	}
	case SE_R1_LPOINT:
		C.r_Pass("lmap_point", "add_point", false, true, false, true, D3DBLEND_ONE, D3DBLEND_ONE, true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", TEX_POINT_ATT, true);
		r1_tex(C, "s_att", TEX_POINT_ATT, true);
		C.r_End();
		break;
	case SE_R1_LSPOT:
		C.r_Pass("lmap_spot", "add_spot", false, true, false, true, D3DBLEND_ONE, D3DBLEND_ONE, true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", "internal\\internal_light_att", true, true);
		r1_tex(C, "s_att", TEX_SPOT_ATT, true);
		C.r_End();
		break;
	case SE_R1_LMODELS:
		C.r_Pass("lmap_l", "lmap_l", false);
		r1_tex(C, "s_base", C.L_textures[0]);
		if (C.L_textures.size() >= 2)
			r1_tex(C, "s_lmap", C.L_textures[1]);
		C.r_End();
		break;
	}
}

// EDITOR --- NO CONSTANT
void CBlender_LaEmB::compile_ED	(CBlender_Compile& C)
{
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(true,true);
		
		// Stage1 - Env texture
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_DIFFUSE);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_DIFFUSE);
		C.StageSET_TMC			(oT2_Name, oT2_xform, "$null", 0);
		C.StageEnd				();
		
		// Stage2 - Base texture
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_CURRENT);
		C.StageSET_TMC			(oT_Name, oT_xform, "$null", 0);
		C.StageEnd				();
	}
	C.PassEnd			();
}

// EDITOR --- WITH CONSTANT
void CBlender_LaEmB::compile_EDc	(CBlender_Compile& C)
{
	// Pass0 - (lmap+env*const)
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(true,true);
		
		// Stage1 - Env texture * constant
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_TMC			(oT2_Name, oT2_xform, oT2_const, 0);
		C.StageEnd				();
		
		// Stage2 - Diffuse color
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_DIFFUSE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_DIFFUSE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.Stage_Texture			("$null");
		C.Stage_Matrix			("$null",0);
		C.Stage_Constant		("$null");
		C.StageEnd				();
	}
	C.PassEnd			();
	
	// Pass1 - *base
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,false);
		C.PassSET_Blend_MUL		();
		C.PassSET_LightFog		(false,true);
		
		// Stage2 - Diffuse color
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		C.StageSET_TMC			(oT_Name, oT_xform, "$null",0);
		C.StageEnd				();
	}
	C.PassEnd			();
}

//
void CBlender_LaEmB::compile_2	(CBlender_Compile& C)
{
	// Pass1 - Lmap+Env
	C.PassBegin			();
	{
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(false,true);
		
		// Stage0 - Lightmap
		C.StageBegin			();
		C.StageEnd				();
		
		// Stage1 - Environment map
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_TMC			(oT2_Name, oT2_xform, "$null",0);
		C.StageEnd				();
	}
	C.PassEnd			();
	
	// Pass2 - Base map
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,false);
		C.PassSET_Blend_MUL2X	();
		C.PassSET_LightFog		(false,true);
		
		// Stage0 - Base
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		C.StageSET_TMC			(oT_Name, oT_xform, "$null",0);
		C.StageEnd				();
	}
	C.PassEnd			();
}
//
void CBlender_LaEmB::compile_2c	(CBlender_Compile& C)
{
	C.PassBegin		();
	{	
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(false,true);
		
		// Stage0 - Environment map [*] const
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_TMC			(oT2_Name,oT2_xform,oT2_const,0);
		C.StageEnd				();

		// Stage1 - [+] Lightmap
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_TMC			("$base1","$null","$null",1);
		C.StageEnd				();
	}
	C.PassEnd			();
	
	// Pass2 - Base map
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,false);
		C.PassSET_Blend_MUL2X	();
		C.PassSET_LightFog		(false,true);
		
		// Stage0 - Detail
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		C.StageSET_TMC			(oT_Name, oT_xform, "$null",0);
		C.StageEnd				();
	}
	C.PassEnd			();
}

//
void CBlender_LaEmB::compile_3	(CBlender_Compile& C)
{
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(false,true);
		
		// Stage0 - [=] Lightmap
		C.StageBegin			();
		C.StageEnd				();
		
		// Stage1 - [+] Env-map
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_TMC			(oT2_Name,oT2_xform,"$null",0);
		C.StageEnd				();
		
		// Stage2 - [*] Base
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE2X,	D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE2X,	D3DTA_CURRENT);
		C.StageSET_TMC			(oT_Name,oT_xform,"$null",0);
		C.StageEnd			();
	}
	C.PassEnd			();
}

//
void CBlender_LaEmB::compile_3c	(CBlender_Compile& C)
{
	C.PassBegin		();
	{
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(false,true);
		
		// Stage1 - [=] Env-map [*] const
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_TMC			(oT2_Name,oT2_xform,oT2_const,0);
		C.StageEnd				();
		
		// Stage0 - [+] Lightmap
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_TMC			("$base1","$null","$null",1);
		C.StageEnd				();
		
		// Stage2 - [*] Base
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE2X,	D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE2X,	D3DTA_CURRENT);
		C.StageSET_TMC			(oT_Name,oT_xform,"$null",0);
		C.StageEnd				();
	}
	C.PassEnd			();
}

//
void CBlender_LaEmB::compile_L	(CBlender_Compile& C)
{
	// Pass1 - Lmap+Env
	C.PassBegin			();
	{
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(false,false);
		
		// Stage0 - Lightmap
		C.StageBegin			();
		C.StageEnd				();
		
		// Stage1 - Environment map
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_TMC			(oT2_Name, oT2_xform, "$null",0);
		C.StageEnd				();
	}
	C.PassEnd			();
}
//
void CBlender_LaEmB::compile_Lc	(CBlender_Compile& C)
{
	C.PassBegin		();
	{	
		C.PassSET_ZB			(true,true);
		C.PassSET_Blend_SET		();
		C.PassSET_LightFog		(false,false);
		
		// Stage0 - Environment map [*] const
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_TFACTOR);
		C.StageSET_TMC			(oT2_Name,oT2_xform,oT2_const,0);
		C.StageEnd				();
		
		// Stage1 - [+] Lightmap
		C.StageBegin			();
		C.StageSET_Color		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_Alpha		(D3DTA_TEXTURE,	  D3DTOP_ADD,			D3DTA_CURRENT);
		C.StageSET_TMC			("$base1","$null","$null",1);
		C.StageEnd				();
	}
	C.PassEnd			();
}
