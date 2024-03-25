// BlenderDefault.cpp: implementation of the CBlender_LmEbB class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop

#include "blender_Lm(EbB).h"
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
	oBlend.value		= FALSE;
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

#if RENDER==R_R1
//////////////////////////////////////////////////////////////////////////
// R1
//////////////////////////////////////////////////////////////////////////
void	CBlender_LmEbB::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	if (C.bEditor)	{
		C.PassBegin		();
		{
			C.PassSET_ZB		(TRUE,TRUE);
			C.PassSET_Blend_SET	();
			C.PassSET_LightFog	(TRUE,TRUE);
			
			// Stage1 - Env texture
			C.StageBegin		();
			C.StageSET_Address	(D3DTADDRESS_CLAMP);
			C.StageSET_Color	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,		D3DTA_DIFFUSE);
			C.StageSET_Alpha	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,		D3DTA_DIFFUSE);
			C.StageSET_TMC		(oT2_Name, oT2_xform, "$null", 0);
			C.StageEnd			();
			
			// Stage2 - Base texture
			C.StageBegin		();
			C.StageSET_Color	(D3DTA_TEXTURE,	  D3DTOP_BLENDTEXTUREALPHA,	D3DTA_CURRENT);
			C.StageSET_Alpha	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,		D3DTA_CURRENT);
			C.StageSET_TMC		(oT_Name, oT_xform, "$null", 0);
			C.StageEnd			();

			// Stage3 - Lighting - should work on all 2tex hardware
			C.StageBegin		();
			C.StageSET_Color	(D3DTA_DIFFUSE,	  D3DTOP_MODULATE,			D3DTA_CURRENT);
			C.StageSET_Alpha	(D3DTA_DIFFUSE,	  D3DTOP_SELECTARG2,		D3DTA_CURRENT);
			C.Stage_Texture		("$null"	);
			C.Stage_Matrix		("$null",	0);
			C.Stage_Constant	("$null"	);
			C.StageEnd			();
		}
		C.PassEnd			();
	} else {
		if (C.L_textures.size()<2)	Debug.fatal	(DEBUG_INFO,"Not enought textures for shader, base tex: %s",*C.L_textures[0]);
		switch (C.iElement)
		{
		case SE_R1_NORMAL_HQ:
		case SE_R1_NORMAL_LQ:
			// Level view
			/*
			if (C.bDetail_Diffuse)
			{
				if (oBlend.value)	C.r_Pass	("lmapE_dt","lmapE_dt",TRUE,TRUE,FALSE,TRUE,D3DBLEND_SRCALPHA,D3DBLEND_INVSRCALPHA,TRUE,0);
				else				C.r_Pass	("lmapE_dt","lmapE_dt",TRUE);
				C.r_Sampler	("s_base",	C.L_textures[0]);
				C.r_Sampler	("s_lmap",	C.L_textures[1]);
				C.r_Sampler	("s_env",	oT2_Name,false,D3DTADDRESS_CLAMP);
				C.r_Sampler	("s_detail",C.detail_texture);
				C.r_End		();
			} else
			{
			*/
			if (oBlend.value)	C.r_Pass	("lmapE","lmapE",TRUE,TRUE,FALSE,TRUE,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	TRUE,0);
			else				C.r_Pass	("lmapE","lmapE",TRUE);
			C.r_Sampler		("s_base",	C.L_textures[0]);
			C.r_Sampler		("s_lmap",	C.L_textures[1]);
			C.r_Sampler_clf	("s_hemi",	*C.L_textures[2]);
			C.r_Sampler		("s_env",	oT2_Name,false,D3DTADDRESS_CLAMP);
			C.r_End			();
			// }
			break;
		case SE_R1_LPOINT:
			C.r_Pass		("lmap_point","add_point",FALSE,TRUE,FALSE,TRUE,D3DBLEND_ONE,D3DBLEND_ONE,TRUE);
			C.r_Sampler		("s_base",	C.L_textures[0]		);
			C.r_Sampler_clf	("s_lmap",	TEX_POINT_ATT		);
			C.r_Sampler_clf	("s_att",	TEX_POINT_ATT		);
			C.r_End			();
			break;
		case SE_R1_LSPOT:
			C.r_Pass		("lmap_spot","add_spot",FALSE,TRUE,FALSE,TRUE,D3DBLEND_ONE,D3DBLEND_ONE,TRUE);
			C.r_Sampler		("s_base",	C.L_textures[0]);
			C.r_Sampler_clf	("s_lmap",	"internal\\internal_light_att",		true);
			C.r_Sampler_clf	("s_att",	TEX_SPOT_ATT		);
			C.r_End			();
			break;
		case SE_R1_LMODELS:
			// Lighting only, not use alpha-channel
			C.r_Pass		("lmap_l","lmap_l",FALSE);
			C.r_Sampler		("s_base",C.L_textures[0]);
			C.r_Sampler		("s_lmap",C.L_textures[1]);
			C.r_Sampler_clf	("s_hemi",*C.L_textures[2]);
			C.r_End			();
			break;
		}
	}
}
#elif RENDER==R_R2
//////////////////////////////////////////////////////////////////////////
// R2
//////////////////////////////////////////////////////////////////////////
void	CBlender_LmEbB::Compile(CBlender_Compile& C)
{
	if (oBlend.value)	C.r_Pass	("lmapE","lmapE",TRUE,TRUE,FALSE,TRUE,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	TRUE,0);
	else				C.r_Pass	("lmapE","lmapE",TRUE);
	C.r_Sampler			("s_base",	C.L_textures[0]	);
	C.r_Sampler			("s_lmap",	C.L_textures[1]	);
	C.r_Sampler_clf		("s_hemi",	*C.L_textures[2]);
	C.r_Sampler			("s_env",	oT2_Name,false,D3DTADDRESS_CLAMP);
	C.r_End				();
}
#else
//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
void	CBlender_LmEbB::Compile(CBlender_Compile& C)
{
#if 0
	if (oBlend.value)	C.r_Pass	("lmapE","lmapE",TRUE,TRUE,FALSE,TRUE,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	TRUE,0);
	else				C.r_Pass	("lmapE","lmapE",TRUE);
	//C.r_Sampler			("s_base",	C.L_textures[0]	);
	C.r_dx10Texture			("s_base",	C.L_textures[0]	);
	C.r_dx10Sampler			("smp_base");
	//C.r_Sampler			("s_lmap",	C.L_textures[1]	);
	C.r_dx10Texture			("s_lmap",	C.L_textures[1]	);
	C.r_dx10Sampler			("smp_linear");
	//C.r_Sampler_clf		("s_hemi",	*C.L_textures[2]);
	C.r_dx10Texture			("s_hemi",	*C.L_textures[2]);
	C.r_dx10Sampler			("smp_rtlinear");
	//C.r_Sampler			("s_env",	oT2_Name,false,D3DTADDRESS_CLAMP);
	C.r_dx10Texture			("s_env",	oT2_Name);
	//C.r_dx10Sampler			("smp_rtlinear");
	C.r_End				();
#else
#pragma todo("Hozar to ???: Cheak using this!")

	IBlender::Compile(C);

	if (oBlend.value)
	{
		switch (C.iElement)
		{
		case SE_R2_NORMAL_HQ:
		case SE_R2_NORMAL_LQ:
			uber_deffer(C, SE_R2_NORMAL_HQ == C.iElement, "deffer_base", "forward_base", false, 0, true);

			C.PassSET_ZB(TRUE, FALSE);
			C.PassSET_Blend(TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);

			C.r_dx10Texture("s_material", r2_material);
			C.r_dx10Texture("env_s0", r2_T_envs0);
			C.r_dx10Texture("env_s1", r2_T_envs1);
			C.r_dx10Texture("sky_s0", r2_T_sky0);
			C.r_dx10Texture("sky_s1", r2_T_sky1);

			C.r_dx10Sampler("smp_material");
			C.r_End();
			break;
		}
	}
	else {
		C.SetParams(1, false);

		switch (C.iElement)
		{
		case SE_R2_NORMAL_HQ:
			uber_deffer(C, true, "deffer_base", "deffer_base", true, 0, true);
			C.r_Stencil(TRUE, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case SE_R2_NORMAL_LQ:
			uber_deffer(C, false, "deffer_base", "deffer_base", true, 0, true);
			C.r_Stencil(TRUE, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		//case SE_R2_SHADOW:
		//	C.r_Pass("shadow_direct_base_aref", "shadow_direct_base_aref", FALSE, TRUE, TRUE, FALSE);
		//	//C.r_Sampler		("s_base",C.L_textures[0]);
		//	C.r_dx10Texture("s_base", C.L_textures[0]);
		//	C.r_dx10Sampler("smp_base");
		//	C.r_dx10Sampler("smp_linear");
		//	C.r_ColorWriteEnable(false, false, false, false);
		//	C.r_End();
		//	break;
		}
	}
#endif
}
#endif