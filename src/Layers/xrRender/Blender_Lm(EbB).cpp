// BlenderDefault.cpp: implementation of the CBlender_LmEbB class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


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

#if RENDER==R_R1
//////////////////////////////////////////////////////////////////////////
// R1
//////////////////////////////////////////////////////////////////////////
void	CBlender_LmEbB::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	if (C.bEditor)	{
		//C.PassBegin		();
		//{
		//	C.PassSET_ZB		(true,true);
		//	C.PassSET_Blend_SET	();
		//	C.PassSET_LightFog	(true,true);
		//	
		//	// Stage1 - Env texture
		//	C.StageBegin		();
		//	C.StageSET_Address	(D3DTADDRESS_CLAMP);
		//	C.StageSET_Color	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,		D3DTA_DIFFUSE);
		//	C.StageSET_Alpha	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,		D3DTA_DIFFUSE);
		//	C.StageSET_TMC		(oT2_Name, oT2_xform, "$null", 0);
		//	C.StageEnd			();
		//	
		//	// Stage2 - Base texture
		//	C.StageBegin		();
		//	C.StageSET_Color	(D3DTA_TEXTURE,	  D3DTOP_BLENDTEXTUREALPHA,	D3DTA_CURRENT);
		//	C.StageSET_Alpha	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,		D3DTA_CURRENT);
		//	C.StageSET_TMC		(oT_Name, oT_xform, "$null", 0);
		//	C.StageEnd			();

		//	// Stage3 - Lighting - should work on all 2tex hardware
		//	C.StageBegin		();
		//	C.StageSET_Color	(D3DTA_DIFFUSE,	  D3DTOP_MODULATE,			D3DTA_CURRENT);
		//	C.StageSET_Alpha	(D3DTA_DIFFUSE,	  D3DTOP_SELECTARG2,		D3DTA_CURRENT);
		//	C.Stage_Texture		("$null"	);
		//	C.Stage_Matrix		("$null",	0);
		//	C.Stage_Constant	("$null"	);
		//	C.StageEnd			();
		//}
		//C.PassEnd			();
		uber_deffer(C, true, "deffer_base", "deffer_base", false, nullptr, true);
		C.r_End();
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
				if (oBlend.value)	C.r_Pass	("lmapE_dt","lmapE_dt",true,true,false,true,D3DBLEND_SRCALPHA,D3DBLEND_INVSRCALPHA,true,0);
				else				C.r_Pass	("lmapE_dt","lmapE_dt",true);
				C.r_Sampler	("s_base",	C.L_textures[0]);
				C.r_Sampler	("s_lmap",	C.L_textures[1]);
				C.r_Sampler	("s_env",	oT2_Name,false,D3DTADDRESS_CLAMP);
				C.r_Sampler	("s_detail",C.detail_texture);
				C.r_End		();
			} else
			{
			*/
			if (oBlend.value)	C.r_Pass	("lmapE","lmapE",true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
			else				C.r_Pass	("lmapE","lmapE",true);
			C.r_Sampler		("s_base",	C.L_textures[0]);
			C.r_Sampler		("s_lmap",	C.L_textures[1]);
			C.r_Sampler_clf	("s_hemi",	*C.L_textures[2]);
			C.r_Sampler		("s_env",	oT2_Name,false,D3DTADDRESS_CLAMP);
			C.r_End			();
			// }
			break;
		case SE_R1_LPOINT:
			C.r_Pass		("lmap_point","add_point",false,true,false,true,D3DBLEND_ONE,D3DBLEND_ONE,true);
			C.r_Sampler		("s_base",	C.L_textures[0]		);
			C.r_Sampler_clf	("s_lmap",	TEX_POINT_ATT		);
			C.r_Sampler_clf	("s_att",	TEX_POINT_ATT		);
			C.r_End			();
			break;
		case SE_R1_LSPOT:
			C.r_Pass		("lmap_spot","add_spot",false,true,false,true,D3DBLEND_ONE,D3DBLEND_ONE,true);
			C.r_Sampler		("s_base",	C.L_textures[0]);
			C.r_Sampler_clf	("s_lmap",	"internal\\internal_light_att",		true);
			C.r_Sampler_clf	("s_att",	TEX_SPOT_ATT		);
			C.r_End			();
			break;
		case SE_R1_LMODELS:
			// Lighting only, not use alpha-channel
			C.r_Pass		("lmap_l","lmap_l",false);
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
	IBlender::Compile(C);

	if(oBlend.value) {
		switch(C.iElement) {
			case SE_R2_NORMAL_HQ:
			case SE_R2_NORMAL_LQ:
			{
				uber_forward(C, SE_R2_NORMAL_HQ == C.iElement, "deffer_base", "forward_base", false, true, 0);
				break;
			}
		}
	}
	else {
		C.SetParams(1, false);

		switch(C.iElement) {
			case SE_R2_NORMAL_HQ:
			uber_deffer(C, true, "deffer_base", "deffer_base", true, 0, true);
			C.r_End();
			break;
			case SE_R2_NORMAL_LQ:
			uber_deffer(C, false, "deffer_base", "deffer_base", true, 0, true);
			C.r_End();
			break;
			case SE_R2_SHADOW:
			C.r_Pass("shadow_base", "shadow_base", false);
			C.r_Sampler("s_base", C.L_textures[0]);
			C.r_End();
			break;
		}
	}
}
#else
//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
void	CBlender_LmEbB::Compile(CBlender_Compile& C)
{
#if 0
	if (oBlend.value)	C.r_Pass	("lmapE","lmapE",true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
	else				C.r_Pass	("lmapE","lmapE",true);
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

	IBlender::Compile(C);

	if (oBlend.value)
	{
		switch (C.iElement)
		{
			case SE_R2_NORMAL_HQ:
			case SE_R2_NORMAL_LQ:
			{
				uber_forward(C, SE_R2_NORMAL_HQ == C.iElement, "deffer_base", "forward_base", true, true, 0);
				break;
			}
		}
	}
	else {
		C.SetParams(1, false);

		switch (C.iElement)
		{
		case SE_R2_NORMAL_HQ:
			uber_deffer(C, true, "deffer_base", "deffer_base", true, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case SE_R2_NORMAL_LQ:
			uber_deffer(C, false, "deffer_base", "deffer_base", true, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case SE_R2_SHADOW:
			C.r_Pass("shadow_base", "shadow_base", false);
			C.r_dx10Texture("s_base", C.L_textures[0]);
			C.r_dx10Sampler("smp_base");
			C.r_dx10Sampler("smp_linear");
			C.r_ColorWriteEnable(false, false, false, false);
			C.r_End();
			break;
		case SE_R2_REFLECTIONS:
			RImplementation.addShaderOption("USE_LENGTH_BUFFER", "1");
			uber_forward(C, false, "deffer_base", "forward_base", true, false, 0);
			break;
		}
	}
#endif
}
#endif