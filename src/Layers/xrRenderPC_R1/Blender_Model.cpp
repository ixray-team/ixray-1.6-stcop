#include "stdafx.h"


#include "Blender_Model.h"
#include "../xrRender/uber_deffer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Model::CBlender_Model()
{
	description.CLS		= B_MODEL;
	description.version	= 2;
	oTessellation.Count         = 4;
	oTessellation.IDselected	= 0;
	oAREF.value			= 32;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oBlend.value		= false;
}

CBlender_Model::~CBlender_Model()
{
	
}

void	CBlender_Model::Save	( IWriter& fs	)
{
	IBlender::Save		(fs);
	xrPWRITE_PROP		(fs,"Use alpha-channel",	xrPID_BOOL,		oBlend);
	xrPWRITE_PROP		(fs,"Alpha ref",			xrPID_INTEGER,	oAREF);
	xrP_TOKEN::Item	I;
	xrPWRITE_PROP	(fs,"Tessellation",	xrPID_TOKEN, oTessellation);
	I.ID = 0; xr_strcpy(I.str,"NO_TESS");	fs.w		(&I,sizeof(I));
	I.ID = 1; xr_strcpy(I.str,"TESS_PN");	fs.w		(&I,sizeof(I));
	I.ID = 2; xr_strcpy(I.str,"TESS_HM");	fs.w		(&I,sizeof(I));
	I.ID = 3; xr_strcpy(I.str,"TESS_PN+HM");	fs.w		(&I,sizeof(I));
}

void	CBlender_Model::Load	( IReader& fs, u16 version)
{
	IBlender::Load		(fs,version);

	switch (version)	
	{
	case 0: 
		oAREF.value			= 32;
		oAREF.min			= 0;
		oAREF.max			= 255;
		oBlend.value		= false;
		break;
	case 1:
	default:
		xrPREAD_PROP	(fs,xrPID_BOOL,		oBlend);
		xrPREAD_PROP	(fs,xrPID_INTEGER,	oAREF);
		break;
	}
	if (version>1)
	{
		xrPREAD_PROP(fs,xrPID_TOKEN,oTessellation);
	}
}

void	CBlender_Model::Compile	(CBlender_Compile& C)
{
	IBlender::Compile		(C);
	if (C.bEditor)
	{
		//C.PassBegin		();
		//{
		//	C.PassSET_ZB		(true,oBlend.value&&(oAREF.value<200)?false:true);
		//	if (oBlend.value)	C.PassSET_Blend_BLEND	(true,oAREF.value);
		//	else				C.PassSET_Blend_SET		();
		//	C.PassSET_LightFog	(true,true);
		//	C.StageBegin		();
		//	C.StageSET_Color	(D3DTA_TEXTURE,	  D3DTOP_MODULATE,		D3DTA_DIFFUSE);
		//	C.StageSET_Alpha	(D3DTA_TEXTURE,	  D3DTOP_SELECTARG1,	D3DTA_DIFFUSE);
		//	C.StageSET_TMC		(oT_Name,	"$null",	"$null",	0		);
		//	C.StageEnd			();
		//}
		//C.PassEnd			();

		bool is_blend = oBlend.value && oAREF.value < 16;

		if(is_blend) {
			RImplementation.addShaderOption("FORWARD_ONLY", "1");
		}

		uber_deffer(C, true, "deffer_model", "deffer_base", !is_blend && !!oBlend.value, nullptr, true);

		if(is_blend) {
			C.PassSET_ZB(true, false);
			C.PassSET_Blend(true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);
		}

		C.r_End();
	} else {
		const char*	vsname		= nullptr;
		const char*	psname		= nullptr;
		switch (C.iElement)
		{
		case SE_R1_NORMAL_HQ:	
			vsname = psname =	"model_def_hq";
			if (oBlend.value)	C.r_Pass	(vsname,psname,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
			else				C.r_Pass	(vsname,psname,true);
			C.r_Sampler			("s_base",	C.L_textures[0]);
			C.r_Sampler_clf		("s_lmap",	"$user$projector",true);
			C.r_End				();
			break;
		case SE_R1_NORMAL_LQ:
			vsname = psname =	"model_def_lq"; 
			if (oBlend.value)	C.r_Pass	(vsname,psname,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
			else				C.r_Pass	(vsname,psname,true);
			C.r_Sampler			("s_base",	C.L_textures[0]);
			C.r_End				();
			break;
		case SE_R1_LPOINT:
			vsname				= "model_def_point";
			psname				= "add_point";
			if (oBlend.value)	C.r_Pass	(vsname,psname,false,true,false,true,D3DBLEND_ONE,	D3DBLEND_ONE,true,oAREF.value);
			else				C.r_Pass	(vsname,psname,false,true,false,true,D3DBLEND_ONE,	D3DBLEND_ONE,true);
			C.r_Sampler			("s_base",	C.L_textures[0]);
			C.r_Sampler_clf		("s_lmap",	TEX_POINT_ATT);
			C.r_Sampler_clf		("s_att",	TEX_POINT_ATT);
			C.r_End				();
			break;
		case SE_R1_LSPOT:
			vsname				= "model_def_spot";
			psname				= "add_spot";
			if (oBlend.value)	C.r_Pass	(vsname,psname,false,true,false,true,D3DBLEND_ONE,	D3DBLEND_ONE,true,oAREF.value);
			else				C.r_Pass	(vsname,psname,false,true,false,true,D3DBLEND_ONE,	D3DBLEND_ONE,true);
			C.r_Sampler			("s_base",	C.L_textures[0]);
			C.r_Sampler_clf		("s_lmap",	"internal\\internal_light_att",		true);
			C.r_Sampler_clf		("s_att",	TEX_SPOT_ATT);
			C.r_End				();
			break;
		case SE_R1_LMODELS:
			vsname				= "model_def_shadow";
			psname				= "model_shadow";
			C.r_Pass			(vsname,psname,false,false,false,true,D3DBLEND_ZERO,D3DBLEND_SRCCOLOR,false,0);
			C.r_End				();
			break;
		}
	}
}
