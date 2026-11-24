#include "stdafx.h"


#include "Blender_Model.h"
#include "uber_deffer.h"
#include "r1_blender_tex.h"

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

void CBlender_Model::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);
	const char*	vsname		= nullptr;
	const char*	psname		= nullptr;
	switch (C.iElement)
	{
	case ShaderVariant::UI:
		// The interface's icon, lit by its own light; low shading quality keeps the classic look
		if (ps_r_shading_quality >= 1)
		{
			uber_ui(C, true, !!oBlend.value, oStrictSorting.value || (oBlend.value && oAREF.value < 16), "shaders\\newsky_viewport#small", "shaders\\newsky_viewport");
			break;
		}

		vsname = psname =	"model_def_hq";
		if (oBlend.value)	C.r_Pass	(vsname,"r1_simple",true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
		else				C.r_Pass	(vsname,"r1_simple",true);
		r1_tex(C, "s_base", C.L_textures[0]);
		C.r_End				();
		break;
	case ShaderVariant::HUD:
		// The HUD keeps a blended surface sorted where the world weighs it order-independently
		if (!oBlend.value || !ForwardWboit())
		{
			break;
		}

		[[fallthrough]];
	case ShaderVariant::NormalHQ:	
	{
		// A model whose texture has a detail texture takes it, as the level's surfaces do: a door or a crate beside a
		// wall was smooth where the wall had its grain
		const bool IsDetailed = C.bDetail_Diffuse && C.detail_texture;
		const R1Bump Bump(C, IsDetailed ? "model_def_hq_dt" : "model_def_hq");
		// Glass: blended with no cut, sorted and out of the depth, as the frame that shades a pixel once drew it. Writing
		// depth, the lens of a sight hid the mark a hair behind it
		const bool IsGlass = oStrictSorting.value || (oBlend.value && oAREF.value < 16);

		if (IsGlass)
		{
			C.r_Pass(Bump.Name, Bump.Name, true, true, false, true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, false, 0);
			C.SH->flags.bStrictB2F = true;
		}
		else if (oBlend.value)	C.r_Pass	(Bump.Name,Bump.Name,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
		else				C.r_Pass	(Bump.Name,Bump.Name,true);
		r1_tex(C, "s_base", C.L_textures[0]);

		if (IsDetailed)
		{
			r1_tex(C, "s_detail", C.detail_texture);
		}

		Bump.Bind(C);
		C.r_End				();
		break;
	}
	case ShaderVariant::NormalLQ:
		vsname = psname =	"model_def_lq"; 
		if (oBlend.value)	C.r_Pass	(vsname,psname,true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,oAREF.value);
		else				C.r_Pass	(vsname,psname,true);
		r1_tex(C, "s_base", C.L_textures[0]);
		C.r_End				();
		break;
	case ShaderVariant::Shadow:
		C.r_Pass			("shadow_model", "shadow_base", false);
		C.r_dx10Texture		("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End				();
		break;
	}
}