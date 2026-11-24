// BlenderDefault.cpp: implementation of the CBlender_Model_EbB class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r1_blender_tex.h"
#include "../../xrEngine/EngineAPI.h"


#include "Blender_Model_EbB.h"
#include "uber_deffer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Model_EbB::CBlender_Model_EbB	()
{
	description.CLS		= B_MODEL_EbB;
	description.version	= 0x1;
	xr_strcpy				(oT2_Name,	"$null");
	xr_strcpy				(oT2_xform,	"$null");
	oBlend.value		= false;
}

CBlender_Model_EbB::~CBlender_Model_EbB	()
{
	
}

void	CBlender_Model_EbB::Save(	IWriter& fs )
{
	description.version	= 0x1;
	IBlender::Save	(fs);
	xrPWRITE_MARKER	(fs,"Environment map");
	xrPWRITE_PROP	(fs,"Name",				xrPID_TEXTURE,	oT2_Name);
	xrPWRITE_PROP	(fs,"Transform",		xrPID_MATRIX,	oT2_xform);
	xrPWRITE_PROP	(fs,"Alpha-Blend",		xrPID_BOOL,		oBlend);
}

void	CBlender_Model_EbB::Load(	IReader& fs, u16 version )
{
	IBlender::Load	(fs,version);
	xrPREAD_MARKER	(fs);
	xrPREAD_PROP	(fs,xrPID_TEXTURE,	oT2_Name);
	xrPREAD_PROP	(fs,xrPID_MATRIX,	oT2_xform);
	if (version>=0x1)	{
		xrPREAD_PROP	(fs,xrPID_BOOL,	oBlend);
	}
}

#include "uber_deffer.h"
void CBlender_Model_EbB::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	// The interface's icon is lit by its own light; low shading quality keeps the classic look
	if (C.iElement == ShaderVariant::UI && ps_r_shading_quality >= 1)
	{
		uber_ui(C, false, false, !!oBlend.value, "shaders\\studio_light#small", "shaders\\studio_light");
		return;
	}

	const char*	vsname			= nullptr;
	const char*	psname			= nullptr;
	switch (C.iElement)
	{
	case ShaderVariant::UI:
		vsname = psname =	"model_env_hq"; 
		if (oBlend.value)	C.r_Pass	(vsname,"model_env_sl",true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
		else				C.r_Pass	(vsname,"model_env_sl",true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_env", oT2_Name, true);
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
		const R1Bump Bump(C, "model_env_hq");
		if (oBlend.value)	C.r_Pass	(Bump.Name,Bump.Name,true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
		else				C.r_Pass	(Bump.Name,Bump.Name,true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_env", oT2_Name, true);
		Bump.Bind(C);
		C.r_End				();
		break;
	}
	case ShaderVariant::NormalLQ:
		vsname = psname =	"model_env_lq"; 
		if (oBlend.value)	C.r_Pass	(vsname,psname,true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
		else				C.r_Pass	(vsname,psname,true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_env", oT2_Name, true);
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
