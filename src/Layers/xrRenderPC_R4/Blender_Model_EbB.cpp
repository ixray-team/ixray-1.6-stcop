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

	if (LightingModeIsStaticRender())
	{
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
		case ShaderVariant::NormalHQ:	
			vsname = psname =	"model_env_hq"; 
			if (oBlend.value)	C.r_Pass	(vsname,psname,true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
			else				C.r_Pass	(vsname,psname,true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_env", oT2_Name, true);
			r1_tex(C, "s_lmap", "$user$projector", true, true);
			C.r_End				();
			break;
		case ShaderVariant::NormalLQ:
			vsname = psname =	"model_env_lq"; 
			if (oBlend.value)	C.r_Pass	(vsname,psname,true,true,false,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,0);
			else				C.r_Pass	(vsname,psname,true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_env", oT2_Name, true);
			C.r_End				();
			break;
		case ShaderVariant::PointLight:
			vsname				= "model_def_point";
			psname				= "add_point";
			C.r_Pass			(vsname,psname,false,true,false,true,D3DBLEND_ONE, D3DBLEND_ONE,true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", TEX_POINT_ATT, true);
			r1_tex(C, "s_att", TEX_POINT_ATT, true);
			C.r_End				();
			break;
		case ShaderVariant::SpotLight:
			vsname				= "model_def_spot";
			psname				= "add_spot";
			C.r_Pass			(vsname,psname,false,true,false,true,D3DBLEND_ONE, D3DBLEND_ONE,true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", "internal\\internal_light_att", true);
			r1_tex(C, "s_att", TEX_SPOT_ATT, true);
			C.r_End				();
			break;
		case ShaderVariant::ModelLighting:
			vsname				= "model_def_shadow";
			psname				= "model_shadow";
			C.r_Pass			(vsname,psname,false,false,false,true,D3DBLEND_ZERO,D3DBLEND_SRCCOLOR,false,0);
			C.r_End				();
			break;
		}
	
		return;
	}

	if (C.bEditor)
	{


		if (oBlend.value)
		{
			C.AddDefine("FORWARD_ONLY", "1");
		}

		uber_deffer(C, true, "deffer_model", "deffer_base", false, 0, true);

		if (oBlend.value) {
			C.PassSET_ZB(TRUE, FALSE);
			C.PassSET_Blend(TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);
			C.r_End();
		}
		else
		{
			uber_deffer_editor_end(C);
		}
		return;
	}

	if (C.iElement == ShaderVariant::UI)
	{
		C.AddDefine("DISABLE_MOTION_VECTORS", "1");
		if (oBlend.value)
		{
			C.AddDefine("UI_ALPHA_BLEND", "1");
		}
		uber_deffer(C, false, "deffer_model", "ui_base", false, 0, true);
		
		if (oBlend.value)
		{
			C.PassSET_ZB(true, false);
			C.PassSET_Blend(true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);
		}

		C.r_dx10Texture("env_s0", "shaders\\studio_light#small");
		C.r_dx10Texture("sky_s0", "shaders\\studio_light");

		C.r_dx10Texture("s_material", r2_material);
		C.r_End();

		return;
	}
	else if (C.iElement == ShaderVariant::Reflection)
	{
		C.AddDefine("USE_LENGTH_BUFFER", "1");
		C.AddDefine("DISABLE_MOTION_VECTORS", "1");
		uber_forward(C, false, "deffer_model", "forward_base", true, false, 0);

		return;
	}

	if (oBlend.value)	
	{
		switch(C.iElement) 
		{
		case ShaderVariant::HUD:
		case ShaderVariant::NormalHQ:
		case ShaderVariant::NormalLQ:
		{
			uber_forward(C, ShaderVariant::NormalLQ != C.iElement, "deffer_model", "forward_base", true, true);
			break;
		}
		case ShaderVariant::Shadow:
		{
			C.AddDefine("DISABLE_VELOCITY", "1");
			C.AddDefine("USE_TRANSPARENT", "1");
			C.r_Pass("shadow_model", "shadow_base", false, true, false, true, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO);

			C.r_dx10Texture("s_base", C.L_textures[0]);

			C.r_End();
			break;
		}
		}
	} 
	else 
	{
		// deferred
		switch(C.iElement) 
		{
		case ShaderVariant::HUD:
		case ShaderVariant::NormalHQ: 	// deffer
			uber_deffer(C, true, "deffer_model", "deffer_base", false, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case ShaderVariant::NormalLQ: 	// deffer
			uber_deffer(C, false, "deffer_model", "deffer_base", false, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case ShaderVariant::Shadow:		// smap
			C.AddDefine("DISABLE_VELOCITY", "1");
			C.r_Pass("shadow_model", "shadow_base", false);
			C.r_dx10Texture("s_base", C.L_textures[0]);
			C.r_ColorWriteEnable(false, false, false, false);
			C.r_End();
			break;
		}
	}
}
