#include "stdafx.h"


#include "uber_deffer.h"
#include "blender_deffer_model.h"

CBlender_deffer_model::CBlender_deffer_model	()	{	
	description.CLS		= B_MODEL;	
	description.version	= 2;
	oTessellation.Count         = 4;
	oTessellation.IDselected	= 0;
	oAREF.value			= 32;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oBlend.value		= false;
}
CBlender_deffer_model::~CBlender_deffer_model	()	{	}

void	CBlender_deffer_model::Save	(	IWriter& fs )
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
void	CBlender_deffer_model::Load	(	IReader& fs, u16 version )
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

void CBlender_deffer_model::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if (C.iElement == ShaderVariant::UI)
	{
		C.AddDefine("DISABLE_MOTION_VECTORS", "1");
		if (oStrictSorting.value || (oBlend.value && oAREF.value < 16))
		{
			C.AddDefine("UI_ALPHA_BLEND", "1");
		}
		uber_deffer(C, true, "deffer_model", "ui_base", !!oBlend.value, 0, true);

		if (oStrictSorting.value || (oBlend.value && oAREF.value < 16)) 
		{
			C.PassSET_ZB(true, false);
			C.PassSET_Blend(true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);
		}

		C.r_dx10Texture("env_s0", "shaders\\newsky_viewport#small");
		C.r_dx10Texture("sky_s0", "shaders\\newsky_viewport");

		C.r_dx10Texture("s_material", r2_material);

		C.r_End();

		return;
	}
	else if (C.iElement == ShaderVariant::Reflection)
	{
		C.AddDefine("USE_LENGTH_BUFFER", "1");
		C.AddDefine("DISABLE_MOTION_VECTORS", "1");

		uber_forward(C, false, "deffer_model", "forward_base", oBlend.value, false, 0);

		return;
	}

	if (oStrictSorting.value || (oBlend.value && oAREF.value < 16)) 
	{
		switch (C.iElement)
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
		bool bAref = oBlend.value;

		C.TessMethod = oTessellation.IDselected;
		switch (C.iElement)
		{
		case ShaderVariant::HUD:
		case ShaderVariant::NormalHQ: // deffer
			uber_deffer(C, true, "deffer_model", "deffer_base", bAref, 0, true);

			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case ShaderVariant::NormalLQ: // deffer
			uber_deffer(C, false, "deffer_model", "deffer_base", bAref, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
			break;
		case ShaderVariant::Shadow: // smap
			if (bAref) {
				C.AddDefine("USE_AREF", "1");
			}
			
			C.AddDefine("DISABLE_VELOCITY", "1");
			C.r_Pass("shadow_model", "shadow_base", false);

			C.r_dx10Texture("s_base", C.L_textures[0]);
			C.r_ColorWriteEnable(false, false, false, false);
			C.r_End();
			break;
		}
	}
}
