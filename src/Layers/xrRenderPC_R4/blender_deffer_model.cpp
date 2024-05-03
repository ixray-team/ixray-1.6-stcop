#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/uber_deffer.h"
#include "Blender_deffer_model.h"

CBlender_deffer_model::CBlender_deffer_model	()	{	
	description.CLS		= B_MODEL;	
	description.version	= 2;
	oTessellation.Count         = 4;
	oTessellation.IDselected	= 0;
	oAREF.value			= 32;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oBlend.value		= FALSE;
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
		oBlend.value		= FALSE;
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

void	CBlender_deffer_model::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	if (oStrictSorting.value || (oBlend.value && oAREF.value < 16)) {
		switch (C.iElement)
		{
		case SE_R2_NORMAL_HQ:
		case SE_R2_NORMAL_LQ:
			uber_deffer(C, SE_R2_NORMAL_HQ == C.iElement, "deffer_model", "forward_base", false, 0, true);

			C.PassSET_ZB(TRUE, FALSE);
			C.PassSET_Blend(TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, oAREF.value);

			C.r_dx10Texture("s_material", r2_material);
			C.r_dx10Texture("env_s0", r2_T_envs0);
			C.r_dx10Texture("env_s1", r2_T_envs1);
			C.r_dx10Texture("sky_s0", r2_T_sky0);
			C.r_dx10Texture("sky_s1", r2_T_sky1);

			C.r_dx10Sampler("smp_material");
			C.r_End();
			break;
		}
	} else {
		BOOL	bAref = oBlend.value;

		C.TessMethod = oTessellation.IDselected;
		switch(C.iElement) 
		{
		case SE_R2_NORMAL_HQ: 			// deffer
			uber_deffer		(C,true,	"deffer_model",	"deffer_base",bAref,0,true);

			C.r_Stencil		( TRUE,D3DCMP_ALWAYS,0xff,0x7f,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);
			C.r_StencilRef	(0x01);
			C.r_End			();
			break;
		case SE_R2_NORMAL_LQ: 			// deffer
			uber_deffer		(C,false,	"deffer_model",	"deffer_base",bAref,0,true);
			C.r_Stencil		( TRUE,D3DCMP_ALWAYS,0xff,0x7f,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);
			C.r_StencilRef	(0x01);
			C.r_End			();
			break;
		case SE_R2_SHADOW:				// smap
			if (bAref)
			{
				//if (RImplementation.o.HW_smap)	C.r_Pass	("shadow_direct_model_aref","shadow_direct_base_aref",	FALSE,TRUE,TRUE,FALSE,D3DBLEND_ZERO,D3DBLEND_ONE,TRUE,220);
				//else							C.r_Pass	("shadow_direct_model_aref","shadow_direct_base_aref",	FALSE);
				//C.r_Sampler		("s_base",C.L_textures[0]);
				C.r_Pass	("shadow_direct_model_aref","shadow_direct_base_aref",	FALSE,TRUE,TRUE,FALSE,D3DBLEND_ZERO,D3DBLEND_ONE,TRUE,220);
				C.r_dx10Texture		("s_base",C.L_textures[0]);
				C.r_dx10Sampler		("smp_base");
				C.r_dx10Sampler		("smp_linear");
				C.r_ColorWriteEnable(false, false, false, false);
				C.r_End			();
				break;
			} 
			else 
			{
				//if (RImplementation.o.HW_smap)	C.r_Pass	("shadow_direct_model","dumb",	FALSE,TRUE,TRUE,FALSE);
				//else							C.r_Pass	("shadow_direct_model","shadow_direct_base",FALSE);
				C.r_Pass	("shadow_direct_model","dumb",	FALSE,TRUE,TRUE,FALSE);
				//C.r_Sampler		("s_base",C.L_textures[0]);
				C.r_dx10Texture		("s_base",C.L_textures[0]);
				C.r_dx10Sampler		("smp_base");
				C.r_dx10Sampler		("smp_linear");
				C.r_ColorWriteEnable(false, false, false, false);
				C.r_End			();
				break;
			}
		}
	}
}
