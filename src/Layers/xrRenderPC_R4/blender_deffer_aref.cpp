#include "stdafx.h"


#include "../xrRender/uber_deffer.h"
#include "blender_deffer_aref.h"

CBlender_deffer_aref::CBlender_deffer_aref	(bool _lmapped) : lmapped(_lmapped)	{	
	description.CLS		= B_DEFAULT_AREF;
	oAREF.value			= 200;
	oAREF.min			= 0;
	oAREF.max			= 255;
	oBlend.value		= false;
	description.version	= 1;
}
CBlender_deffer_aref::~CBlender_deffer_aref	()	{	}

void	CBlender_deffer_aref::Save	(	IWriter& fs )
{
	IBlender::Save	(fs);
	xrPWRITE_PROP	(fs,"Alpha ref",	xrPID_INTEGER,	oAREF);
	xrPWRITE_PROP	(fs,"Alpha-blend",	xrPID_BOOL,		oBlend);
}
void	CBlender_deffer_aref::Load	(	IReader& fs, u16 version )
{
	IBlender::Load	(fs,version);
	if (1==version)	{
		xrPREAD_PROP	(fs,xrPID_INTEGER,	oAREF);
		xrPREAD_PROP	(fs,xrPID_BOOL,		oBlend);
	}
}

void	CBlender_deffer_aref::Compile(CBlender_Compile& C)
{
	IBlender::Compile		(C);

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
	else 
	{
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
			RImplementation.addShaderOption("USE_AREF", "1");
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
}
