// Blender_Vertex_aref.cpp: implementation of the CBlender_Detail_Still class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r1_blender_tex.h"
#include "../../xrEngine/EngineAPI.h"


#include "Blender_detail_still.h"
#include "uber_deffer.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Detail_Still::CBlender_Detail_Still()
{
	description.CLS		= B_DETAIL;
	description.version	= 0;
}

CBlender_Detail_Still::~CBlender_Detail_Still()
{

}

void	CBlender_Detail_Still::Save		(IWriter& fs )
{
	IBlender::Save		(fs);
	xrPWRITE_PROP		(fs,"Alpha-blend",	xrPID_BOOL,		oBlend);
}

void	CBlender_Detail_Still::Load		(IReader& fs, u16 version )
{
	IBlender::Load		(fs,version);
	xrPREAD_PROP		(fs,xrPID_BOOL,		oBlend);
}

//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
#include "uber_deffer.h"
void CBlender_Detail_Still::Compile	(CBlender_Compile& C)
{
	IBlender::Compile(C);

	if (LightingModeIsStaticRender())
	{

		switch (C.iElement)
		{
		case ShaderVariant::NormalHQ:
			C.r_Pass("detail_wave", "detail", true, true, true, false, D3DBLEND_ONE, D3DBLEND_ZERO, oBlend.value ? true : false, oBlend.value ? 200 : 0);
			r1_tex(C, "s_base", C.L_textures[0]);
			C.r_End		();
			break;
		case ShaderVariant::NormalLQ:
			C.r_Pass("detail_still", "detail", true, true, true, false, D3DBLEND_ONE, D3DBLEND_ZERO, oBlend.value ? true : false, oBlend.value ? 200 : 0);
			r1_tex(C, "s_base", C.L_textures[0]);
			C.r_End		();
			break;
		case ShaderVariant::PointLight:
			break;
		case ShaderVariant::SpotLight:
			break;
		case ShaderVariant::ModelLighting:
			break;
		}
	
		return;
	}

	if (C.bEditor)
	{

		uber_deffer(C, false, "deffer_detail", "deffer_base", true, 0, true);
		C.r_CullMode(D3DCULL_NONE);
		uber_deffer_editor_end(C);
		return;
	}

	if(C.iElement == SE_R2_DETAIL_SHADOW_HQ || C.iElement == SE_R2_DETAIL_SHADOW_LQ)
	{
		C.AddDefine("DISABLE_MOTION_VECTORS", "1");
		C.AddDefine("DETAIL_SHADOW_PASS", "1");
		C.AddDefine("USE_AREF", "1");
	}
	else
	{
		C.AddDefine("FIX_CULL_NORMAL", "1");
	}

	if (C.iElement == ShaderVariant::NormalHQ || C.iElement == SE_R2_DETAIL_SHADOW_HQ)
	{
		C.AddDefine("USE_TREEWAVE", "1");
	}

	switch(C.iElement)
	{
	case ShaderVariant::NormalHQ:
		uber_deffer(C, false, "deffer_detail", "deffer_base", true, 0, true);
		C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		C.r_StencilRef(0x01);
		C.r_CullMode(D3DCULL_NONE);
		C.r_End();
		break;
	case ShaderVariant::NormalLQ:
		uber_deffer(C, false, "deffer_detail", "deffer_base", true, 0, true);
		C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		C.r_StencilRef(0x01);
		C.r_CullMode(D3DCULL_NONE);
		C.r_End();
		break;
	case SE_R2_DETAIL_SHADOW_HQ:
	case SE_R2_DETAIL_SHADOW_LQ:
		C.r_Pass("deffer_detail", "shadow_base", false);

		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_CullMode(D3DCULL_NONE);

		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
	}

	C.Defines.clear();
}
