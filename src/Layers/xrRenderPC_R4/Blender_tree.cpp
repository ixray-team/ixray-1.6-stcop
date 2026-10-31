// Blender_Vertex_aref.cpp: implementation of the CBlender_Tree class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r1_blender_tex.h"
#include "../../xrEngine/EngineAPI.h"


#include "Blender_tree.h"
#include "uber_deffer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Tree::CBlender_Tree()
{
	description.CLS		= B_TREE;
	description.version	= 1;
	oBlend.value		= false;
	oNotAnTree.value	= false;
}

CBlender_Tree::~CBlender_Tree()
{

}

void	CBlender_Tree::Save		(IWriter& fs )
{
	IBlender::Save		(fs);
	xrPWRITE_PROP		(fs,"Alpha-blend",	xrPID_BOOL,		oBlend);
	xrPWRITE_PROP		(fs,"Object LOD",	xrPID_BOOL,		oNotAnTree);
}

void	CBlender_Tree::Load		(IReader& fs, u16 version )
{
	IBlender::Load		(fs,version);
	xrPREAD_PROP		(fs,xrPID_BOOL,		oBlend);
	if (version>=1)		{
		xrPREAD_PROP		(fs,xrPID_BOOL,		oNotAnTree);
	}
}

//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
void	CBlender_Tree::Compile	(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	if (LightingModeIsStaticRender())
	{

		u32							tree_aref		= 200;
		if (oNotAnTree.value)		tree_aref		= 0;

		switch (C.iElement)
		{
		case ShaderVariant::NormalHQ:
			if (oNotAnTree.value)	{
				
				const char* tsv	= "tree_s", *tsp="vert";
				if (C.bDetail_Diffuse)	{ tsv="tree_s_dt"; tsp="vert_dt";}
				if (oBlend.value)	C.r_Pass	(tsv,	tsp,	true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
				else				C.r_Pass	(tsv,	tsp,	true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
				r1_tex(C, "s_base", C.L_textures[0]);
				r1_tex(C, "s_detail", C.detail_texture);
				C.r_End				();
			} else {
				
				if (C.bDetail_Diffuse)
				{
					if (oBlend.value)	C.r_Pass	("tree_w_dt","vert_dt",	true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
					else				C.r_Pass	("tree_w_dt","vert_dt",	true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
					r1_tex(C, "s_base", C.L_textures[0]);
					r1_tex(C, "s_detail", C.detail_texture);
					C.r_End				();
				} else {
					if (oBlend.value)	C.r_Pass	("tree_w",	"vert",		true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
					else				C.r_Pass	("tree_w",	"vert",		true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
					r1_tex(C, "s_base", C.L_textures[0]);
					r1_tex(C, "s_detail", C.detail_texture);
					C.r_End				();
				}
			}
			break;
		case ShaderVariant::NormalLQ:
			
			if (oBlend.value)	C.r_Pass	("tree_s",	"vert",		true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
			else				C.r_Pass	("tree_s",	"vert",		true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
			r1_tex(C, "s_base", C.L_textures[0]);
			C.r_End				();
			break;
		case ShaderVariant::PointLight:
			C.r_Pass		((oNotAnTree.value)?"tree_s_point":"tree_w_point",	"add_point",false,true,false,true,D3DBLEND_ONE,D3DBLEND_ONE,true,0);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", TEX_POINT_ATT, true);
			r1_tex(C, "s_att", TEX_POINT_ATT, true);
			C.r_End			();
			break;
		case ShaderVariant::SpotLight:
			C.r_Pass		((oNotAnTree.value)?"tree_s_spot":"tree_w_spot",	"add_spot",	false,true,false,true,D3DBLEND_ONE,D3DBLEND_ONE,true,0);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", "internal\\internal_light_att", true);
			r1_tex(C, "s_att", TEX_SPOT_ATT, true);
			C.r_End			();
			break;
		case ShaderVariant::ModelLighting:
			
			break;
		}
	
		return;
	}

	if (C.bEditor)
	{

		uber_deffer(C, true, "deffer_base", "deffer_base", oBlend.value, 0, true);
		uber_deffer_editor_end(C);
		return;
	}

	if (!oNotAnTree.value) {
		C.AddDefine("USE_TREEWAVE", "1");
	}

	switch (C.iElement)	{
		case ShaderVariant::NormalHQ:
			uber_deffer(C, true, "deffer_lod", "deffer_base", oBlend.value, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();
		
		break;
		case ShaderVariant::NormalLQ:
			uber_deffer(C, false, "deffer_lod", "deffer_base", oBlend.value, 0, true);
			C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
			C.r_StencilRef(0x01);
			C.r_End();

		break;
		case ShaderVariant::Shadow:
		{
			if (oBlend.value)
			{
				C.AddDefine("USE_AREF", "1");
			}

			C.r_Pass("shadow_lod", "shadow_base", false);

			C.r_dx10Texture("s_base", C.L_textures[0]);

			C.r_ColorWriteEnable(false, false, false, false);
			C.r_End();
		}
		break;
		case ShaderVariant::Reflection:
		{
			C.AddDefine("USE_LENGTH_BUFFER", "1");
			C.AddDefine("DISABLE_MOTION_VECTORS", "1");
			uber_forward(C, false, "deffer_lod", "forward_base", oBlend.value, false, 0);
		}
		break;
	}
	C.Defines.clear();
}
