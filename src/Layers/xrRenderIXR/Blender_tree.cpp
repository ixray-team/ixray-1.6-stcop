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

	u32							tree_aref		= 200;
	if (oNotAnTree.value)		tree_aref		= 0;

	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
		if (oNotAnTree.value)	{
			
			const R1Bump Vertex(C, C.bDetail_Diffuse ? "tree_s_dt" : "tree_s");
			const R1Bump Pixel(C, C.bDetail_Diffuse ? "vert_dt" : "vert");
			if (oBlend.value)	C.r_Pass	(Vertex.Name,	Pixel.Name,	true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
			else				C.r_Pass	(Vertex.Name,	Pixel.Name,	true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_detail", C.detail_texture);
			Pixel.Bind(C);
			C.r_End				();
		} else {
			
			const R1Bump Vertex(C, C.bDetail_Diffuse ? "tree_w_dt" : "tree_w");
			const R1Bump Pixel(C, C.bDetail_Diffuse ? "vert_dt" : "vert");
			if (oBlend.value)	C.r_Pass	(Vertex.Name,	Pixel.Name,	true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
			else				C.r_Pass	(Vertex.Name,	Pixel.Name,	true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_detail", C.detail_texture);
			Pixel.Bind(C);
			C.r_End				();
		}
		break;
	case ShaderVariant::NormalLQ:
		
		if (oBlend.value)	C.r_Pass	("tree_s",	"vert",		true,true,true,true,D3DBLEND_SRCALPHA,	D3DBLEND_INVSRCALPHA,	true,tree_aref);
		else				C.r_Pass	("tree_s",	"vert",		true,true,true,true,D3DBLEND_ONE,		D3DBLEND_ZERO,			true,tree_aref);
		r1_tex(C, "s_base", C.L_textures[0]);
		C.r_End				();
		break;
	case ShaderVariant::Shadow:
		if (oBlend.value) C.AddDefine("USE_AREF", "1");
		C.r_Pass("shadow_lod", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	}
}
