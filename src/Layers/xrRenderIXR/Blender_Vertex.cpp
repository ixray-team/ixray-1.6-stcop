// Blender_Vertex.cpp: implementation of the CBlender_Vertex class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "Blender_Vertex.h"
#include "r1_blender_tex.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_Vertex::CBlender_Vertex()
{
	description.CLS		        = B_VERT;
	description.version         = 1;
	oTessellation.Count         = 4;
	oTessellation.IDselected	= 0;
}

CBlender_Vertex::~CBlender_Vertex()
{
	
}

void	CBlender_Vertex::Save	( IWriter& fs	)
{
	IBlender::Save	(fs);
	xrP_TOKEN::Item	I;
	xrPWRITE_PROP	(fs,"Tessellation",	xrPID_TOKEN, oTessellation);
	I.ID = 0; xr_strcpy(I.str,"NO_TESS");	fs.w		(&I,sizeof(I));
	I.ID = 1; xr_strcpy(I.str,"TESS_PN");	fs.w		(&I,sizeof(I));
	I.ID = 2; xr_strcpy(I.str,"TESS_HM");	fs.w		(&I,sizeof(I));
	I.ID = 3; xr_strcpy(I.str,"TESS_PN+HM");	fs.w		(&I,sizeof(I));
}

void	CBlender_Vertex::Load	( IReader& fs, u16 version	)
{
	IBlender::Load	(fs,version);
	if (version>0)
	{
		xrPREAD_PROP(fs,xrPID_TOKEN,oTessellation);
	}
}

void CBlender_Vertex::Compile	(CBlender_Compile& C)
{
	IBlender::Compile		(C);

	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
		// Level view. A forward pixel shader is not told what kind of vertex it shades: this says it is the level
		C.AddDefine("SURFACE_LEVEL", "1");

		if (C.bDetail_Diffuse)
		{
			const R1Bump Bump(C, "vert_dt");
			C.r_Pass	(Bump.Name, Bump.Name, true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_detail", C.detail_texture);
			Bump.Bind(C);
			C.r_End		();
		} else	{
			const R1Bump Bump(C, "vert");
			C.r_Pass	(Bump.Name, Bump.Name, true);
			r1_tex(C, "s_base", C.L_textures[0]);
			Bump.Bind(C);
			C.r_End		();
		}
		break;
	case ShaderVariant::NormalLQ:
		// Level view
		C.AddDefine("SURFACE_LEVEL", "1");
		C.r_Pass	("vert","vert",true);
		r1_tex(C, "s_base", C.L_textures[0]);
		C.r_End		();
		break;
	case ShaderVariant::Shadow:
		C.r_Pass("shadow_base", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	}
}
