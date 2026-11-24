// BlenderDefault.cpp: implementation of the CBlender_BmmD class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r1_blender_tex.h"
#include "../../xrEngine/EngineAPI.h"


#include "Blender_BmmD.h"
#include "uber_deffer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBlender_BmmD::CBlender_BmmD	()
{
	description.CLS		= B_BmmD;
	xr_strcpy				(oT2_Name,	"$null");
	xr_strcpy				(oT2_xform,	"$null");
	description.version	= 3;
	xr_strcpy				(oR_Name,	"detail\\detail_grnd_grass");	//"$null");
	xr_strcpy				(oG_Name,	"detail\\detail_grnd_asphalt");	//"$null");
	xr_strcpy				(oB_Name,	"detail\\detail_grnd_earth");	//"$null");
	xr_strcpy				(oA_Name,	"detail\\detail_grnd_yantar");	//"$null");
}

CBlender_BmmD::~CBlender_BmmD	()
{
}

void	CBlender_BmmD::Save		(IWriter& fs )
{
	IBlender::Save	(fs);
	xrPWRITE_MARKER	(fs,"Detail map");
	xrPWRITE_PROP	(fs,"Name",				xrPID_TEXTURE,	oT2_Name);
	xrPWRITE_PROP	(fs,"Transform",		xrPID_MATRIX,	oT2_xform);
	xrPWRITE_PROP	(fs,"R2-R",				xrPID_TEXTURE,	oR_Name);
	xrPWRITE_PROP	(fs,"R2-G",				xrPID_TEXTURE,	oG_Name);
	xrPWRITE_PROP	(fs,"R2-B",				xrPID_TEXTURE,	oB_Name);
	xrPWRITE_PROP	(fs,"R2-A",				xrPID_TEXTURE,	oA_Name);
}

void	CBlender_BmmD::Load		(IReader& fs, u16 version )
{
	IBlender::Load	(fs,version);
	if (version<3)	{
		xrPREAD_MARKER	(fs);
		xrPREAD_PROP	(fs,xrPID_TEXTURE,	oT2_Name);
		xrPREAD_PROP	(fs,xrPID_MATRIX,	oT2_xform);
	} else {
		xrPREAD_MARKER	(fs);
		xrPREAD_PROP	(fs,xrPID_TEXTURE,	oT2_Name);
		xrPREAD_PROP	(fs,xrPID_MATRIX,	oT2_xform);
		xrPREAD_PROP	(fs,xrPID_TEXTURE,	oR_Name);
		xrPREAD_PROP	(fs,xrPID_TEXTURE,	oG_Name);
		xrPREAD_PROP	(fs,xrPID_TEXTURE,	oB_Name);
		xrPREAD_PROP	(fs,xrPID_TEXTURE,	oA_Name);
	}
}

//////////////////////////////////////////////////////////////////////////
// R3
//////////////////////////////////////////////////////////////////////////
#include "dxRenderDeviceRender.h"
void	CBlender_BmmD::Compile	(CBlender_Compile& C)
{
	IBlender::Compile(C);

	C.SH->flags.bLandscape = true;
	if (C.L_textures.size()<2)	Debug.fatal	(DEBUG_INFO,"Not enought textures for shader, base tex: %s",*C.L_textures[0]);
	switch (C.iElement)
	{
	case ShaderVariant::NormalHQ:
#ifndef _EDITOR
		// Low shading quality draws the terrain with its one detail texture
		if (ps_r1_flags.test(R1FLAG_TERRAIN_MASK) && ps_r_shading_quality >= 1)
		{
			string256 mask;
			xr_strconcat(mask, C.L_textures[0].c_str(), "_mask");

			// From medium shading quality up the layers bring their normal and material maps
			const bool IsBumped = ps_r_shading_quality >= 1 && ps_r__common_flags.test(R2FLAG_USE_BUMP);

			if (IsBumped)
			{
				if (DEV->m_textures_description.UsePBRTexures(oR_Name))
				{
					C.AddDefine("USE_PBR", "1");
				}

				// A bumped surface mirrors the sky
				C.AddDefine("USE_SURFACE_IBL", "1");

				string256 Map;
				C.r_Pass("impl_dt_bump", "impl_dt_hq_bump", true);

				C.r_dx10Texture("sky_s0", r2_T_sky0);
				C.r_dx10Texture("sky_s1", r2_T_sky1);

				C.r_dx10Texture("s_dn_r", xr_strconcat(Map, oR_Name, "_bump"));
				C.r_dx10Texture("s_dn_g", xr_strconcat(Map, oG_Name, "_bump"));
				C.r_dx10Texture("s_dn_b", xr_strconcat(Map, oB_Name, "_bump"));
				C.r_dx10Texture("s_dn_a", xr_strconcat(Map, oA_Name, "_bump"));
				C.r_dx10Texture("s_dn_rX", xr_strconcat(Map, oR_Name, "_bump#"));
				C.r_dx10Texture("s_dn_gX", xr_strconcat(Map, oG_Name, "_bump#"));
				C.r_dx10Texture("s_dn_bX", xr_strconcat(Map, oB_Name, "_bump#"));
				C.r_dx10Texture("s_dn_aX", xr_strconcat(Map, oA_Name, "_bump#"));
			}
			else
			{
				C.r_Pass("impl_dt", "impl_dt_hq", true);
			}

			r1_tex(C, "s_mask", mask);
			r1_tex(C, "s_dt_r", oR_Name);
			r1_tex(C, "s_dt_g", oG_Name);
			r1_tex(C, "s_dt_b", oB_Name);
			r1_tex(C, "s_dt_a", oA_Name);
		} 
		else
#endif 
		{
			C.r_Pass("impl_dt", "impl_dt", true);
		}
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", C.L_textures[1]);
		r1_tex(C, "s_detail", oT2_Name);
		C.r_End			();
		break;
	case ShaderVariant::NormalLQ:
		C.r_Pass		("impl_dt",	"impl_dt", true);
		r1_tex(C, "s_base", C.L_textures[0]);
		r1_tex(C, "s_lmap", C.L_textures[1]);
		r1_tex(C, "s_detail", oT2_Name);
		C.r_End			();
		break;
	case ShaderVariant::Shadow:
		C.r_Pass("shadow_base", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	}
}
