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
	// codepath is the same, only the shaders differ
	// ***only pixel shaders differ***
	string256 mask;
	xr_strconcat(mask, C.L_textures[0].c_str(), "_mask");

	if (LightingModeIsStatic() && !C.bEditor)
	{
		C.SH->flags.bLandscape = true;
		if (C.L_textures.size()<2)	Debug.fatal	(DEBUG_INFO,"Not enought textures for shader, base tex: %s",*C.L_textures[0]);
		switch (C.iElement)
		{
		case SE_R1_NORMAL_HQ:
#ifndef _EDITOR
			if (ps_r1_flags.test(R1FLAG_TERRAIN_MASK)) {
				string256 mask;
				xr_strconcat(mask, C.L_textures[0].c_str(), "_mask");

				C.r_Pass("impl_dt", "impl_dt_hq", true);
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
		case SE_R1_NORMAL_LQ:
			C.r_Pass		("impl_dt",	"impl_dt", true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			r1_tex(C, "s_detail", oT2_Name);
			C.r_End			();
			break;
		case SE_R1_LPOINT:
			C.r_Pass("impl_point", "add_point", false, true, false, true, D3DBLEND_ONE, D3DBLEND_ONE, true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", TEX_POINT_ATT, true);
			r1_tex(C, "s_att", TEX_POINT_ATT, true);
			C.r_End			();
			break;
		case SE_R1_LSPOT:
			C.r_Pass("impl_spot", "add_spot", false, true, false, true, D3DBLEND_ONE, D3DBLEND_ONE, true);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", "internal\\internal_light_att", true);
			r1_tex(C, "s_att", TEX_SPOT_ATT, true);
			C.r_End			();
			break;
		case SE_R1_LMODELS:
			C.r_Pass		("impl_l","impl_l", false);
			r1_tex(C, "s_base", C.L_textures[0]);
			r1_tex(C, "s_lmap", C.L_textures[1]);
			C.r_End			();
			break;
		}
	
		return;
	}

	if (C.bEditor) {

		string256 mask;
		xr_strconcat(mask, C.L_textures[0].c_str(), "_mask");

		//	C.r_Pass("impl_dt", "impl_dt", TRUE);
		uber_deffer(C, true, "deffer_base", "deffer_impl", false, oT2_Name[0] ? oT2_Name : 0, true);
		C.r_dx10Texture("s_mask", mask);

		C.r_dx10Texture("s_dt_r", oR_Name);
		C.r_dx10Texture("s_dt_g", oG_Name);
		C.r_dx10Texture("s_dt_b", oB_Name);
		C.r_dx10Texture("s_dt_a", oA_Name);

		C.r_dx10Texture("s_dn_r", xr_strconcat(mask, oR_Name, "_bump"));
		C.r_dx10Texture("s_dn_g", xr_strconcat(mask, oG_Name, "_bump"));
		C.r_dx10Texture("s_dn_b", xr_strconcat(mask, oB_Name, "_bump"));
		C.r_dx10Texture("s_dn_a", xr_strconcat(mask, oA_Name, "_bump"));
		C.r_dx10Texture("s_detail", oT2_Name);

		C.r_End();
		return;
	}

	RImplementation.addShaderOption("USE_LM_HEMI", "1");
	RImplementation.addShaderOption("USE_TDETAIL_BUMP", "1");

	bool isSpecularR = false;
	bool isSpecularG = false;
	bool isSpecularB = false;
	bool isSpecularA = false;

	switch(C.iElement) {
	case SE_R2_NORMAL_HQ:
	case SE_R2_NORMAL_LQ:
		C.SH->flags.bLandscape = false;

		//C.r_Pass("deffer_base", "dumb", FALSE, TRUE, TRUE);
		//C.r_ColorWriteEnable(false, false, false, false);
		//C.r_End(false);

		C.bDetail_Bump = C.bDetail_Diffuse;

		if(C.iElement == SE_R2_NORMAL_HQ)
		{
			RImplementation.addShaderOption("USE_4_BUMP", "");

			string_path temp { };

			auto IsSpecularExist = [&temp](LPCSTR texture)
			{
				bool specular_texture = FS.exist(temp, "$textures$", texture, "_spec.dds");
				specular_texture = specular_texture || FS.exist(temp, "$level$", texture, "_spec.dds");

				return specular_texture;
			};

			if (IsSpecularExist(oR_Name))
			{
				isSpecularR = true;
				RImplementation.addShaderOption("USE_4_R_IOR_TEXTURE", "1");
			}

			if (IsSpecularExist(oG_Name))
			{
				isSpecularG = true;
				RImplementation.addShaderOption("USE_4_G_IOR_TEXTURE", "1");
			}

			if (IsSpecularExist(oB_Name))
			{
				isSpecularB = true;
				RImplementation.addShaderOption("USE_4_B_IOR_TEXTURE", "1");
			}

			if (IsSpecularExist(oA_Name))
			{
				isSpecularA = true;
				RImplementation.addShaderOption("USE_4_A_IOR_TEXTURE", "1");
			}
		}

		uber_deffer(C, true, "deffer_base", "deffer_impl", false, oT2_Name[0] ? oT2_Name : 0, true);
	//	C.RS.SetRS(D3DRS_ZFUNC, D3D11_COMPARISON_EQUAL);

		if(C.iElement == SE_R2_NORMAL_HQ)
		{
			for(const char* unused : { "s_bump", "s_bumpX", "s_detail", "s_detailBump", "s_detailBumpX" })
				C.r_dx10Unbind(unused);
		}

		C.r_dx10Texture("s_lmap", C.L_textures[1]);

		if(C.iElement == SE_R2_NORMAL_HQ) 
		{
			C.r_dx10Texture("s_mask", mask);

			C.r_dx10Texture("s_dt_r", oR_Name);
			C.r_dx10Texture("s_dt_g", oG_Name);
			C.r_dx10Texture("s_dt_b", oB_Name);
			C.r_dx10Texture("s_dt_a", oA_Name);

			C.r_dx10Texture("s_dn_r", xr_strconcat(mask, oR_Name, "_bump"));
			C.r_dx10Texture("s_dn_g", xr_strconcat(mask, oG_Name, "_bump"));
			C.r_dx10Texture("s_dn_b", xr_strconcat(mask, oB_Name, "_bump"));
			C.r_dx10Texture("s_dn_a", xr_strconcat(mask, oA_Name, "_bump"));

			C.r_dx10Texture("s_dn_rX", xr_strconcat(mask, oR_Name, "_bump#"));
			C.r_dx10Texture("s_dn_gX", xr_strconcat(mask, oG_Name, "_bump#"));
			C.r_dx10Texture("s_dn_bX", xr_strconcat(mask, oB_Name, "_bump#"));
			C.r_dx10Texture("s_dn_aX", xr_strconcat(mask, oA_Name, "_bump#"));

			if (isSpecularR)
			{
				C.r_dx10Texture("s_dt_spec_r", xr_strconcat(mask, oR_Name, "_spec"));
			}

			if (isSpecularG)
			{
				C.r_dx10Texture("s_dt_spec_g", xr_strconcat(mask, oG_Name, "_spec"));
			}

			if (isSpecularB)
			{
				C.r_dx10Texture("s_dt_spec_b", xr_strconcat(mask, oB_Name, "_spec"));
			}

			if (isSpecularA)
			{
				C.r_dx10Texture("s_dt_spec_a", xr_strconcat(mask, oA_Name, "_spec"));
			}
		}

		C.r_Stencil(true, D3DCMP_ALWAYS, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		C.r_StencilRef(0x01);

		C.r_End ();
		break;
	case SE_R2_SHADOW:
		C.r_Pass("shadow_base", "shadow_base", false);
		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_dx10Sampler("smp_base");
		C.r_dx10Sampler("smp_linear");
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_End();
		break;
	case SE_R2_REFLECTIONS:
		C.SH->flags.bLandscape = false;
		C.bDetail = !!DEV->m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);
		C.bDetail_Bump = C.bDetail_Diffuse = C.bDetail;

		RImplementation.addShaderOption("USE_LENGTH_BUFFER", "1");
		RImplementation.addShaderOption("DISABLE_MOTION_VECTORS", "1");

		uber_deffer(C, true, "deffer_base", "deffer_impl", false, oT2_Name[0] ? oT2_Name : 0, true);

		C.r_dx10Texture("s_lmap", C.L_textures[1]);

		C.r_dx10Texture("s_material", r2_material);
		C.r_dx10Texture("env_s0", r2_T_envs0);
		C.r_dx10Texture("env_s1", r2_T_envs1);
		C.r_dx10Texture("sky_s0", r2_T_sky0);
		C.r_dx10Texture("sky_s1", r2_T_sky1);

		C.r_dx10Sampler("smp_material");
		C.r_End();
		break;
	}

	RImplementation.clearAllShaderOptions();
}
