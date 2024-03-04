#include "stdafx.h"
#include "uber_deffer.h"
void fix_texture_name(LPSTR fn);

#include "dxRenderDeviceRender.h"

void uber_deffer(CBlender_Compile& C, bool hq, LPCSTR vs, LPCSTR ps, BOOL aref, LPCSTR detail_replace, bool DO_NOT_FINISH)
{
	string256 fname, fnameA, fnameB;
	xr_strcpy(fname, *C.L_textures[0]);

	fix_texture_name(fname);
	ref_texture _t; _t.create(fname);

	bool bump = ps_r__common_flags.test(R2FLAG_USE_BUMP) && _t.bump_exist();

	bool lmap = false;
	if (C.L_textures.size() >= 3) {
		auto tex = C.L_textures[2].c_str();
		if (tex[0] == 'l' && tex[1] == 'm' && tex[2] == 'a' && tex[3] == 'p') {
			lmap = true;
		}
	}

	string256 dt;
	xr_strcpy(dt, sizeof(dt), detail_replace ? detail_replace : (C.detail_texture ? C.detail_texture : ""));

	string256 texDetailBump = { '\0' };
	string256 texDetailBumpX = { '\0' };
	bool bHasDetailBump = false;

	if (C.bDetail_Bump) {
		LPCSTR detail_bump_texture = DEV->m_textures_description.GetBumpName(dt).c_str();
		if (detail_bump_texture) {
			bHasDetailBump = true;
			xr_strcpy(texDetailBump, sizeof(texDetailBump), detail_bump_texture);
			xr_strcpy(texDetailBumpX, sizeof(texDetailBumpX), detail_bump_texture);
			xr_strcat(texDetailBumpX, "#");
		}
	}

	if (lmap) {
		RImplementation.addShaderOption("USE_LM_HEMI", "1");
	}

	if (aref) {
		RImplementation.addShaderOption("USE_AREF", "1");
	}

	if (bump) {
		RImplementation.addShaderOption("USE_BUMP", "1");

		xr_strcpy(fnameA, _t.bump_get().c_str());
		strconcat(sizeof(fnameB), fnameB, fnameA, "#");
	}
	else {
		fnameA[0] = fnameB[0] = 0;
	}

	if (C.bUseSteepParallax) {
		RImplementation.addShaderOption("USE_STEEPPARALLAX", "1");
	}

	if (C.bDetail_Diffuse) {
		RImplementation.addShaderOption("USE_TDETAIL", "1");
	}

	if (C.bDetail_Bump) {
		RImplementation.addShaderOption("USE_TDETAIL_BUMP", "1");
	}

	if (hq) {
		RImplementation.addShaderOption("USE_HIGH_QUALITY", "1");
	}

#ifdef USE_DX11
	if (bump && hq && RImplementation.o.dx11_enable_tessellation && C.TessMethod != 0) {
		string256 hs = "DX11\\tess", ds = "DX11\\tess";

		if (C.TessMethod == CBlender_Compile::TESS_PN || C.TessMethod == CBlender_Compile::TESS_PN_HM) {
			RImplementation.addShaderOption("TESS_PN", "1");
		}

		if (C.TessMethod == CBlender_Compile::TESS_HM || C.TessMethod == CBlender_Compile::TESS_PN_HM) {
			RImplementation.addShaderOption("TESS_HM", "1");
		}

		C.r_TessPass(vs, hs, ds, "null", ps, FALSE);

		u32 stage = C.r_dx10Sampler("smp_bump_ds");
		if (stage != u32(-1)) {
			C.i_dx10Address(stage, D3DTADDRESS_WRAP);
			C.i_dx10FilterAnizo(stage, TRUE);
		}

		if (ps_r2_ls_flags_ext.test(R2FLAGEXT_WIREFRAME)) {
			C.R().SetRS(D3DRS_FILLMODE, D3DFILL_WIREFRAME);
		}

		C.r_dx10Texture("s_tbump", fnameA);
		C.r_dx10Texture("s_tbumpX", fnameB);

		if (bHasDetailBump) {
			C.r_dx10Texture("s_tdetailBumpX", texDetailBumpX);
		}
	}
	else if (C.SH->flags.bLandscape) {
		C.r_Pass(vs, ps, FALSE, TRUE, FALSE);
	}
	else {
		C.r_Pass(vs, ps, FALSE);
	}

	C.r_dx10Texture("s_base", C.L_textures[0]);
	C.r_dx10Texture("s_bumpX", fnameB);
	C.r_dx10Texture("s_bump", fnameA);

	C.r_dx10Texture("s_detail", dt);

	if (bHasDetailBump) {
		C.r_dx10Texture("s_detailBump", texDetailBump);
		C.r_dx10Texture("s_detailBumpX", texDetailBumpX);
	}

	if (lmap) {
		C.r_dx10Texture("s_hemi", C.L_textures[2]);
	}

	C.r_dx10Sampler("smp_base");
	C.r_dx10Sampler("smp_rtlinear");
#else //USE_DX11
	C.r_Pass		(vs,ps,	FALSE);
	VERIFY(C.L_textures[0].size());
	if(bump)
	{
		VERIFY2(xr_strlen(fnameB), C.L_textures[0].c_str());
		VERIFY2(xr_strlen(fnameA), C.L_textures[0].c_str());
	}
	if(bHasDetailBump)
	{
		VERIFY2(xr_strlen(texDetailBump), C.L_textures[0].c_str());
		VERIFY2(xr_strlen(texDetailBumpX), C.L_textures[0].c_str());
	}
	C.r_Sampler		("s_base",		C.L_textures[0],	false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);
	C.r_Sampler		("s_bumpX",		fnameB,				false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);	// should be before base bump
	C.r_Sampler		("s_bump",		fnameA,				false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);
	C.r_Sampler		("s_bumpD",		dt,					false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);
	C.r_Sampler		("s_detail",	dt,					false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);
	if (bHasDetailBump)
	{
		C.r_Sampler		("s_detailBump", texDetailBump,	false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);
		C.r_Sampler		("s_detailBumpX",texDetailBumpX,false,	D3DTADDRESS_WRAP,	D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,	D3DTEXF_ANISOTROPIC);
	}
	if (lmap)C.r_Sampler("s_hemi",	C.L_textures[2],	false,	D3DTADDRESS_CLAMP,	D3DTEXF_LINEAR,		D3DTEXF_NONE,	D3DTEXF_LINEAR);
#endif

	if (!DO_NOT_FINISH) {
		C.r_End();
	}
}

#ifdef USE_DX11
void uber_shadow(CBlender_Compile& C, LPCSTR vspec, bool aref)
{
	string256 fname, fnameA, fnameB;
	xr_strcpy(fname, *C.L_textures[0]);

	fix_texture_name(fname);
	ref_texture _t; _t.create(fname);

	bool bump = ps_r__common_flags.test(R2FLAG_USE_BUMP) && _t.bump_exist();

	bool lmap = false;
	if (C.L_textures.size() >= 3) {
		auto tex = C.L_textures[2].c_str();
		if (tex[0] == 'l' && tex[1] == 'm' && tex[2] == 'a' && tex[3] == 'p') {
			lmap = true;
		}
	}

	string256 vs, dt;
	strconcat(sizeof(vs), vs, "deffer_", vspec);

	xr_strcpy(dt, sizeof(dt), C.detail_texture ? C.detail_texture : "");

	string256 texDetailBump = { '\0' };
	string256 texDetailBumpX = { '\0' };
	bool bHasDetailBump = false;

	if (C.bDetail_Bump) {
		LPCSTR detail_bump_texture = DEV->m_textures_description.GetBumpName(dt).c_str();
		if (detail_bump_texture) {
			bHasDetailBump = true;
			xr_strcpy(texDetailBump, sizeof(texDetailBump), detail_bump_texture);
			xr_strcpy(texDetailBumpX, sizeof(texDetailBumpX), detail_bump_texture);
			xr_strcat(texDetailBumpX, "#");
		}
	}

	if (aref) {
		RImplementation.addShaderOption("USE_AREF", "1");
	}

	if (bump) {
		RImplementation.addShaderOption("USE_BUMP", "1");

		xr_strcpy(fnameA, _t.bump_get().c_str());
		strconcat(sizeof(fnameB), fnameB, fnameA, "#");
	}
	else {
		fnameA[0] = fnameB[0] = 0;
	}

	if (C.bDetail_Diffuse) {
		RImplementation.addShaderOption("USE_TDETAIL", "1");
	}

	if (C.bDetail_Bump) {
		RImplementation.addShaderOption("USE_TDETAIL_BUMP", "1");
	}

	if (bump && RImplementation.o.dx11_enable_tessellation && C.TessMethod!=0)
	{
		string256 hs = "DX11\\tess", ds = "DX11\\tess_shadow";

		if (C.TessMethod == CBlender_Compile::TESS_PN || C.TessMethod == CBlender_Compile::TESS_PN_HM) {
			RImplementation.addShaderOption("TESS_PN", "1");
		}

		if (C.TessMethod == CBlender_Compile::TESS_HM || C.TessMethod == CBlender_Compile::TESS_PN_HM) {
			RImplementation.addShaderOption("TESS_HM", "1");
		}

		C.r_TessPass(vs, hs, ds, "null", "dumb", FALSE, TRUE, TRUE, FALSE);

		C.r_dx10Texture("s_base", C.L_textures[0]);
		C.r_dx10Texture("s_bumpX", fnameB);
		C.r_dx10Texture("s_bump", fnameA);

		if (bHasDetailBump) {
			C.r_dx10Texture("s_detailBump", texDetailBump);
			C.r_dx10Texture("s_detailBumpX", texDetailBumpX);
		}

		u32 stage = C.r_dx10Sampler("smp_bump_ds");
		if (stage != u32(-1)) {
			C.i_dx10Address(stage, D3DTADDRESS_WRAP);
			C.i_dx10FilterAnizo(stage, TRUE);
		}
	}
	else {
		C.r_Pass("shadow_direct_base", "dumb", FALSE, TRUE, TRUE, FALSE);
	}
}
#endif