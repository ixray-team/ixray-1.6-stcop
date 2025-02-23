#include "stdafx.h"
#include "uber_deffer.h"

#include "dxRenderDeviceRender.h"
void fix_texture_name(LPSTR fn);

void uber_deffer(CBlender_Compile& C, bool hq, LPCSTR vs, LPCSTR ps, BOOL aref, LPCSTR detail_replace, bool DO_NOT_FINISH)
{
	string256 fname, fnameA, fnameB;
	xr_strcpy(fname, *C.L_textures[0]);

	fix_texture_name(fname);
	ref_texture pTexture; pTexture.create(fname);

#ifdef _EDITOR
	ps_r__common_flags.set(R2FLAG_USE_BUMP, TRUE);
#endif

	bool bump = ps_r__common_flags.test(R2FLAG_USE_BUMP) && pTexture.bump_exist();

#ifndef _EDITOR
	bool lmap = false;
	if(C.L_textures.size() >= 3) {
		auto tex = C.L_textures[2].c_str();
		if(tex[0] == 'l' && tex[1] == 'm' && tex[2] == 'a' && tex[3] == 'p') {
			lmap = true;
		}
	}
#else 
	bool lmap = false;
#endif

	string256 dt;
	xr_strcpy(dt, sizeof(dt), detail_replace ? detail_replace : (C.detail_texture ? C.detail_texture : ""));

	string256 texDetailBump = { '\0' };
	string256 texDetailBumpX = { '\0' };
	bool bHasDetailBump = false;

	if(C.bDetail_Bump) {
		LPCSTR detail_bump_texture = DEV->m_textures_description.GetBumpName(dt).c_str();
		if(detail_bump_texture) {
			bHasDetailBump = true;
			xr_strcpy(texDetailBump, sizeof(texDetailBump), detail_bump_texture);
			xr_strcpy(texDetailBumpX, sizeof(texDetailBumpX), detail_bump_texture);
			xr_strcat(texDetailBumpX, "#");
		}
	}

	if(lmap) {
		RImplementation.addShaderOption("USE_LM_HEMI", "1");
	}

	if(aref) {
		RImplementation.addShaderOption("USE_AREF", "1");
#ifdef USE_DX11
		pTexture->Load();
		auto Format = pTexture->get_Format();

		if(Format >= DXGI_FORMAT_BC1_TYPELESS && Format < DXGI_FORMAT_BC2_TYPELESS) {
			RImplementation.addShaderOption("USE_DXT1_HACK", "1");
		}
#endif
	}

	if(!!DEV->m_textures_description.UsePBRTexures(fname)) {
		RImplementation.addShaderOption("USE_PBR", "1");
	}

	if(bump) {
		RImplementation.addShaderOption("USE_BUMP", "1");

		xr_strcpy(fnameA, pTexture.bump_get().c_str());
		xr_strconcat(fnameB, fnameA, "#");
	}
	else {
		fnameA[0] = fnameB[0] = 0;
	}

	if(C.bUseSteepParallax) {
		RImplementation.addShaderOption("USE_STEEPPARALLAX", "1");
	}

	if(C.bDetail_Diffuse) {
		RImplementation.addShaderOption("USE_TDETAIL", "1");
	}

	if(C.bDetail_Bump && C.bDetail_Diffuse) {
		RImplementation.addShaderOption("USE_TDETAIL_BUMP", "1");
	}

	if(hq) {
		RImplementation.addShaderOption("USE_HIGH_QUALITY", "1");
	}

	if(bump) {

		string512 errorMsg;
		xr_sprintf(errorMsg, "Missing bump texture: %s\n"
			"                         Loading texture: %s", dt, C.L_textures[0].c_str());

		R_ASSERT3(fnameB[0] && xr_strlen(fnameB), errorMsg,  "Missing bump texture\n");
		R_ASSERT3(fnameA[0] && xr_strlen(fnameA), errorMsg,  "Missing bump texture\n");
	}

	if(bHasDetailBump)
	{
		string512 errorMsg;
		xr_sprintf(errorMsg, "Missing detail texture: %s\n"
			"                         Loading texture: %s", dt, C.L_textures[0].c_str());

		R_ASSERT3(texDetailBump[0] && xr_strlen(texDetailBump), errorMsg, "Missing detail texture");
		R_ASSERT3(texDetailBumpX[0] && xr_strlen(texDetailBumpX), errorMsg, "Missing detail texture");
	}

#ifdef USE_DX11
	if (bump && hq && RImplementation.o.dx11_enable_tessellation && C.TessMethod != 0) {
		string256 hs = "tess", ds = "tess";

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

		if(bump) {
			C.r_dx10Texture("s_tbump", fnameA);
			C.r_dx10Texture("s_tbumpX", fnameB);
		}

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

	if(bump) {
		C.r_dx10Texture("s_bumpX", fnameB);
		C.r_dx10Texture("s_bump", fnameA);
	}

	if(dt && dt[0]) {
		C.r_dx10Texture("s_detail", dt);
	}

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
	C.r_Pass(vs, ps, FALSE);

	C.r_Sampler_waf("s_base", C.L_textures[0].c_str(), false);
	C.r_Sampler_waf("s_bumpX", fnameB, false);
	C.r_Sampler_waf("s_bump", fnameA, false);
	C.r_Sampler_waf("s_bumpD", dt, false);
	C.r_Sampler_waf("s_detail", dt, false);

	if(bHasDetailBump) {
		C.r_Sampler_waf("s_detailBump", texDetailBump, false);
		C.r_Sampler_waf("s_detailBumpX", texDetailBumpX, false);
	}

	if(lmap) {
		C.r_Sampler_clf("s_hemi", C.L_textures[2].c_str(), false);
	}
#endif

#ifdef _EDITOR
	C.r_Sampler_clw("s_material", "shaders\\r2_material");
	C.r_Sampler("env_s0", "$user$env_s0");
	C.r_Sampler("env_s1", "$user$env_s1");
	C.r_Sampler("sky_s0", "$user$sky0");
	C.r_Sampler("sky_s1", "$user$sky1");
#endif

	if (!DO_NOT_FINISH) {
		C.r_End();
	}
}
