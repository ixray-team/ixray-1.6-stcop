#include "stdafx.h"
#include "uber_deffer.h"

#include "dxRenderDeviceRender.h"
void fix_texture_name(LPSTR fn);

void uber_deffer(CBlender_Compile& C, bool hq, const char* vs, const char* ps, bool aref, const char* detail_replace, bool DO_NOT_FINISH, bool DO_NOT_START)
{
	string256 fname, fnameA, fnameB;
	xr_strcpy(fname, *C.L_textures[0]);

	fix_texture_name(fname);
	ref_texture pTexture; pTexture.create(fname);

#ifdef _EDITOR
	ps_r__common_flags.set(R2FLAG_USE_BUMP, true);
#endif

	bool bump = ps_r__common_flags.test(R2FLAG_USE_BUMP) && pTexture.bump_exist();
	bool lmap = false;

#ifndef _EDITOR
	if(C.L_textures.size() >= 3)
	{
		auto tex = C.L_textures[2].c_str();
		if(tex[0] == 'l' && tex[1] == 'm' && tex[2] == 'a' && tex[3] == 'p')
		{
			lmap = true;
		}
	}
#endif

	string256 dt;
	xr_strcpy(dt, sizeof(dt), detail_replace ? detail_replace : (C.detail_texture ? C.detail_texture : ""));

	string256 texDetailBump = { '\0' };
	string256 texDetailBumpX = { '\0' };
	bool bHasDetailBump = false;

	if(C.bDetail_Bump)
	{
		const char* detail_bump_texture = DEV->m_textures_description.GetBumpName(dt).c_str();
		if(detail_bump_texture != nullptr && detail_bump_texture[0] != '\0') {
			bHasDetailBump = true;
			xr_strcpy(texDetailBump, sizeof(texDetailBump), detail_bump_texture);
			xr_strcpy(texDetailBumpX, sizeof(texDetailBumpX), detail_bump_texture);
			xr_strcat(texDetailBumpX, "#");
		}
		else
		{
			if (dt && dt[0])
			{
				Msg("! Texture [%s] has no detail bump [%s]", C.L_textures[0].c_str(), dt);
			}
			else
			{
				Msg("! Texture [%s] has no detail bump", C.L_textures[0].c_str());
			}
		}
	}

	if(lmap) 
	{
		RImplementation.addShaderOption("USE_LM_HEMI", "1");
	}

	if (ps_r2_ls_flags_ext.test(R2FLAGEXT_WIREFRAME))
	{
		aref = false;
	}

	if(aref)
	{
		RImplementation.addShaderOption("USE_AREF", "1");
#ifdef USE_DX11
		pTexture->Load();

		ERHI_FORMAT Format = pTexture->get_Format();
		if(Format >= ERHI_FORMAT::BC1_TYPELESS && Format < ERHI_FORMAT::BC2_TYPELESS)
		{
			RImplementation.addShaderOption("USE_DXT1_HACK", "1");
		}
#endif
	}

	if(!!DEV->m_textures_description.UsePBRTexures(fname))
	{
		RImplementation.addShaderOption("USE_PBR", "1");
	}

	if(bump)
	{
		RImplementation.addShaderOption("USE_BUMP", "1");

		xr_strcpy(fnameA, pTexture.bump_get().c_str());
		xr_strconcat(fnameB, fnameA, "#");
	}
	else
	{
		fnameA[0] = fnameB[0] = 0;
	}

	if(C.bUseSteepParallax)
	{
		RImplementation.addShaderOption("USE_STEEPPARALLAX", "1");
	}

	if(dt && dt[0] && C.bDetail_Diffuse)
	{
		RImplementation.addShaderOption("USE_TDETAIL", "1");
	}

	if (C.bDetail_Diffuse)
	{
		if (!dt || !dt[0])
		{
			Msg("! Texture [%s] has no detail texture", C.L_textures[0].c_str());
		}
	}

	if(bHasDetailBump && dt && dt[0] && C.bDetail_Diffuse)
	{
		RImplementation.addShaderOption("USE_TDETAIL_BUMP", "1");
	}

	if(hq)
	{
		RImplementation.addShaderOption("USE_HIGH_QUALITY", "1");
	}

	if(bump)
	{
		string512 errorMsg;
		xr_sprintf(errorMsg, "Missing bump texture: %s\n\t\t\tLoading texture: %s", dt, C.L_textures[0].c_str());

		R_ASSERT3(fnameB[0] && xr_strlen(fnameB), errorMsg,  "Missing bump texture\n");
		R_ASSERT3(fnameA[0] && xr_strlen(fnameA), errorMsg,  "Missing bump texture\n");
	}

	string_path temp;

	static bool UseWinterPass = EngineExternal()[EEngineExternalRender::UseDynamicSnowMask];
	bool snow_texture = UseWinterPass && FS.exist(temp, _textures_, C.L_textures[0].c_str(), "_snowmask.dds");

	if (snow_texture)
	{
		RImplementation.addShaderOption("USE_SNOW_TEXTURE", "1");
	}
	
	bool hair_texture = FS.exist(temp, _textures_, C.L_textures[0].c_str(), "_hairmask.dds");

	if (hair_texture)
	{
		RImplementation.addShaderOption("USE_HAIRMASK", "1");
	}
	
	bool specular_texture = FS.exist(temp, "$textures$", C.L_textures[0].c_str(), "_spec.dds");
	specular_texture = specular_texture || FS.exist(temp, "$level$", C.L_textures[0].c_str(), "_spec.dds");

	if (specular_texture)
	{
		RImplementation.addShaderOption("USE_IOR_TEXTURE", "1");
	}

	if(bHasDetailBump)
	{
		string512 errorMsg;
		xr_sprintf(errorMsg, "Missing detail texture: %s\n\t\t\tLoading texture: %s", dt, C.L_textures[0].c_str());

		R_ASSERT3(texDetailBump[0] && xr_strlen(texDetailBump), errorMsg, "Missing detail texture");
		R_ASSERT3(texDetailBumpX[0] && xr_strlen(texDetailBumpX), errorMsg, "Missing detail texture");
	}

	if (auto pShaderOptions = DEV->m_textures_description.GetShaderExternal(fname))
	{
		for (auto& [Name, Value] : *pShaderOptions)
		{
			RImplementation.addShaderOption(Name.data(), Value.data());
		}

		if (hq && pShaderOptions->contains(xr_string("USE_PARRALAX_INTERIOR")))
		{
			C.r_Pass(vs, "forwrad_interior", FALSE, TRUE, FALSE, TRUE, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA);
			C.RS.SetRS(D3DRS_ZFUNC, D3D11_COMPARISON_EQUAL);
			C.SetPassPriority(3);

			C.r_dx10Texture("s_base", C.L_textures[0]);
			C.r_dx10Texture("s_env", "newsky_reflection_lobby_room");

			C.r_dx10Texture("env_s0", r2_T_envs0);
			C.r_dx10Texture("env_s1", r2_T_envs1);

			if (lmap) 
			{
				C.r_dx10Texture("s_hemi", C.L_textures[2]);
			}

			C.r_dx10Texture("s_smap_sun", r2_RT_smap_depth_sun);
			C.r_dx10Sampler("smp_smap");
		
			C.r_dx10Sampler("smp_base");
			C.r_dx10Sampler("smp_linear");
		
			C.r_End(false);
		}
	}

	C.RS.SetRS(D3DRS_ZFUNC, D3D11_COMPARISON_LESS_EQUAL);

#ifdef USE_DX11
	if (bump && hq && RImplementation.o.dx11_enable_tessellation && C.TessMethod != CBlender_Compile::NO_TESS)
	{
		string256 hs = "tess", ds = "tess";

		if (C.TessMethod & CBlender_Compile::TESS_PN) 
		{
			RImplementation.addShaderOption("TESS_PN", "1");
		}

		if (C.TessMethod & CBlender_Compile::TESS_HM) 
		{
			RImplementation.addShaderOption("TESS_HM", "1");
		}

		C.r_TessPass(vs, hs, ds, "null", ps, FALSE);
	}
	else if (!DO_NOT_START)
	{
		if (C.SH->flags.bLandscape)
		{
			C.r_Pass(vs, ps, false, true, false);
		}
		else
		{
			C.r_Pass(vs, ps, false);
		}
	}

	if (ps_r2_ls_flags_ext.test(R2FLAGEXT_WIREFRAME))
	{
		C.R().SetRS(D3DRS_FILLMODE, D3DFILL_WIREFRAME);
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
		C.r_dx10Texture("s_lmap", C.L_textures[1]);
		C.r_dx10Texture("s_hemi", C.L_textures[2]);
	}

	string256 Path = {};

	if (snow_texture)
	{
		xr_strconcat(Path, *C.L_textures[0], "_snowmask");
		C.r_dx10Texture("s_snow", Path);
	}

	if (hair_texture)
	{
		string256 Path = {};
		xr_strconcat(Path, *C.L_textures[0], "_hairmask");
		C.r_dx10Texture("s_hair", Path);
	}

	if (specular_texture)
	{
		xr_strconcat(Path, *C.L_textures[0], "_spec");
		C.r_dx10Texture("s_specular", Path);
	}

	C.r_dx10Texture("s_smap_sun", r2_RT_smap_depth_sun);
	C.r_dx10Sampler("smp_smap");

	C.r_dx10Sampler("smp_base");
	C.r_dx10Sampler("smp_linear");
	C.r_dx10Sampler("smp_rtlinear");
	C.r_dx10Sampler("smp_nofilter");

#else //USE_DX11

	if (!DO_NOT_START)
	{
		C.r_Pass(vs, ps, false);
	}

	C.r_Sampler_waf("s_base", C.L_textures[0].c_str(), false);

	if (bump)
	{
		C.r_Sampler_waf("s_bumpX", fnameB, false);
		C.r_Sampler_waf("s_bump", fnameA, false);
		C.r_Sampler_waf("s_bumpD", dt, false);
	}

	if (dt && dt[0])
	{
		C.r_Sampler_waf("s_detail", dt, false);
	}

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

	if (!DO_NOT_FINISH) 
	{
		C.r_End();
	}
}

void uber_forward(CBlender_Compile& C, bool hq, const char* vs, const char* ps, bool aref, bool blend, const char* detail_replace, bool DO_NOT_FINISH, bool DO_NOT_START)
{
	uber_deffer(C, hq, vs, ps, aref, detail_replace, true, DO_NOT_START);

	if (blend) 
	{
		C.PassSET_ZB(true, false);
		C.PassSET_Blend(true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA, true, 0);
	}

#ifndef _EDITOR
	#ifdef USE_DX11
		C.r_dx10Texture("s_material", r2_material);
		C.r_dx10Texture("s_position", r2_RT_P);

		C.r_dx10Texture("env_s0", r2_T_envs0);
		C.r_dx10Texture("env_s1", r2_T_envs1);

		C.r_dx10Texture("sky_s0", r2_T_sky0);
		C.r_dx10Texture("sky_s1", r2_T_sky1);

		C.r_dx10Texture("s_env", r2_RT_env_temp);

		C.r_dx10Sampler("smp_material");
	#elif RENDER==R_R2
		C.r_Sampler("s_material", r2_material);

		C.r_Sampler("env_s0", r2_T_envs0);
		C.r_Sampler("env_s1", r2_T_envs1);

		C.r_Sampler("sky_s0", r2_T_sky0);
		C.r_Sampler("sky_s1", r2_T_sky1);
	#endif
#endif

	if(!DO_NOT_FINISH)
	{
		C.r_End();
	}
}