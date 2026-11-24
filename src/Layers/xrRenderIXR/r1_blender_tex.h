/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once

#ifndef TEX_POINT_ATT
#define TEX_POINT_ATT	"internal\\internal_light_attpoint"
#endif
#ifndef TEX_SPOT_ATT
#define TEX_SPOT_ATT	"internal\\internal_light_attclip"
#endif

#include "dxRenderDeviceRender.h"

void fix_texture_name(char* fn);

// From medium shading quality up a surface whose base texture has a bump map takes the bumped permutation of its
// shader (<name>_bump) and that texture's two bump maps
struct R1Bump
{
	string64 Name;
	string256 Maps[2] = {};
	string256 DetailMaps[2] = {};
	bool IsOn = false;
	bool IsModel = false;
	bool IsDetailOrphan = false;

	R1Bump(CBlender_Compile& C, const char* Shader, bool CanBump = true)
	{
		string256 Base;
		xr_strcpy(Base, *C.L_textures[0]);
		fix_texture_name(Base);
		ref_texture Texture;
		Texture.create(Base);
		IsOn = CanBump && ps_r_shading_quality >= 1 && ps_r__common_flags.test(R2FLAG_USE_BUMP) && Texture.bump_exist();
		xr_strconcat(Name, Shader, IsOn ? "_bump" : "");

		// At high shading quality a detailed surface with a bump map of its own takes its detail texture's too
		const bool IsDetailed = ps_r_shading_quality >= 2 && C.bDetail_Bump && C.bDetail_Diffuse && C.detail_texture && strstr(Shader, "_dt");
		const shared_str DetailBump = IsDetailed ? DEV->m_textures_description.GetBumpName(C.detail_texture) : shared_str();
		IsDetailOrphan = DetailBump.size() && !Texture.bump_exist();

		if (!IsOn)
		{
			return;
		}

		xr_strcpy(Maps[0], Texture.bump_get().c_str());
		xr_strconcat(Maps[1], Maps[0], "#");

		if (DetailBump.size())
		{
			xr_strcpy(DetailMaps[0], DetailBump.c_str());
			xr_strconcat(DetailMaps[1], DetailMaps[0], "#");
			C.AddDefine("USE_TDETAIL_BUMP", "1");
		}

		if (DEV->m_textures_description.UsePBRTexures(Base))
		{
			C.AddDefine("USE_PBR", "1");
		}

		// A bumped surface mirrors the sky
		C.AddDefine("USE_SURFACE_IBL", "1");

		// A bumped model takes the HUD's raindrops: the HUD is drawn with the world's models
		IsModel = !strncmp(Shader, "model", 5);

		if (IsModel)
		{
			C.AddDefine("USE_SURFACE_HUD_RAIN", "1");
		}

		// The texture asks for parallax, as the blenders of the frame that shades a pixel once ask it
		if (DEV->m_textures_description.UseSteepParallax(Base))
		{
			C.AddDefine("USE_STEEPPARALLAX", "1");
		}
	}

	void Bind(CBlender_Compile& C) const
	{
		if (IsOn)
		{
			C.r_dx10Texture("s_bump", Maps[0]);
			C.r_dx10Texture("s_bumpX", Maps[1]);

			C.r_dx10Texture("sky_s0", r2_T_sky0);
			C.r_dx10Texture("sky_s1", r2_T_sky1);
		}

		if (DetailMaps[0][0])
		{
			C.r_dx10Texture("s_detailBump", DetailMaps[0]);
			C.r_dx10Texture("s_detailBumpX", DetailMaps[1]);
		}

		if (IsDetailOrphan && !C.IsCluster)
		{
			Msg("! [forward] texture [%s] has a detail bump but no bump of its own: drawn without the detail bump", C.L_textures[0].c_str());
		}

		if (IsModel)
		{
			C.r_dx10Texture("s_hud_rain", "fx\\hud_rain");
		}
	}
};

IC void r1_tex(CBlender_Compile& C, const char* name, const char* texture, bool = false, bool = false)
{
	C.r_dx10Texture(name, texture);

	// Every lit surface of the forward level frame reads the sun cascade and the occlusion; its base texture is the
	// one bind they all make
	if (!xr_strcmp(name, "s_base"))
	{
		C.r_dx10Texture("s_smap_sun", r2_RT_smap_depth_sun);
		C.r_dx10Texture("s_heightfield_shadow", R4_RT_HEIGHTFIELD_SHADOW);
		C.r_dx10Texture("s_sunmask", r2_sunmask);
		C.r_dx10Texture("s_smap", r2_RT_smap_depth);
		C.r_dx10Texture("s_occ", r2_RT_ssao_temp);
		C.r_dx10Texture("s_generic", "$user$hud_contact");
		C.IsCoveredPass = C.IsCoverCandidate;
	}
}

IC void r1_tex(CBlender_Compile& C, const char* name, const shared_str& texture, bool = false, bool = false)
{
	r1_tex(C, name, texture.c_str());
}
