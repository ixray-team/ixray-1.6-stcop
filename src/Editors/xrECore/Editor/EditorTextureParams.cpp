#include "stdafx.h"
#include "../xrEProps/ChooseTypesHelper.H"
#include "../../xrEngine/ETextureParams.H"

void EditorFillPropTextureParams(STextureParams* ThisCall, LPCSTR base_name, xr_vector<PropItem*>& items, PropValue::TOnChange OnChangeEvent)
{
	ThisCall->OnTypeChangeEvent = OnChangeEvent;
	PropValue* P = PHelper().CreateToken32(items, "Type", (u32*)&ThisCall->type, ttype_token);
	P->OnChangeEvent.bind(ThisCall, &STextureParams::OnTypeChange);
	PHelper().CreateCaption(items, "Source\\Width", shared_str().printf("%d", ThisCall->width));
	PHelper().CreateCaption(items, "Source\\Height", shared_str().printf("%d", ThisCall->height));
	PHelper().CreateCaption(items, "Source\\Alpha", ThisCall->HasAlpha() ? "present" : "absent");

	switch (ThisCall->type)
	{
		case STextureParams::ttImage:
		case STextureParams::ttCubeMap:
			PHelper().CreateToken32(items, "Format", (u32*)&ThisCall->fmt, tfmt_token);

			PHelper().CreateFlag32(items, "MipMaps\\Enabled", &ThisCall->flags, STextureParams::flGenerateMipMaps);
			PHelper().CreateToken32(items, "MipMaps\\Filter", (u32*)&ThisCall->mip_filter, tparam_token);

			P = PHelper().CreateToken32(items, "Bump\\Mode", (u32*)&ThisCall->bump_mode, tbmode_token);
			P->OnChangeEvent.bind(ThisCall, &STextureParams::OnTypeChange);
			if (STextureParams::tbmUse == ThisCall->bump_mode || STextureParams::tbmUseParallax == ThisCall->bump_mode)
			{
				xr_string path;
				path = base_name;
				PHelper().CreateChoose(items, "Bump\\Texture", &ThisCall->bump_name, smTexture, path.c_str());
			}

			PHelper().CreateFlag32(items, "Details\\Use As Diffuse", &ThisCall->flags, STextureParams::flDiffuseDetail);
			PHelper().CreateFlag32(items, "Details\\Use As Bump (R2)", &ThisCall->flags, STextureParams::flBumpDetail);
			PHelper().CreateChoose(items, "Details\\Texture", &ThisCall->detail_name, smTexture);
			PHelper().CreateFloat(items, "Details\\Scale", &ThisCall->detail_scale, 0.1f, 10000.f, 0.1f, 2);

			PHelper().CreateToken32(items, "Material\\Base", (u32*)&ThisCall->material, tmtl_token);
			PHelper().CreateFloat(items, "Material\\Weight", &ThisCall->material_weight);

			//		PHelper().CreateFlag32		(items, "Flags\\Binary Alpha",		&flags,				flBinaryAlpha);
			PHelper().CreateFlag32(items, "Flags\\Dither", &ThisCall->flags, STextureParams::flDitherColor);
			PHelper().CreateFlag32(items, "Flags\\Dither Each MIP", &ThisCall->flags, STextureParams::flDitherEachMIPLevel);
			PHelper().CreateFlag32(items, "Flags\\Implicit Lighted", &ThisCall->flags, STextureParams::flImplicitLighted);

			PHelper().CreateFlag32(items, "Fade\\Enable Color", &ThisCall->flags, STextureParams::flFadeToColor);
			PHelper().CreateFlag32(items, "Fade\\Enabled Alpha", &ThisCall->flags, STextureParams::flFadeToAlpha);
			PHelper().CreateU8(items, "Fade\\Delay 'n' MIP", &ThisCall->fade_delay, 0, 255);
			PHelper().CreateU32(items, "Fade\\% of color to fade in", &ThisCall->fade_amount, 0, 100, 0);
			PHelper().CreateColor(items, "Fade\\Color", &ThisCall->fade_color);
			PHelper().CreateU8(items, "Fade\\Alpha", ((u8*)&ThisCall->fade_color) + 3, 0, 255);

			PHelper().CreateFlag32(items, "Border\\Enabled Color", &ThisCall->flags, STextureParams::flColorBorder);
			PHelper().CreateFlag32(items, "Border\\Enabled Alpha", &ThisCall->flags, STextureParams::flAlphaBorder);
			PHelper().CreateColor(items, "Border\\Color", &ThisCall->border_color);
			break;
		case STextureParams::ttBumpMap:
			PHelper().CreateChoose(items, "Bump\\Special NormalMap", &ThisCall->ext_normal_map_name, smTexture, base_name);
			PHelper().CreateFloat(items, "Bump\\Virtual Height (m)", &ThisCall->bump_virtual_height, 0.f, 0.1f, 0.001f, 3);
			break;
		case STextureParams::ttNormalMap:
			P = PHelper().CreateToken32(items, "Format", (u32*)&ThisCall->fmt, tfmt_token); P->Owner()->Enable(false);

			PHelper().CreateFlag32(items, "MipMaps\\Enabled", &ThisCall->flags, STextureParams::flGenerateMipMaps);
			PHelper().CreateToken32(items, "MipMaps\\Filter", (u32*)&ThisCall->mip_filter, tparam_token);
			break;
		case STextureParams::ttTerrain:
			P = PHelper().CreateToken32(items, "Format", (u32*)&ThisCall->fmt, tfmt_token); P->Owner()->Enable(false);

			PHelper().CreateFlag32(items, "Details\\Use As Diffuse", &ThisCall->flags, STextureParams::flDiffuseDetail);
			PHelper().CreateFlag32(items, "Details\\Use As Bump (R2)", &ThisCall->flags, STextureParams::flBumpDetail);
			PHelper().CreateChoose(items, "Details\\Texture", &ThisCall->detail_name, smTexture);
			PHelper().CreateFloat(items, "Details\\Scale", &ThisCall->detail_scale, 0.1f, 10000.f, 0.1f, 2);

			PHelper().CreateToken32(items, "Material\\Base", (u32*)&ThisCall->material, tmtl_token);
			PHelper().CreateFloat(items, "Material\\Weight", &ThisCall->material_weight);

			P = PHelper().CreateFlag32(items, "Flags\\Implicit Lighted", &ThisCall->flags, STextureParams::flImplicitLighted);  P->Owner()->Enable(false);
			break;
	}
}