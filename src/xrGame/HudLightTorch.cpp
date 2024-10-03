#include "stdafx.h"

#include "HudLightTorch.h"

#include "player_hud.h"
#include "HudItem.h"
#include "physic_item.h"
#include "CustomDetector.h"
#include "Weapon.h"
#include "Inventory.h"

void TransformToHudTemp(Fvector& pos) {
	Fmatrix inv_v;
	inv_v.invert(Device.mView);
	Fmatrix inv_p;
	inv_p.invert(Device.mProject);

	Device.mView.transform_tiny(pos);
	Device.mProject_hud.transform_tiny(pos);

	inv_p.transform_tiny(pos);
	inv_v.transform_tiny(pos);
}

void TransformToHud(Fvector& pos, Fvector& dir) {
	dir.add(pos);
	TransformToHudTemp(dir);
	TransformToHudTemp(pos);
	dir = dir.sub(pos).normalize();
}

HudLightTorch::~HudLightTorch() {
	if (RenderLight != nullptr)
	{
		RenderLight.destroy();
	}

	if (OmniLight != nullptr)
	{
		OmniLight.destroy();
	}
}

void HudLightTorch::NewTorchlight(const char* section) {
	if (!READ_IF_EXISTS(pSettings, r_bool, section, "torch_installed", FALSE))
	{
		return;
	}

	IsTorchInstalled = true;

	if (RenderLight)
	{
		RenderLight.destroy();
	}

	if (OmniLight)
	{
		OmniLight.destroy();
	}

	Section = section;

	RenderLight = ::Render->light_create();
	RenderLight->set_type((IRender_Light::LT)READ_IF_EXISTS(pSettings, r_u32, section, "torch_render_type", IRender_Light::SPOT));
	RenderLight->set_range(READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_range", 15.0f));
	RenderLight->set_shadow(!!READ_IF_EXISTS(pSettings, r_bool, section, "torch_render_shadow", TRUE));

	OmniLight = ::Render->light_create();
	OmniLight->set_type(IRender_Light::POINT); // (IRender_Light::LT)READ_IF_EXISTS(pSettings, r_u32, section, "torch_omni_type", IRender_Light::POINT));
	OmniLight->set_range(READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_omni_range", 0.75f));
	OmniLight->set_shadow(!!READ_IF_EXISTS(pSettings, r_bool, section, "torch_omni_shadow", FALSE));

	LightBone = pSettings->r_string(section, "torch_light_bone");

	LightOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "torch_attach_offset_x", 0.0f);
	LightOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "torch_attach_offset_y", 0.0f);
	LightOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "torch_attach_offset_z", 0.0f);

	AimOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "torch_aim_attach_offset_x", 0.0f);
	AimOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "torch_aim_attach_offset_y", 0.0f);
	AimOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "torch_aim_attach_offset_z", 0.0f);

	LightWorldOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "torch_world_attach_offset_x", 0.0f);
	LightWorldOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "torch_world_attach_offset_y", 0.0f);
	LightWorldOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "torch_world_attach_offset_z", 0.0f);

	OmniOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "torch_omni_attach_offset_x", LightOffset.x);
	OmniOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "torch_omni_attach_offset_y", LightOffset.y);
	OmniOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "torch_omni_attach_offset_z", LightOffset.z);

	OmniWorldOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "torch_omni_world_attach_offset_x", LightWorldOffset.x);
	OmniWorldOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "torch_omni_world_attach_offset_y", LightWorldOffset.y);
	OmniWorldOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "torch_omni_world_attach_offset_z", LightWorldOffset.z);

	LightColor.r = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_color_r", 0.60f);
	LightColor.g = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_color_g", 0.55f);
	LightColor.b = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_color_b", 0.55f);
	LightColor.a = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_color_a", 0.80f);
	RenderLight->set_color(LightColor);

	OmniColor.r = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_omni_color_r", 0.60f);
	OmniColor.g = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_omni_color_g", 0.55f);
	OmniColor.b = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_omni_color_b", 0.55f);
	OmniColor.a = READ_IF_EXISTS(pSettings, r_float, section, "torch_r2_omni_color_a", 0.80f);
	OmniLight->set_color(OmniColor);

	RenderLight->set_cone(deg2rad(READ_IF_EXISTS(pSettings, r_float, section, "torch_spot_angle", 75.0f)));
	RenderLight->set_texture(pSettings->r_string(section, "torch_spot_texture"));

	IsLightDirByBone = !!READ_IF_EXISTS(pSettings, r_bool, section, "light_directions_by_bones", FALSE);
	if (IsLightDirByBone)
	{
		LightDirBoneName = pSettings->r_string(section, "light_dir_bone");
	}
}

void HudLightTorch::SwitchTorchlight(bool isActive) {
	if (IsTorchInstalled)
	{
		IsRenderLight = isActive;
	}
	else
	{
		IsRenderLight = false;
	}
}

void HudLightTorch::UpdateTorchFromObject(CHudItem* item) const {
	if (RenderLight == nullptr || OmniLight == nullptr || item == nullptr || item->object().Visual() == nullptr)
	{
		return;
	}

	if (item->GetState() == item->eHidden && item->object().H_Parent())
	{
		RenderLight->set_active(false);
		OmniLight->set_active(false);

		return;
	}

	bool isHudMode = item->GetHUDmode();
	if (IsRenderLight)
	{
		IKinematics* kin = nullptr;
		Fmatrix xform;
		Fvector lightPos = { 0, 0, 0 };
		Fvector lightDir = { 0, 0, 1 };
		Fvector omniPos = { 0, 0, 0 };
		u16 lightBoneId = BI_NONE;
		u16 lightDirBoneId = BI_NONE;
		Fvector up, right;

		if (isHudMode)
		{
			xform = item->HudItemData()->m_item_transform;
			kin = item->HudItemData()->m_model;
			lightBoneId = kin->LL_BoneID(LightBone);

			Fvector3 curr_light_offset = LightOffset;
			Fvector3 curr_omni_offset = OmniOffset;

			if (smart_cast<CCustomDetector*>(item) != nullptr)
			{
				CWeapon* wpn = smart_cast<CWeapon*>(Actor()->inventory().ActiveItem());
				if (wpn != nullptr && wpn->WpnCanShoot() && wpn->GetAimFactor() > 0.001f)
				{
					Fvector3 aim_offset = AimOffset;
					aim_offset.mul(wpn->GetAimFactor());
					curr_light_offset.add(aim_offset);
					curr_omni_offset.add(aim_offset);
				}
			}

			kin->LL_GetTransform(lightBoneId).transform_tiny(lightPos, curr_light_offset);
			kin->LL_GetTransform(lightBoneId).transform_tiny(omniPos, curr_omni_offset);

			if (IsLightDirByBone)
			{
				lightDirBoneId = kin->LL_BoneID(LightDirBoneName);
				kin->LL_GetTransform(lightDirBoneId).transform_tiny(lightDir, LightOffset);
				lightDir = lightDir.sub(lightPos).normalize();
			}
		}
		else
		{
			xform = item->object().XFORM();
			kin = item->object().Visual()->dcast_PKinematics();
			lightBoneId = kin->LL_BoneID(LightBone);
			kin->LL_GetTransform(lightBoneId).transform_tiny(lightPos, LightWorldOffset);
			kin->LL_GetTransform(lightBoneId).transform_tiny(omniPos, OmniWorldOffset);

			if (IsLightDirByBone)
			{
				lightDirBoneId = kin->LL_BoneID(LightDirBoneName);
				kin->LL_GetTransform(lightDirBoneId).transform_tiny(lightDir, LightWorldOffset);
				lightDir = lightDir.sub(lightPos).normalize();
			}
		}

		xform.transform_tiny(lightPos);
		xform.transform_tiny(omniPos);
		xform.transform_dir(lightDir);

		if (isHudMode)
		{
			TransformToHud(lightPos, lightDir);
			TransformToHudTemp(omniPos);
		}

		Fvector::generate_orthonormal_basis_normalized(lightDir, up, right);

		OmniLight->set_hud_mode(false);
		OmniLight->set_position(omniPos);

		RenderLight->set_hud_mode(false);
		RenderLight->set_position(lightPos);
		RenderLight->set_rotation(lightDir, right);

		RenderLight->set_ignore_object(item->object().H_Root());
	}

	RenderLight->set_active(IsRenderLight);
	OmniLight->set_active(false);
}
