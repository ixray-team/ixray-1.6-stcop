#include "StdAfx.h"
#include "HudTorchLight.h"
#include "player_hud.h"
#include "CustomDevice.h"
#include "Weapon.h"
#include "Actor.h"
#include "Inventory.h"
#include "../debug_renderer.h"
#include "ElectronicsProblemsManager.h"

void THudLightTorch::BeginComponent(IECSOwner* O)
{
	SetInstalled(true);
}

void THudLightTorch::EndComponent()
{
	if (RenderLight != nullptr)
	{
		RenderLight.destroy();
	}

	if (OmniLight != nullptr)
	{
		OmniLight.destroy();
	}
}

void THudLightTorch::NewTorchlight(const char* section)
{
	if (!IsTorchInstalled)
		return;

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

	if (pSettings->line_exist(section, "torch_cone_bones"))
	{
		ConeBones.clear();
		string128 bone_name = {};

		LPCSTR lineStr = pSettings->r_string(section, "torch_cone_bones");
		for (auto i = 0, cnt = _GetItemCount(lineStr); i < cnt; ++i)
		{
			ConeBones.push_back(_GetItem(lineStr, i, bone_name));
		}
	}

	Fvector tmp_vector = { -1.0f, -1.0f, 0.0f };

	tmp_vector = READ_IF_EXISTS(pSettings, r_fvector3, section, "torch_breaking_params", tmp_vector);
	BreakingParams.start_condition = tmp_vector.x;
	BreakingParams.end_condition = tmp_vector.y;
	BreakingParams.start_probability = tmp_vector.z;
}

void THudLightTorch::SwitchTorchlight(bool isActive)
{
	if (IsTorchInstalled)
	{
		IsRenderLight = isActive;
	}
	else
	{
		IsRenderLight = false;
	}
}

bool forceTPDraw = false;
void THudLightTorch::UpdateTorchFromObject(CHudItem* item) const
{
	if (RenderLight == nullptr || OmniLight == nullptr || item == nullptr || item->object().Visual() == nullptr)
	{
		return;
	}

	if (item->object().H_Parent())
	{
		if (item->GetState() == item->eHidden || item->cast_inventory_item()->CurrPlace() == eItemPlace::eItemPlaceRuck)
		{
			RenderLight->set_active(false);
			OmniLight->set_active(false);

			return;
		}
	}

	bool isHudMode = item->GetHUDmode() && item->HudItemData();
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

			if (item->cast_custom_device() != nullptr)
			{
				PIItem active_item = Actor()->inventory().ActiveItem();
				CWeapon* wpn = active_item != nullptr ? active_item->cast_weapon() : nullptr;
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
			Device.transform_hud2world(lightPos, lightDir);
			Device.transform_hud2world(omniPos);
		}

		Fvector::generate_orthonormal_basis_normalized(lightDir, up, right);

		OmniLight->set_hud_mode(false);
		OmniLight->set_position(omniPos);

		RenderLight->set_hud_mode(false);
		RenderLight->set_position(lightPos);
		RenderLight->set_rotation(lightDir, right);

		RenderLight->set_ignore_object(item->object().H_Root());
		if (isHudMode)
		{
			Fobb obb;
			Fmatrix trans = item->HudItemData()->m_item_transform;
			obb.m_rotate.i.set(trans.i);
			obb.m_rotate.j.set(trans.j);
			obb.m_rotate.k.set(trans.k);
			obb.m_halfsize.set(0.005f, 0.005f, 0.005f);

#ifdef DEBUG_DRAW
			if (forceTPDraw)
			{
				Fvector posDebug = lightPos;
				obb.m_translate.set(posDebug);
				obb.xform_full(trans);
				Level().debug_renderer().draw_obb(trans, color_xrgb(255, 0, 0));
			}
#endif
		}
	}

	RenderLight->set_active(IsRenderLight);
	OmniLight->set_active(false);
}

void THudLightTorch::SwitchTorch(bool& saved_status, bool status, bool forced)
{
	if (!forced && status == saved_status)
	{
		return;
	}

	saved_status = status;

	SwitchTorchlight(status);
}

void THudLightTorch::UpdateTorch(CHudItemObject* item, bool& saved_status)
{
	SwitchTorch(saved_status, saved_status, true);

	bool is_broken = false;
	const float current_condition = item->GetCondition();

	const THudLightTorch::breaking_params& BreakingParams = this->BreakingParams;

	if (current_condition < BreakingParams.end_condition)
	{
		is_broken = true;
	}
	else if (current_condition < BreakingParams.start_condition)
	{
		is_broken = (::Random.randF(0.0f, 1.0f) < BreakingParams.start_probability +
			(BreakingParams.start_condition - current_condition) *
			(1.0f - BreakingParams.start_probability) /
			(BreakingParams.start_condition - BreakingParams.end_condition));
	}

	if (is_broken)
	{
		SwitchTorchlight(false);
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, BOOL status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, FALSE);
			}
		}
	};

	attachable_hud_item* HID = item->HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = item->Visual() != nullptr ? PKinematics(item->Visual()) : nullptr;

	for (const shared_str& bone : ConeBones)
	{
		SetVisible(hud_kin, bone, GetTorchActive());
		SetVisible(world_kin, bone, GetTorchActive());
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void THudLightLaser::BeginComponent(IECSOwner* O)
{
	THudLightTorch::BeginComponent(O);
	IsLightDirByBone = false;
}

void THudLightLaser::NewTorchlight(const char* section)
{
	if (RenderLight)
	{
		RenderLight.destroy();
	}

	if (OmniLight)
	{
		OmniLight.destroy();
	}

	if (!pSettings->line_exist(section, "laser_installed"))
	{
		return;
	}

	Section = section;
	LightBone = pSettings->r_string(section, "laserdot_attach_bone");

	LaserLightDist = READ_IF_EXISTS(pSettings, r_float, section, "laser_light_distance", 15.0f);

	LaserWorkDist = READ_IF_EXISTS(pSettings, r_float, section, "laser_wrok_distance", LaserLightDist * 0.5f);
	LaserMaxDist = READ_IF_EXISTS(pSettings, r_float, section, "laser_max_distance", LaserWorkDist);

	LightColor = READ_IF_EXISTS(pSettings, r_fcolor, section, "laser_light_color", LightColor.set(1, 0, 0, 0));

	RenderLight = ::Render->light_create();

	RenderLight->set_color(LightColor);
	RenderLight->set_range(LaserLightDist);
	RenderLight->set_type(IRender_Light::SPOT);

	RenderLight->set_shadow(!!READ_IF_EXISTS(pSettings, r_bool, section, "laser_render_shadow", TRUE));

	LightSpotAngle = READ_IF_EXISTS(pSettings, r_fvector2, section, "laser_spot_angle", LightSpotAngle.set(2, 5));
	LightSpotAngle.mul(M_PI / 180);

	RenderLight->set_cone(LightSpotAngle.x);
	RenderLight->set_texture(pSettings->r_string(section, "laser_spot_texture"));

	LightOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "laserdot_attach_offset_x", 0.0f);
	LightOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "laserdot_attach_offset_y", 0.0f);
	LightOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "laserdot_attach_offset_z", 0.0f);

	LightWorldOffset.x = READ_IF_EXISTS(pSettings, r_float, section, "laserdot_world_attach_offset_x", 0.0f);
	LightWorldOffset.y = READ_IF_EXISTS(pSettings, r_float, section, "laserdot_world_attach_offset_y", 0.0f);
	LightWorldOffset.z = READ_IF_EXISTS(pSettings, r_float, section, "laserdot_world_attach_offset_z", 0.0f);

	LightWorldOffset = READ_IF_EXISTS(pSettings, r_fvector3, section, "laserdot_world_attach_offset", LightWorldOffset);
	LightOffset = READ_IF_EXISTS(pSettings, r_fvector3, section, "laserdot_attach_offset", LightOffset);

	if (pSettings->line_exist(section, "laser_ray_bones"))
	{
		ConeBones.clear();
		string128 bone_name = {};

		LPCSTR lineStr = pSettings->r_string(section, "laser_ray_bones");
		for (auto i = 0, cnt = _GetItemCount(lineStr); i < cnt; ++i)
		{
			ConeBones.push_back(_GetItem(lineStr, i, bone_name));
		}
	}

	Fvector tmp_vector = { -1.0f, -1.0f, 0.0f };

	tmp_vector = READ_IF_EXISTS(pSettings, r_fvector3, section, "laser_breaking_params", tmp_vector);
	BreakingParams.start_condition = tmp_vector.x;
	BreakingParams.end_condition = tmp_vector.y;
	BreakingParams.start_probability = tmp_vector.z;
	BreakingParams.levels_problem = READ_IF_EXISTS(pSettings, r_float, section, "laser_problems_level", 0.0f);
}

bool forceLPDraw = false;
void THudLightLaser::UpdateTorchFromObject(CHudItem* item) const
{
	if (RenderLight == nullptr || item == nullptr || item->object().Visual() == nullptr)
	{
		return;
	}

	if (item->object().H_Parent())
	{
		if (item->GetState() == item->eHidden || item->cast_inventory_item()->CurrPlace() == eItemPlace::eItemPlaceRuck)
		{
			RenderLight->set_active(false);
			return;
		}
	}

	if (IsRenderLight)
	{
		IKinematics* kin = nullptr;
		Fmatrix xform;

		Fvector lightPos = { 0, 0, 0 };
		Fvector lightDir = { 0, 0, 1 };

		u16 lightBoneId = BI_NONE;
		u16 lightDirBoneId = BI_NONE;

		Fvector up, right;
		bool isHudMode = item->GetHUDmode() && item->HudItemData() != nullptr;

		if (isHudMode)
		{
			xform = item->HudItemData()->m_item_transform;
			kin = item->HudItemData()->m_model;

			Fvector curr_light_offset = LightOffset;

			if (CWeapon* wpn = item->cast_weapon())
			{
				if (wpn->WpnCanShoot() && wpn->GetAimFactor() > 0.001f)
				{
					Fvector aim_offset = Device.vCameraPosition;
					_lerp(curr_light_offset, aim_offset, wpn->GetAimFactor());
				}
			}

			lightBoneId = kin->LL_BoneID(LightBone);
			kin->LL_GetTransform(lightBoneId).transform_tiny(lightPos, curr_light_offset);
		}
		else
		{
			xform = item->object().XFORM();
			kin = item->object().Visual()->dcast_PKinematics();

			lightBoneId = kin->LL_BoneID(LightBone);
			kin->LL_GetTransform(lightBoneId).transform_tiny(lightPos, LightWorldOffset);
		}

		xform.transform_tiny(lightPos);
		xform.transform_dir(lightDir);

		if (isHudMode)
		{
			Device.transform_hud2world(lightPos, lightDir);
		}

		Fvector::generate_orthonormal_basis_normalized(lightDir, up, right);;

		collide::rq_result	RQ; RQ.range = LaserMaxDist;
		collide::rq_target	RT = collide::rqtBoth;

		if (!g_pGameLevel->ObjectSpace.RayPick(lightPos, lightDir, RQ.range, RT, RQ, item->object().H_Root()))
		{
			RQ.range = LaserMaxDist;
		}
		
		float CurrentLightSpotAngle = LightSpotAngle.x;

		if (RQ.range > LaserWorkDist)
		{
			float ShiftDistance = RQ.range - LaserWorkDist;
			lightPos.mad(lightDir, ShiftDistance);

			if (isHudMode)
			{
				CurrentLightSpotAngle += (LightSpotAngle.y - LightSpotAngle.x) * ShiftDistance / (LaserMaxDist - LaserWorkDist);
			}
		}

		RenderLight->set_position(lightPos);
		RenderLight->set_rotation(lightDir, right);
		RenderLight->set_cone(CurrentLightSpotAngle);

		RenderLight->set_ignore_object(item->object().H_Root());
		if (isHudMode)
		{
			Fobb obb;
			Fmatrix trans = item->HudItemData()->m_item_transform;
			obb.m_rotate.i.set(trans.i);
			obb.m_rotate.j.set(trans.j);
			obb.m_rotate.k.set(trans.k);
			obb.m_halfsize.set(0.005f, 0.005f, 0.005f);

#ifdef DEBUG_DRAW
			if (forceLPDraw)
			{
				Fvector posDebug = lightPos;
				obb.m_translate.set(posDebug);
				obb.xform_full(trans);
				Level().debug_renderer().draw_obb(trans, color_xrgb(255, 0, 0));
			}
#endif
		}
	}

	RenderLight->set_active(IsRenderLight);
}

void THudLightLaser::SwitchLaser(bool& saved_status, bool status, bool forced)
{
	if (!forced && status == saved_status)
	{
		return;
	}

	saved_status = status;

	SwitchTorchlight(status);
}

void THudLightLaser::UpdateLaser(CHudItemObject* item, bool& saved_status)
{
	SwitchLaser(saved_status, saved_status, true);

	bool is_broken = false;
	const float current_condition = item->GetCondition();
	const int current_problems_cnt = Level().GetElectronicsProblemsManager()->CurrentElectronicsProblemsCnt();
	const int target_problems_cnt = Level().GetElectronicsProblemsManager()->TargetElectronicsProblemsCnt();

	const THudLightLaser::breaking_params& BreakingParams = this->BreakingParams;

	if (current_condition < BreakingParams.end_condition)
	{
		is_broken = true;
	}
	else if (current_condition < BreakingParams.start_condition || BreakingParams.levels_problem > 0.0f && current_problems_cnt >= BreakingParams.levels_problem)
	{
		float probability = 0.0f;

		if (target_problems_cnt >= BreakingParams.levels_problem)
		{
			probability = 1.0f;
		}
		else if (BreakingParams.start_condition == BreakingParams.end_condition)
		{
			probability = BreakingParams.start_condition;
		}
		else
		{
			probability = BreakingParams.start_probability + (BreakingParams.start_condition - current_condition) * (1.0f - BreakingParams.start_probability) / (BreakingParams.start_condition - BreakingParams.end_condition);
		}

		is_broken = !!(::Random.randF(0.0f, 1.0f) < probability);
	}

	if (is_broken)
	{
		SwitchTorchlight(false);
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, BOOL status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, FALSE);
			}
		}
	};

	attachable_hud_item* HID = item->HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = item->Visual() != nullptr ? PKinematics(item->Visual()) : nullptr;

	for (const shared_str& bone : ConeBones)
	{
		SetVisible(hud_kin, bone, GetTorchActive());
		SetVisible(world_kin, bone, GetTorchActive());
	}

	UpdateTorchFromObject(item);
}