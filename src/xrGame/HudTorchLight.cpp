#include "StdAfx.h"
#include "HudTorchLight.h"
#include "player_hud.h"
#include "CustomDevice.h"
#include "Weapon.h"
#include "Actor.h"
#include "Inventory.h"
#include "../debug_renderer.h"
#include "ElectronicsProblemsManager.h"
#include "../xrEngine/CameraBase.h"

static constexpr float LIGHT_INERTION_SPEED_MAX = 7.5f;
static constexpr float LIGHT_INERTION_SPEED_MIN = 0.5f;

void THudLightTorch::BeginComponent(IECSOwner* O)
{
	SetInstalled(true);

	Owner = smart_cast<PIItem>(O);
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
	RenderLight->set_type((IRender_Light::LT)pSettings->read_if_exists<u32>(section, "torch_render_type", IRender_Light::SPOT));
	const float LegacyRange = pSettings->read_if_exists<float>(section, "torch_r2_range", 15.0f);
	RenderLight->set_range(LegacyRange);
	IsRenderShadow = !!pSettings->read_if_exists<bool>(section, "torch_render_shadow", true);
	RenderLight->set_shadow(IsRenderShadow);

	OmniLight = ::Render->light_create();
	OmniLight->set_type(IRender_Light::POINT); // (IRender_Light::LT)pSettings->read_if_exists<u32>(section, "torch_omni_type", IRender_Light::POINT));
	OmniLight->set_range(pSettings->read_if_exists<float>(section, "torch_r2_omni_range", 0.75f));
	OmniLight->set_shadow(!!pSettings->read_if_exists<bool>(section, "torch_omni_shadow", false));

	LightBone = pSettings->r_string(section, "torch_light_bone");

	LightOffset.x = pSettings->read_if_exists<float>(section, "torch_attach_offset_x", 0.0f);
	LightOffset.y = pSettings->read_if_exists<float>(section, "torch_attach_offset_y", 0.0f);
	LightOffset.z = pSettings->read_if_exists<float>(section, "torch_attach_offset_z", 0.0f);

	AimOffset.x = pSettings->read_if_exists<float>(section, "torch_aim_attach_offset_x", 0.0f);
	AimOffset.y = pSettings->read_if_exists<float>(section, "torch_aim_attach_offset_y", 0.0f);
	AimOffset.z = pSettings->read_if_exists<float>(section, "torch_aim_attach_offset_z", 0.0f);

	LightWorldOffset.x = pSettings->read_if_exists<float>(section, "torch_world_attach_offset_x", 0.0f);
	LightWorldOffset.y = pSettings->read_if_exists<float>(section, "torch_world_attach_offset_y", 0.0f);
	LightWorldOffset.z = pSettings->read_if_exists<float>(section, "torch_world_attach_offset_z", 0.0f);

	OmniOffset.x = pSettings->read_if_exists<float>(section, "torch_omni_attach_offset_x", LightOffset.x);
	OmniOffset.y = pSettings->read_if_exists<float>(section, "torch_omni_attach_offset_y", LightOffset.y);
	OmniOffset.z = pSettings->read_if_exists<float>(section, "torch_omni_attach_offset_z", LightOffset.z);

	OmniWorldOffset.x = pSettings->read_if_exists<float>(section, "torch_omni_world_attach_offset_x", LightWorldOffset.x);
	OmniWorldOffset.y = pSettings->read_if_exists<float>(section, "torch_omni_world_attach_offset_y", LightWorldOffset.y);
	OmniWorldOffset.z = pSettings->read_if_exists<float>(section, "torch_omni_world_attach_offset_z", LightWorldOffset.z);

	LightColor.r = pSettings->read_if_exists<float>(section, "torch_r2_color_r", 0.60f);
	LightColor.g = pSettings->read_if_exists<float>(section, "torch_r2_color_g", 0.55f);
	LightColor.b = pSettings->read_if_exists<float>(section, "torch_r2_color_b", 0.55f);
	LightColor.a = pSettings->read_if_exists<float>(section, "torch_r2_color_a", 0.80f);
	RenderLight->set_color(LightColor);

	OmniColor.r = pSettings->read_if_exists<float>(section, "torch_r2_omni_color_r", 0.60f);
	OmniColor.g = pSettings->read_if_exists<float>(section, "torch_r2_omni_color_g", 0.55f);
	OmniColor.b = pSettings->read_if_exists<float>(section, "torch_r2_omni_color_b", 0.55f);
	OmniColor.a = pSettings->read_if_exists<float>(section, "torch_r2_omni_color_a", 0.80f);
	OmniLight->set_color(OmniColor);

	const float LegacyCone = deg2rad(pSettings->read_if_exists<float>(section, "torch_spot_angle", 75.0f));
	RenderLight->set_cone(LegacyCone);
	RenderLight->set_texture(pSettings->r_string(section, "torch_spot_texture"));

	IsLightDirByBone = !!pSettings->read_if_exists<bool>(section, "light_directions_by_bones", false);
	if (IsLightDirByBone)
	{
		LightDirBoneName = pSettings->r_string(section, "light_dir_bone");
	}

	if (pSettings->line_exist(section, "torch_cone_bones"))
	{
		ConeBones.clear();
		string128 bone_name = {};

		const char* lineStr = pSettings->r_string(section, "torch_cone_bones");
		for (auto i = 0, cnt = _GetItemCount(lineStr); i < cnt; ++i)
		{
			ConeBones.push_back(_GetItem(lineStr, i, bone_name));
		}
	}

	Fvector tmp_vector = { -1.0f, -1.0f, 0.0f };

	tmp_vector = pSettings->read_if_exists<Fvector>(section, "torch_breaking_params", tmp_vector);
	BreakingParams.start_condition = tmp_vector.x;
	BreakingParams.end_condition = tmp_vector.y;
	BreakingParams.start_probability = tmp_vector.z;

	Beam.Load(section);
	Beam.SetLegacy(LightColor, LegacyRange, LegacyCone);
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

static bool IsLightRootActive(PIItem Root)
{
	if (CHudItem* HudItem = Root->cast_hud_item())
	{
		return HudItem->GetState() != CHUDState::eHidden && Root->CurrPlace() != eItemPlace::eItemPlaceRuck;
	}

	CInventoryOwner* Owner = Root->object().H_Parent()->cast_inventory_owner();
	return Owner && Owner->attached(Root);
}

static CActor* WornInFirstPerson(PIItem Root)
{
	if (Root->cast_hud_item() || !Root->object().H_Parent())
	{
		return nullptr;
	}

	CActor* pActor = Root->object().H_Parent()->cast_actor();
	return pActor && pActor == Level().CurrentViewEntity() && pActor->HUDview() ? pActor : nullptr;
}

bool THudLightTorch::GetOwnerTransform(IKinematics*& kin, Fmatrix& xform) const
{
	PIItem Root = Owner->AttachRoot();
	CHudItem* HudItem = Root->cast_hud_item();
	const bool hud = HudItem && HudItem->GetHUDmode() && HudItem->HudItemData();

	if (!hud)
	{
		if (CObject* Parent = Root->object().H_Parent())
		{
			PKinematics(Parent->Visual())->CalculateBones(true);
		}
	}

	if (!Owner->AttachTransform(hud, kin, xform))
	{
		kin = nullptr;
	}

	return hud;
}

bool forceTPDraw = false;
void THudLightTorch::UpdateTorchFromObject()
{
	IKinematics* kin = nullptr;
	Fmatrix xform;
	const bool isHudMode = GetOwnerTransform(kin, xform);

	if (RenderLight == nullptr || OmniLight == nullptr || kin == nullptr)
	{
		return;
	}

	PIItem Root = Owner->AttachRoot();
	if (Root->object().H_Parent())
	{
		if (!IsLightRootActive(Root))
		{
			RenderLight->set_active(false);
			OmniLight->set_active(false);
			return;
		}
	}

	if (IsRenderLight)
	{
		Fvector lightPos = zero_vel;
		Fvector lightDir = { 0.0f, 0.0f, 1.0f };
		Fvector omniPos = zero_vel;
		u16 lightBoneId = BI_NONE;
		u16 lightDirBoneId = BI_NONE;
		Fvector up = zero_vel, right = zero_vel;

		if (isHudMode)
		{
			lightBoneId = kin->LL_BoneID(LightBone);

			Fvector curr_light_offset = LightOffset;
			Fvector curr_omni_offset = OmniOffset;

			if (Owner->cast_custom_device() != nullptr)
			{
				PIItem active_item = Actor()->inventory().ActiveItem();
				CWeapon* wpn = active_item != nullptr ? active_item->cast_weapon() : nullptr;
				if (wpn != nullptr && wpn->WpnCanShoot() && wpn->GetAimFactor() > 0.001f)
				{
					Fvector aim_offset = AimOffset;
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
			lightPos = g_player_hud->ToWorld(lightPos);
			omniPos = g_player_hud->ToWorld(omniPos);
			Device.transform_hud2world(lightPos, lightDir);
			Device.transform_hud2world(omniPos);
		}

		RenderLight->set_shadow(IsRenderShadow);

		if (CActor* pActor = WornInFirstPerson(Root))
		{
			if (!RenderLight->get_active())
			{
				PrevHP.set(-pActor->cam_Active()->yaw, -pActor->cam_Active()->pitch);
				Beam.Reset();
			}

			if (Beam.IsSwayEnabled())
			{
				PrevHP = Beam.UpdateDirection(pActor, Device.fTimeDelta);
			}
			else
			{
				PrevHP.x = angle_inertion_var(PrevHP.x, -pActor->cam_Active()->yaw, LIGHT_INERTION_SPEED_MIN, LIGHT_INERTION_SPEED_MAX, PI_DIV_6, Device.fTimeDelta);
				PrevHP.y = angle_inertion_var(PrevHP.y, -pActor->cam_Active()->pitch, LIGHT_INERTION_SPEED_MIN, LIGHT_INERTION_SPEED_MAX, PI_DIV_6, Device.fTimeDelta);
			}

			lightPos.set(Beam.IsEyeAnchored() ? Beam.GetEyePosition() : Device.vCameraPosition);
			RenderLight->set_shadow(Beam.IsEyeAnchored() ? Beam.HasEyeShadow() : IsRenderShadow);
			omniPos.set(Device.vCameraPosition);
			lightDir.setHP(PrevHP.x, PrevHP.y);
		}

		Fvector::generate_orthonormal_basis_normalized(lightDir, up, right);

		OmniLight->set_hud_mode(false);
		OmniLight->set_position(omniPos);

		RenderLight->set_hud_mode(false);
		RenderLight->set_position(lightPos);
		RenderLight->set_rotation(lightDir, right);

		RenderLight->set_ignore_object(Owner->object().H_Root());

		if (isHudMode)
		{
			Fobb obb;
			Fmatrix trans = xform;
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

	if (IsRenderLight)
	{
		Beam.UpdateLight(RenderLight._get(), Owner->object().H_Root()->Position(), nullptr, Device.fTimeDelta);
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

void THudLightTorch::UpdateTorch(PIItem item, bool& saved_status)
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

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, bool status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, false);
			}
		}
	};

	CHudItem* HudItem = item->cast_hud_item();
	attachable_hud_item* HID = HudItem != nullptr ? HudItem->HudItemData() : nullptr;
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : item->m_attach_parent != nullptr ? item->HudModel() : nullptr;
	IKinematics* world_kin = item->object().Visual() != nullptr ? PKinematics(item->object().Visual()) : nullptr;

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

	LaserLightDist = pSettings->read_if_exists<float>(section, "laser_light_distance", 15.0f);

	LaserWorkDist = pSettings->read_if_exists<float>(section, "laser_work_distance", LaserLightDist * 0.5f);
	LaserMaxDist = pSettings->read_if_exists<float>(section, "laser_max_distance", LaserWorkDist);

	LightColor = pSettings->read_if_exists<Fcolor>(section, "laser_light_color", LightColor.set(1.0f, 0.0f, 0.0f, 0.0f));

	RenderLight = ::Render->light_create();

	RenderLight->set_color(LightColor);
	RenderLight->set_range(LaserLightDist);
	RenderLight->set_type(IRender_Light::SPOT);

	RenderLight->set_shadow(!!pSettings->read_if_exists<bool>(section, "laser_render_shadow", true));

	LightSpotAngle = pSettings->read_if_exists<Fvector2>(section, "laser_spot_angle", LightSpotAngle.set(2, 5));
	LightSpotAngle.mul(M_PI / 180);

	RenderLight->set_cone(LightSpotAngle.x);
	RenderLight->set_texture(pSettings->r_string(section, "laser_spot_texture"));

	LightOffset.x = pSettings->read_if_exists<float>(section, "laserdot_attach_offset_x", 0.0f);
	LightOffset.y = pSettings->read_if_exists<float>(section, "laserdot_attach_offset_y", 0.0f);
	LightOffset.z = pSettings->read_if_exists<float>(section, "laserdot_attach_offset_z", 0.0f);

	LightWorldOffset.x = pSettings->read_if_exists<float>(section, "laserdot_world_attach_offset_x", 0.0f);
	LightWorldOffset.y = pSettings->read_if_exists<float>(section, "laserdot_world_attach_offset_y", 0.0f);
	LightWorldOffset.z = pSettings->read_if_exists<float>(section, "laserdot_world_attach_offset_z", 0.0f);

	LightWorldOffset = pSettings->read_if_exists<Fvector>(section, "laserdot_world_attach_offset", LightWorldOffset);
	LightOffset = pSettings->read_if_exists<Fvector>(section, "laserdot_attach_offset", LightOffset);

	if (pSettings->line_exist(section, "laser_ray_bones"))
	{
		ConeBones.clear();
		string128 bone_name = {};

		const char* lineStr = pSettings->r_string(section, "laser_ray_bones");
		for (auto i = 0, cnt = _GetItemCount(lineStr); i < cnt; ++i)
		{
			ConeBones.push_back(_GetItem(lineStr, i, bone_name));
		}
	}

	Fvector tmp_vector = { -1.0f, -1.0f, 0.0f };

	tmp_vector = pSettings->read_if_exists<Fvector>(section, "laser_breaking_params", tmp_vector);
	BreakingParams.start_condition = tmp_vector.x;
	BreakingParams.end_condition = tmp_vector.y;
	BreakingParams.start_probability = tmp_vector.z;
	BreakingParams.levels_problem = pSettings->read_if_exists<float>(section, "laser_problems_level", 0.0f);
}

bool forceLPDraw = false;
void THudLightLaser::UpdateTorchFromObject()
{
	IKinematics* kin = nullptr;
	Fmatrix xform;
	const bool isHudMode = GetOwnerTransform(kin, xform);

	if (RenderLight == nullptr || kin == nullptr)
	{
		return;
	}

	PIItem Root = Owner->AttachRoot();
	if (Root->object().H_Parent())
	{
		if (!IsLightRootActive(Root))
		{
			RenderLight->set_active(false);
			return;
		}
	}

	if (IsRenderLight)
	{
		Fvector lightPos = { 0, 0, 0 };
		Fvector lightDir = { 0, 0, 1 };
		Fvector up, right;
		const u16 lightBoneId = kin->LL_BoneID(LightBone);

		if (isHudMode)
		{
			Fvector curr_light_offset = LightOffset;

			if (CWeapon* wpn = Root->cast_weapon())
			{
				if (wpn->WpnCanShoot() && wpn->GetAimFactor() > 0.001f)
				{
					Fvector aim_offset = Device.vCameraPosition;
					_lerp(curr_light_offset, aim_offset, wpn->GetAimFactor());
				}
			}

			kin->LL_GetTransform(lightBoneId).transform_tiny(lightPos, curr_light_offset);
		}
		else
		{
			kin->LL_GetTransform(lightBoneId).transform_tiny(lightPos, LightWorldOffset);
		}

		xform.transform_tiny(lightPos);
		xform.transform_dir(lightDir);

		if (isHudMode)
		{
			lightPos = g_player_hud->ToWorld(lightPos);
			Device.transform_hud2world(lightPos, lightDir);
		}

		if (CActor* pActor = WornInFirstPerson(Root))
		{
			if (!RenderLight->get_active())
			{
				PrevHP.set(-pActor->cam_Active()->yaw, -pActor->cam_Active()->pitch);
			}
			PrevHP.x = angle_inertion_var(PrevHP.x, -pActor->cam_Active()->yaw, LIGHT_INERTION_SPEED_MIN, LIGHT_INERTION_SPEED_MAX, PI_DIV_6, Device.fTimeDelta);
			PrevHP.y = angle_inertion_var(PrevHP.y, -pActor->cam_Active()->pitch, LIGHT_INERTION_SPEED_MIN, LIGHT_INERTION_SPEED_MAX, PI_DIV_6, Device.fTimeDelta);

			lightPos.set(Device.vCameraPosition);
			lightDir.setHP(PrevHP.x, PrevHP.y);
		}

		Fvector::generate_orthonormal_basis_normalized(lightDir, up, right);

		collide::rq_result	RQ; RQ.range = LaserMaxDist;
		collide::rq_target	RT = collide::rqtBoth;

		if (!g_pGameLevel->ObjectSpace.RayPick(lightPos, lightDir, RQ.range, RT, RQ, Owner->object().H_Root()))
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

		RenderLight->set_ignore_object(Owner->object().H_Root());

#ifdef DEBUG_DRAW
		if (isHudMode && forceLPDraw)
		{
			Fobb obb;
			Fmatrix trans = xform;
			obb.m_rotate.i.set(trans.i);
			obb.m_rotate.j.set(trans.j);
			obb.m_rotate.k.set(trans.k);
			obb.m_halfsize.set(0.005f, 0.005f, 0.005f);
			obb.m_translate.set(lightPos);
			obb.xform_full(trans);
			Level().debug_renderer().draw_obb(trans, color_xrgb(255, 0, 0));
		}
#endif
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

void THudLightLaser::UpdateLaser(PIItem item, bool& saved_status)
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

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, bool status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, false);
			}
		}
	};

	CHudItem* HudItem = item->cast_hud_item();
	attachable_hud_item* HID = HudItem != nullptr ? HudItem->HudItemData() : nullptr;
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : item->m_attach_parent != nullptr ? item->HudModel() : nullptr;
	IKinematics* world_kin = item->object().Visual() != nullptr ? PKinematics(item->object().Visual()) : nullptr;

	for (const shared_str& bone : ConeBones)
	{
		SetVisible(hud_kin, bone, GetTorchActive());
		SetVisible(world_kin, bone, GetTorchActive());
	}
}