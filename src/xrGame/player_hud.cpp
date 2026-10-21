#include "StdAfx.h"
#include "player_hud.h"
#include "WatchDevice/WatchDevice.h"
#include "HudItem.h"
#include "Actor.h"
#include "physic_item.h"
#include "ActorEffector.h"
#include "../xrEngine/IGame_Persistent.h"
#include "InertionData.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "Inventory.h"
#include "WeaponBinoculars.h"
#include "WeaponPistol.h"
#include "../xrEngine/GameMtlLib.h"

player_hud* g_player_hud = nullptr;
player_hud* g_player_hud2 = nullptr;

namespace HudRotation
{
	Fquaternion FromEuler(const Fvector& degrees)
	{
		Fmatrix rotation;
		rotation.setHPB(deg2rad(degrees.x), deg2rad(degrees.y), deg2rad(degrees.z));
		return Fquaternion().set(rotation);
	}

	Fvector ToEuler(const Fquaternion& rotation)
	{
		Fvector hpb;
		Fmatrix().rotation(rotation).getHPB(hpb);
		return hpb.mul(180.f / PI);
	}

	Fvector Delta(const Fquaternion& from, const Fquaternion& to)
	{
		Fquaternion delta;
		delta.mul(to, Fquaternion().inverse(from));
		if (delta.w < 0.f)
		{
			delta.set(-delta.w, -delta.x, -delta.y, -delta.z);
		}

		Fvector axis;
		float angle;
		if (!delta.get_axis_angle(axis, angle))
		{
			return zero_vel;
		}

		return axis.mul(angle * 180.f / PI);
	}

	Fquaternion Apply(const Fvector& degrees, const Fquaternion& rotation)
	{
		const float angle = degrees.magnitude();
		if (angle <= EPS_S)
		{
			return rotation;
		}

		Fvector axis = Fvector().div(degrees, angle);
		Fquaternion step;
		step.rotation(axis, deg2rad(angle));
		return Fquaternion().mul(step, rotation);
	}
}
Fvector _ancor_pos;
Fvector _wpn_root_pos;
extern float AnimatorHideSpeedFactor;

static float hud_smooth(float t)
{
	return t * t * (3.f - 2.f * t);
}

float CalcMotionSpeed(const shared_str& anim_name)
{
	if (!IsGameTypeSingle() && (anim_name == "anm_show" || anim_name == "anm_hide"))
	{
		return 2.0f;
	}
	else if (CActor* Actor = (Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr); Actor && Actor->HudAnimator()->TargetAnimator())
	{
		return AnimatorHideSpeedFactor;
	}
	else
	{
		return 1.0f;
	}
}

player_hud_motion* player_hud_motion_container::find_motion(const shared_str& name)
{
	const auto pose = PoseOverrides.find(name);
	const shared_str& alias = pose != PoseOverrides.end() ? pose->second : name;

	xr_vector<player_hud_motion>::iterator it = m_anims.begin();
	xr_vector<player_hud_motion>::iterator it_e = m_anims.end();
	for (; it != it_e; ++it)
	{
		const shared_str& s = (true) ? (*it).m_alias_name : (*it).m_base_name;
		if (s == alias)
		{
			return &(*it);
		}
	}

	return nullptr;
}

attachable_hud_item_motion* player_hud_motion_container::find_item_motion(const shared_str& name)
{
	xr_vector<attachable_hud_item_motion>::iterator it = m_item_anims.begin();
	xr_vector<attachable_hud_item_motion>::iterator it_e = m_item_anims.end();
	for (; it != it_e; ++it)
	{
		const shared_str& s = (true) ? (*it).m_alias_name : (*it).m_name;
		if (s == name)
		{
			return &(*it);
		}
	}

	return nullptr;
}

bool player_hud_motion_container::has_motion(const shared_str& name)
{
	return m_names[name];
}

void player_hud_motion_container::load(IKinematicsAnimated* model, const shared_str& sect, IKinematicsAnimated* item_model)
{
	CInifile::Sect& _sect = pSettings->r_section(sect);

	for (const auto& data : _sect.Data)
	{
		xr_string res = data.first.c_str();

		if (strstr(res.c_str(), "anim_") == res.c_str() &&
			res.find_first_of("anim_") != xr_string::npos)
		{
			if (model == g_player_hud->GetModel())
			{
				continue;
			}
			xr_string str = res.substr(5);
			res = "anm_" + str;
		};

		if (strstr(data.first.c_str(), "anm_bp_") == data.first.c_str())
		{
			load_bonepart_motions(item_model, data, sect);
		}
		else if (strstr(res.c_str(), "anm_") == res.c_str())
		{
			// St4lker0k765: little hack for SoC animations
			CInifile::Item itmTemp;
			itmTemp.first = res.c_str();
			itmTemp.second = data.second;

			load_default_motions(model, itmTemp, sect);
		}
	}

#ifndef MASTER_GOLD
	static bool NeedReg = true;

	if (!NeedReg)
		return;

	NeedReg = false;

	if (!Device.IsEditorMode())
	{

		CImGuiManager::Instance().Subscribe
		(
			"HudAdjust",
			CImGuiManager::ERenderPriority::eMedium, []
			{
				if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::HudAdjust)]) {
					return;
				}

				extern u32 hud_adj_mode;
				extern bool hud_adj_crosshair;
				static bool EnableAdjust = false;

				ImGui::Begin("HudAdjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::HudAdjust)]);
				ImGui::Checkbox("Enable", &EnableAdjust);

				if (EnableAdjust)
				{
					float padding = 10.0f;
					float buttonHeight = 35.0f;

					float widthHudPosition = ImGui::CalcTextSize("Hud Position").x + 20.0f;
					float widthHudRotation = ImGui::CalcTextSize("Hud Rotation").x + 20.0f;
					float widthItemPosition = ImGui::CalcTextSize("Item Position").x + 20.0f;
					float widthItemRotation = ImGui::CalcTextSize("Item Rotation").x + 20.0f;
					float widthFirePoint = ImGui::CalcTextSize("Fire Point").x + 20.0f;
					float widthFire2Point = ImGui::CalcTextSize("Fire 2 Point").x + 20.0f;
					float widthShellPoint = ImGui::CalcTextSize("Shell Point").x + 20.0f;
					float widthPosStep = ImGui::CalcTextSize("pos STEP").x + 20.0f;
					float widthRotStep = ImGui::CalcTextSize("rot STEP").x + 20.0f;
					float widthCrosshair = ImGui::CalcTextSize("Crosshair").x + 20.0f;

					if (ImGui::Button("Hud Position", { widthHudPosition, buttonHeight }))
						hud_adj_mode = 1;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("Hud Rotation", { widthHudRotation, buttonHeight }))
						hud_adj_mode = 2;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("Item Position", { widthItemPosition, buttonHeight }))
						hud_adj_mode = 3;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("Item Rotation", { widthItemRotation, buttonHeight }))
						hud_adj_mode = 4;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("Fire Point", { widthFirePoint, buttonHeight }))
						hud_adj_mode = 5;

					ImGui::NewLine();

					if (ImGui::Button("Fire 2 Point", { widthFire2Point, buttonHeight }))
						hud_adj_mode = 6;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("Shell Point", { widthShellPoint, buttonHeight }))
						hud_adj_mode = 7;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("pos STEP", { widthPosStep, buttonHeight }))
						hud_adj_mode = 8;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("rot STEP", { widthRotStep, buttonHeight }))
						hud_adj_mode = 9;
					ImGui::SameLine(0, padding);
					if (ImGui::Button("Crosshair", { widthCrosshair, buttonHeight }))
						hud_adj_crosshair = !hud_adj_crosshair;
				}
				else
				{
					hud_adj_mode = 0;
				}

				ImGui::End();

				if (EnableAdjust && hud_adj_mode == 0)
				{
					hud_adj_mode = 1;
				}
			}
		);
	}
#endif
}

void player_hud_motion_container::load_default_motions(IKinematicsAnimated* model, const CInifile::Item& data, const shared_str& sect)
{
	player_hud_motion* pm = nullptr;

	string512 buff = {};
	MotionID motion_ID;

	m_names[data.first] = true;
	const shared_str& anm = data.second;
	m_anims.resize(m_anims.size() + 1);
	pm = &m_anims.back();
	pm->m_alias_name = data.first;

	int items_count = _GetItemCount(anm.c_str());
	if (items_count == 1)
	{
		pm->m_base_name = anm;
		pm->m_additional_name = anm;
		pm->m_anim_speed = 1.0f;
	}
	else
	{
		string512 str_item = {};
		_GetItem(anm.c_str(), 0, str_item);
		pm->m_base_name = str_item;

		_GetItem(anm.c_str(), 1, str_item);
		pm->m_additional_name = (strlen(str_item) > 0) ? pm->m_additional_name = str_item : pm->m_base_name;

		_GetItem(anm.c_str(), 2, str_item);
		pm->m_anim_speed = strlen(str_item) > 0 ? atof(str_item) : 1.0f;

		if (items_count > 3)
		{
			for (u32 j = 3; j < items_count; ++j)
			{
				string512 str_item = {};
				_GetItem(anm.c_str(), j, str_item);
				pm->m_bone_parts.push_back(str_item);
			}
		}
	}


	for (u32 i = 0; i <= 8; ++i)
	{
		if (i == 0)
		{
			xr_strcpy(buff, pm->m_base_name.c_str());
		}
		else
		{
			xr_sprintf(buff, "%s%d", pm->m_base_name.c_str(), i);
		}

		motion_ID = model->ID_Cycle_Safe(buff);
		if (!motion_ID.valid() && i == 0)
		{
			motion_ID = model->ID_Cycle_Safe("hand_idle_doun");
			Msg("! motion not found[%s] section [%s]", pm->m_base_name.c_str(), sect.c_str());
		}

		if (motion_ID.valid())
		{
			pm->m_animations.resize(pm->m_animations.size() + 1);
			pm->m_animations.back().mid = motion_ID;
			pm->m_animations.back().name = buff;
		}
	}

	R_ASSERT2(pm->m_animations.size(), make_string<const char*>("motion not found [%s]", pm->m_base_name.c_str()));
}

void player_hud_motion_container::load_bonepart_motions(IKinematicsAnimated* model, const CInifile::Item& data, const shared_str& sect)
{
	if (model == nullptr)
	{
		return;
	}

	attachable_hud_item_motion* ahim = nullptr;

	string512 buff = {};
	MotionID motion_ID;

	m_names[data.first] = true;
	const shared_str& anm = data.second;
	m_item_anims.resize(m_item_anims.size() + 1);
	ahim = &m_item_anims.back();
	ahim->m_alias_name = data.first;

	int items_count = _GetItemCount(anm.c_str());
	if (items_count == 1)
	{
		ahim->m_name = anm;
		ahim->m_anim_speed = 1.0f;
	}
	else
	{
		string512 str_item = {};
		_GetItem(anm.c_str(), 0, str_item);
		ahim->m_name = str_item;

		for (u32 j = 1; j < items_count; ++j)
		{
			string512 str_item = {};
			_GetItem(anm.c_str(), j, str_item);
			ahim->m_bone_parts.push_back(str_item);

			if (std::find(m_banned_bone_parts.begin(), m_banned_bone_parts.end(), str_item) == m_banned_bone_parts.end())
			{
				m_banned_bone_parts.push_back(str_item);
			}
		}

		ahim->m_anim_speed = 1.0f;
	}

	for (u32 i = 0; i <= 8; ++i)
	{
		if (i == 0)
		{
			xr_strcpy(buff, ahim->m_name.c_str());
		}
		else
		{
			xr_sprintf(buff, "%s%d", ahim->m_name.c_str(), i);
		}

		motion_ID = model->ID_Cycle_Safe(buff);
		if (!motion_ID.valid() && i == 0)
		{
			motion_ID = model->ID_Cycle_Safe("idle");
			Msg("! motion not found[%s] section [%s]", ahim->m_name.c_str(), sect.c_str());
		}

		if (motion_ID.valid())
		{
			ahim->m_animations.resize(ahim->m_animations.size() + 1);
			ahim->m_animations.back().mid = motion_ID;
			ahim->m_animations.back().name = buff;
		}
	}

	R_ASSERT2(ahim->m_animations.size(), make_string<const char*>("motion not found [%s]", ahim->m_name.c_str()));
}

Fvector& attachable_hud_item::hands_attach_pos()
{
	return m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos];
}

Fvector& attachable_hud_item::hands_attach_rot()
{
	return m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisRot];
}

Fvector& attachable_hud_item::hands_offset_pos()
{
	EHudOffsetType idx = m_parent_hud_item->GetCurrentHudOffsetIdx();
	return m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][idx];
}

Fvector& attachable_hud_item::hands_offset_rot()
{
	EHudOffsetType idx = m_parent_hud_item->GetCurrentHudOffsetIdx();
	return m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][idx];
}

void attachable_hud_item::set_bone_visible(const shared_str& bone_name, bool bVisibility, bool bSilent)
{
	u16  bone_id;
	bool bVisibleNow;
	bone_id			= m_model->LL_BoneID			(bone_name);
	if(bone_id==BI_NONE)
	{
		if(bSilent)	return;
		R_ASSERT2	(0, make_string<const char*>("model [%s] has no bone [%s]",m_visual_name.c_str(), bone_name.c_str()));
	}
	bVisibleNow		= m_model->LL_GetBoneVisible	(bone_id);
	if(bVisibleNow!=bVisibility)
		m_model->LL_SetBoneVisible	(bone_id,bVisibility, false);
}

void attachable_hud_item::update(bool bForce)
{
	if(!bForce && m_upd_firedeps_frame==Device.dwFrame)	return;
	bool isWidescreen = UI().is_widescreen() && !m_model_combined;
	
	if(!!m_measures.m_prop_flags.test(hud_item_measures::e_16x9_mode_now)!=isWidescreen)
	{
		m_measures.load(m_sect_name, m_model, m_model_combined);
	}

	Fvector ypr = m_measures.m_item_attach[EHudOffsetAxis::eAxisRot];
	ypr.mul							(PI/180.f);
	m_attach_offset.setHPB			(ypr.x,ypr.y,ypr.z);
	m_attach_offset.translate_over(m_measures.m_item_attach[EHudOffsetAxis::eAxisPos]);

	m_parent->calc_transform		(m_attach_place_idx, m_attach_offset, m_item_transform, m_model_combined);
	m_upd_firedeps_frame			= Device.dwFrame;

	IKinematicsAnimated* ka			=	m_model->dcast_PKinematicsAnimated();
	if(ka)
	{
		ka->UpdateTracks									();
		ka->dcast_PKinematics()->CalculateBones_Invalidate	();
		ka->dcast_PKinematics()->CalculateBones				(true);
	}
}

void attachable_hud_item::update_hud_additional(Fmatrix& trans)
{
	if (m_parent_hud_item)
	{
		m_parent_hud_item->UpdateHudAdditonal(trans);
	}

	m_collision_box.m_halfsize = m_measures.m_collision_params.obb_scale;
	trans.transform_tiny(m_collision_box.m_translate, m_measures.m_collision_params.obb_pos);
	m_collision_box.m_rotate.set(trans);

	CDB::COLLIDER xrc;
	xrc.obb_options(CDB::OPT_FULL_TEST);
	xrc.obb_query(Level().ObjectSpace.GetStaticModel(), m_collision_box);

	float nearest_obb_dist = 1000.0f;

	for (CDB::RESULT& result : xrc.r_vec())
	{
		SGameMtl* pMtl = GMLib.GetMaterialByIdx(result.material);
		if (pMtl && (pMtl->Flags.is(SGameMtl::flPassable) || pMtl->Flags.is(SGameMtl::flActorObstacle)))
		{
			continue;
		}

		m_collision_box.FindContactsClipping(result.verts, [](const Fvector& contact, void* user_data)
		{
			float& nearest_obb_dist = *(float*)user_data;
			float to_cam_dist = Device.vCameraPosition.distance_to(contact);
			if (to_cam_dist < nearest_obb_dist)
			{
				nearest_obb_dist = to_cam_dist;
			}

		}, &nearest_obb_dist);
	}
	clamp(nearest_obb_dist, 0.0f, 1.0f);

	float fContactK = 1.0f - ((nearest_obb_dist - m_measures.m_collision_params.dist_min) / (m_measures.m_collision_params.dist_max - m_measures.m_collision_params.dist_min));

	clamp(fContactK, 0.0f, 1.0f);

	fContactK *= 1.0f - (m_parent->attached_item(0) ? m_parent->attached_item(0)->m_parent_hud_item->GetAimFactor() : 0.0f);

	Fvector fTargetPos = m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eCollision];
	fTargetPos.mul(fContactK);
	Fvector fTargetRot = m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eCollision];
	fTargetRot.mul(fContactK);

	m_collision_inertia_pos.spring_inertion(fTargetPos, m_collision_inertia_pos_vel, Device.fTimeDelta,
		m_measures.m_collision_params.stifness, m_measures.m_collision_params.damping);
	m_collision_inertia_rot.spring_inertion(fTargetRot, m_collision_inertia_rot_vel, Device.fTimeDelta,
		m_measures.m_collision_params.stifness, m_measures.m_collision_params.damping);

	Fmatrix hud_collision;
	hud_collision.identity();
	hud_collision.rotateX(m_collision_inertia_rot.x);

	Fmatrix hud_collision_y;
	hud_collision_y.identity();
	hud_collision_y.rotateY(m_collision_inertia_rot.y);
	hud_collision.mulA_43(hud_collision_y);

	Fmatrix hud_collision_z;
	hud_collision_z.identity();
	hud_collision_z.rotateZ(m_collision_inertia_rot.z);
	hud_collision.mulA_43(hud_collision_z);

	hud_collision.translate_over(m_collision_inertia_pos);
	CollisionXform.set(hud_collision);
	trans.mulB_43(hud_collision);
}

void attachable_hud_item::setup_firedeps(firedeps& fd)
{
	update							(false);
	if(m_measures.m_prop_flags.test(hud_item_measures::e_fire_point))
	{
		R_ASSERT4(m_measures.m_fire_bone != BI_NONE, 
			"Invalid fire bone specified", 
			m_sect_name.c_str(), 
			pSettings->r_string(m_sect_name, "fire_bone"));

		Fmatrix& fire_mat								= m_model->LL_GetTransform(m_measures.m_fire_bone);
		fire_mat.transform_tiny							(fd.vLastFP, m_measures.m_fire_point_offset);
		m_item_transform.transform_tiny					(fd.vLastFP);

		fd.vLastFD.set									(m_item_transform.k);
		VERIFY(_valid(fd.vLastFD));

		fd.m_FireParticlesXForm.identity				();
		fd.m_FireParticlesXForm.set(m_item_transform);
		VERIFY(_valid(fd.m_FireParticlesXForm));
	}

	if(m_measures.m_prop_flags.test(hud_item_measures::e_fire_point) || m_measures.m_prop_flags.test(hud_item_measures::e_fire_point2))
	{
		Fmatrix& fire_mat			= m_measures.m_fire_bone2 != u16(-1) ? m_model->LL_GetTransform(m_measures.m_fire_bone2) : m_model->LL_GetTransform(m_measures.m_fire_bone);
		fire_mat.transform_tiny		(fd.vLastFP2,m_measures.m_fire_point2_offset);
		m_item_transform.transform_tiny	(fd.vLastFP2);
		VERIFY(_valid(fd.vLastFP2));
	}

	if(m_measures.m_prop_flags.test(hud_item_measures::e_shell_point))
	{
		Fmatrix& fire_mat = m_measures.m_shell_bone != u16(-1) ? m_model->LL_GetTransform(m_measures.m_shell_bone) : m_model->LL_GetTransform(m_measures.m_fire_bone);
		fire_mat.transform_tiny		(fd.vLastSP,m_measures.m_shell_point_offset);
		m_item_transform.transform_tiny	(fd.vLastSP);
		VERIFY(_valid(fd.vLastSP));
	}
}

void hud_item_measures::hud_hands_positions::Load(const shared_str& section, bool is_16x9)
{
	bool default_is_self = sSection.size() > 0;

	if (sSection.size() == 0 || bIs16x9 == is_16x9)
	{
		sSection = section;
	}

	bIs16x9 = is_16x9;

	string64 _prefix = {};
	xr_sprintf(_prefix, "%s", bIs16x9 ? "_16x9" : "");
	string128 val_name = {};

	xr_strconcat(val_name, "hands_position", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault] : zero_vel);
	xr_strconcat(val_name, "hands_orientation", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault] : zero_vel);
	
	LoadAimParams(section, is_16x9, default_is_self);

	xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimAlt] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimAlt] : zero_vel);
	xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimAlt] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimAlt] : zero_vel);

	xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eSafemode] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eSafemode] : zero_vel);
	xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eSafemode] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eSafemode] : zero_vel);

	xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eCollision] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eCollision] : zero_vel);
	xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eCollision] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eCollision] : zero_vel);

	memcpy(hands_offsets_tune, hands_offsets, sizeof(hands_offsets_tune));

	hands_offsets_saved[EHudOffsetAxis::eAxisPos] = zero_vel;
	hands_offsets_saved[EHudOffsetAxis::eAxisRot] = zero_vel;
}

void hud_item_measures::hud_hands_positions::LoadAimParams(const shared_str& section, bool is_16x9, bool default_is_self)
{
	string64 widescreenPrefix = {};
	xr_sprintf(widescreenPrefix, "%s", bIs16x9 ? "_16x9" : "");
	string128 val_name = {};

	xr_strconcat(val_name, "aim_hud_offset_pos", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, "zoom_offset", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] : zero_vel);
	}

	xr_strconcat(val_name, "aim_hud_offset_rot", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].x = READ_IF_EXISTS(pSettings, r_float, sSection, "zoom_rotate_x", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].x : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].y = READ_IF_EXISTS(pSettings, r_float, sSection, "zoom_rotate_y", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].y : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].z = READ_IF_EXISTS(pSettings, r_float, sSection, "zoom_rotate_z", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].z : 0.0f);
	}

	xr_strconcat(val_name, "gl_hud_offset_pos", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, "grenade_zoom_offset", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] : zero_vel);
	}

	xr_strconcat(val_name, "gl_hud_offset_rot", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL] = READ_IF_EXISTS(pSettings, r_fvector3, sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].x = READ_IF_EXISTS(pSettings, r_float, sSection, "grenade_zoom_rotate_x", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].x : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].y = READ_IF_EXISTS(pSettings, r_float, sSection, "grenade_zoom_rotate_y", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].y : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].z = READ_IF_EXISTS(pSettings, r_float, sSection, "grenade_zoom_rotate_z", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].z : 0.0f);
	}
}

bool attachable_hud_item::need_renderable()
{
	return m_parent_hud_item->need_renderable();
}

void attachable_hud_item::render()
{
	::Render->set_Transform		(&m_item_transform);
	::Render->add_Visual		(m_model->dcast_RenderVisual(), true);
	debug_draw_firedeps			();

	m_parent_hud_item->item().render_attachments(m_item_transform, m_model, true);
	m_parent_hud_item->render_hud_mode();
}

bool attachable_hud_item::render_item_ui_query()
{
	return m_parent_hud_item->render_item_3d_ui_query();
}

void attachable_hud_item::render_item_ui()
{
	m_parent_hud_item->render_item_3d_ui();
}

void hud_item_measures::load(const shared_str& sect_name, IKinematics* K, bool combined_model)
{
	if (combined_model)
	{
		m_item_attach[EHudOffsetAxis::eAxisPos] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "position", zero_vel);
		m_item_attach[EHudOffsetAxis::eAxisRot] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "orientation", zero_vel);
	}
	else
	{
		m_item_attach[EHudOffsetAxis::eAxisPos] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "item_position", zero_vel);
		m_item_attach[EHudOffsetAxis::eAxisRot] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "item_orientation", zero_vel);
	}

	shared_str bone_name;
	m_prop_flags.set(e_fire_point, pSettings->line_exist(sect_name, "fire_bone"));
	if (m_prop_flags.test(e_fire_point))
	{
		bone_name = pSettings->r_string(sect_name, "fire_bone");
		m_fire_bone = K->LL_BoneID(bone_name);
		m_fire_point_offset = pSettings->r_fvector3(sect_name, "fire_point");
	}
	else
	{
		m_fire_point_offset.set(0, 0, 0);
	}

	m_prop_flags.set(e_fire_point2, pSettings->line_exist(sect_name, "fire_bone2"));
	if (m_prop_flags.test(e_fire_point2))
	{
		bone_name = pSettings->r_string(sect_name, "fire_bone2");
		m_fire_bone2 = K->LL_BoneID(bone_name);
	}
	// St4lker0k765: got to move this param from fire_bone2 because SoC don't have it in configs
	m_fire_point2_offset = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "fire_point2", zero_vel);

	m_prop_flags.set(e_shell_point, pSettings->line_exist(sect_name, "shell_point"));
	if (m_prop_flags.test(e_shell_point))
	{
		bone_name = READ_IF_EXISTS(pSettings, r_string, sect_name, "shell_bone", "");
		m_shell_bone = bone_name.size() ? K->LL_BoneID(bone_name) : u16(-1);
		m_shell_point_offset = pSettings->r_fvector3(sect_name, "shell_point");
	}
	else
	{
		m_shell_point_offset.set(0, 0, 0);
	}

	m_inertion_params.m_tendto_speed = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_speed", 1.0f);
	m_inertion_params.m_tendto_speed_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_aim_speed", 1.0f);
	m_inertion_params.m_tendto_ret_speed = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_ret_speed", 5.0f);
	m_inertion_params.m_tendto_ret_speed_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_ret_aim_speed", 5.0f);

	bool is_16x9 = UI().is_widescreen() && !combined_model;

	m_hands_positions.Load(sect_name, is_16x9);
	m_hands_attach_real[EHudOffsetAxis::eAxisPos] = m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
	m_hands_attach_real[EHudOffsetAxis::eAxisRot] = m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];

	m_weapon_inertion.Load(sect_name, is_16x9);

	m_collision_params.obb_pos = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "collision_box_pos", zero_vel);
	m_collision_params.obb_scale = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "collision_box_scale", Fvector({1.0f, 1.0f, 1.0f}));

	m_collision_params.damping = READ_IF_EXISTS(pSettings, r_float, sect_name, "collision_damping", 40.0f);
	m_collision_params.stifness = READ_IF_EXISTS(pSettings, r_float, sect_name, "collision_stifness", 400.0f);

	m_collision_params.dist_min = READ_IF_EXISTS(pSettings, r_float, sect_name, "collision_dist_min", 0.5f);
	m_collision_params.dist_max = READ_IF_EXISTS(pSettings, r_float, sect_name, "collision_dist_max", 0.8f);

	m_prop_flags.set(e_16x9_mode_now,is_16x9);
}

void weapon_inertion::Load(const shared_str& section, bool is_16x9)
{
	move_suicide_offset.Load(section, "hud_move_suicide_offset", is_16x9);

	move_to_crouch_offset.Load(section, "hud_move_to_crouch_offset", is_16x9);
	move_from_crouch_offset.Load(section, "hud_move_from_crouch_offset", is_16x9);
	move_to_slow_crouch_offset.Load(section, "hud_move_to_slow_crouch_offset", is_16x9);
	move_from_slow_crouch_offset.Load(section, "hud_move_from_slow_crouch_offset", is_16x9);

	move_to_rlookout_offset.Load(section, "hud_move_to_rlookout_offset", is_16x9);
	move_from_rlookout_offset.Load(section, "hud_move_from_rlookout_offset", is_16x9);
	move_to_llookout_offset.Load(section, "hud_move_to_llookout_offset", is_16x9);
	move_from_llookout_offset.Load(section, "hud_move_from_llookout_offset", is_16x9);

	aim_move_to_crouch_offset.Load(section, "hud_aim_move_to_crouch_offset", is_16x9);
	aim_move_from_crouch_offset.Load(section, "hud_aim_move_from_crouch_offset", is_16x9);
	aim_move_to_slow_crouch_offset.Load(section, "hud_aim_move_to_slow_crouch_offset", is_16x9);
	aim_move_from_slow_crouch_offset.Load(section, "hud_aim_move_from_slow_crouch_offset", is_16x9);

	aim_move_to_rlookout_offset.Load(section, "hud_aim_move_to_rlookout_offset", is_16x9);
	aim_move_from_rlookout_offset.Load(section, "hud_aim_move_from_rlookout_offset", is_16x9);
	aim_move_to_llookout_offset.Load(section, "hud_aim_move_to_llookout_offset", is_16x9);
	aim_move_from_llookout_offset.Load(section, "hud_aim_move_from_llookout_offset", is_16x9);

	aim_move_jump_offset.Load(section, "hud_aim_move_jump_offset", is_16x9);
	aim_move_fall_offset.Load(section, "hud_aim_move_fall_offset", is_16x9);
	aim_move_landing_offset.Load(section, "hud_aim_move_landing_offset", is_16x9);
	aim_move_landing2_offset.Load(section, "hud_aim_move_landing2_offset", is_16x9);

	move_rlookout_offset.Load(section, "hud_move_rlookout_offset", is_16x9);
	move_llookout_offset.Load(section, "hud_move_llookout_offset", is_16x9);

	aim_move_rlookout_offset.Load(section, "hud_aim_move_rlookout_offset", is_16x9);
	aim_move_llookout_offset.Load(section, "hud_aim_move_llookout_offset", is_16x9);

	move_left_offset.Load(section, "hud_move_left_offset", is_16x9);
	move_right_offset.Load(section, "hud_move_right_offset", is_16x9);
	move_forward_offset.Load(section, "hud_move_forward_offset", is_16x9);
	move_back_offset.Load(section, "hud_move_back_offset", is_16x9);

	move_crouch_offset.Load(section, "hud_move_crouch_offset", is_16x9);
	move_slow_crouch_offset.Load(section, "hud_move_slow_crouch_offset", is_16x9);

	move_jump_offset.Load(section, "hud_move_jump_offset", is_16x9);
	move_fall_offset.Load(section, "hud_move_fall_offset", is_16x9);
	move_landing_offset.Load(section, "hud_move_landing_offset", is_16x9);
	move_landing2_offset.Load(section, "hud_move_landing2_offset", is_16x9);

	move_rlookout_offset_speed_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_rlookout_offset_speed_factor", 1.0f);
	move_llookout_offset_speed_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_llookout_offset_speed_factor", 1.0f);

	aim_move_slow_crouch_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_aim_move_slow_crouch_factor", 1.0f);
	aim_move_crouch_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_aim_move_crouch_factor", 1.0f);
	aim_move_slow_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_aim_move_slow_factor", 1.0f);

	move_slow_crouch_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_slow_crouch_factor", 1.0f);
	move_crouch_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_crouch_factor", 1.0f);
	move_slow_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_slow_factor", 1.0f);

	no_other_hud_moving_while_suicide = READ_IF_EXISTS(pSettings, r_bool, section, "no_other_hud_moving_while_suicide", false);

	to_crouch_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_crouch_time", 0.0f) * 1000.f);
	from_crouch_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_crouch_time", 0.0f) * 1000.f);
	to_slow_crouch_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_slow_crouch_time", 0.0f) * 1000.f);
	from_slow_crouch_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_slow_crouch_time", 0.0f) * 1000.f);

	to_rlookout_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_rlookout_time", 0.0f) * 1000.f);
	from_rlookout_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_rlookout_time", 0.0f) * 1000.f);
	to_llookout_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_llookout_time", 0.0f) * 1000.f);
	from_llookout_time = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_llookout_time", 0.0f) * 1000.f);

	move_weaponhide_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_weaponhide_factor", 1.0f);
	move_unzoom_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_unzoom_factor", 1.0f);
	move_stabilize_factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_stabilize_factor", 2.0f);

	move_speed_pos = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_speed_pos", 0.1f);
	move_speed_rot = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_speed_rot", 0.4f);

	move_suicide_speed_pos = READ_IF_EXISTS(pSettings, r_float, section, "suicide_speed_pos", 0.2f);
	move_suicide_speed_rot = READ_IF_EXISTS(pSettings, r_float, section, "suicide_speed_rot", 0.002f);
}

void weapon_inertion::base_params::Load(const shared_str& section, const shared_str& str, bool is_16x9)
{
	shared_str name;

	name.printf("%s_pos%s", *str, is_16x9 ? "_16x9" : "");
	position = READ_IF_EXISTS(pSettings, r_fvector3, section, *name, zero_vel);

	name.printf("%s_rot%s", *str, is_16x9 ? "_16x9" : "");
	rotation = READ_IF_EXISTS(pSettings, r_fvector3, section, *name, zero_vel);
}

attachable_hud_item::~attachable_hud_item()
{
	IRenderVisual* v			= m_model->dcast_RenderVisual();
	::Render->model_Delete		(v);
	m_model						= nullptr;
}

static void hud_read_motion_list(const shared_str& sect, const char* key, xr_vector<shared_str>& list)
{
	list.clear();
	if (const char* motions = READ_IF_EXISTS(pSettings, r_string, sect, key, nullptr))
	{
		string128 motion;
		for (int i = 0, count = _GetItemCount(motions); i < count; ++i)
		{
			list.emplace_back(_GetItem(motions, i, motion));
		}
	}
}

static bool hud_motion_listed(const xr_vector<shared_str>& list, const shared_str& motion)
{
	for (const shared_str& name : list)
	{
		const size_t length = name.size();
		if (length > 0 && name.c_str()[length - 1] == '*' ? strncmp(motion.c_str(), name.c_str(), length - 1) == 0 : motion == name)
		{
			return true;
		}
	}

	return false;
}

static const shared_str& hud_retarget_source_idle(const shared_str& motion)
{
	static xr_hash_map<shared_str, shared_str> sources;
	static xr_hash_set<shared_str> idles;
	static bool built = false;
	if (!built)
	{
		built = true;
		string128 name;
		const auto read_list = [&name](CInifile::Sect& sect, const char* key, xr_vector<shared_str>& list)
		{
			const char* value = nullptr;
			if (sect.line_exist(key, &value) && value != nullptr)
			{
				for (int i = 0, count = _GetItemCount(value); i < count; ++i)
				{
					list.emplace_back(_GetItem(value, i, name));
				}
			}
		};

		for (int pass = 0; pass < 3; ++pass)
		{
			for (CInifile::Sect& sect : pSettings->sections())
			{
				const char* idle = nullptr;
				if (!sect.line_exist("anm_idle", &idle) || idle == nullptr || _GetItemCount(idle) == 0)
				{
					continue;
				}

				const bool motion_set = !sect.line_exist("item_visual") && !sect.line_exist("visual");
				if (motion_set != (pass == 0))
				{
					continue;
				}

				const shared_str idle_motion = _GetItem(idle, 0, name);
				idles.insert(idle_motion);
				xr_vector<shared_str> retarget;
				read_list(sect, "retarget_motions", retarget);

				for (const CInifile::Item& line : sect.Data)
				{
					const char* alias = line.first.c_str();
					if (alias == nullptr || strstr(alias, "anm_") != alias || strstr(alias, "anm_bp_") == alias || line.second.size() == 0
						|| (motion_set ? xr_strcmp(alias, "anm_idle") == 0 : strstr(alias, "anm_idle") == alias || hud_motion_listed(retarget, line.first) != (pass == 2)))
					{
						continue;
					}

					sources.try_emplace(shared_str(_GetItem(line.second.c_str(), 0, name)), idle_motion);
				}
			}
		}
	}

	static xr_hash_map<shared_str, shared_str> by_name;
	if (const auto named = by_name.find(motion); named != by_name.end())
	{
		return named->second;
	}

	shared_str found;
	xr_string prefix = motion.c_str() != nullptr ? motion.c_str() : "";
	for (size_t split = prefix.rfind('_'); split != xr_string::npos && split > 0; split = prefix.rfind('_'))
	{
		prefix.resize(split);
		const shared_str candidate = (prefix + "_idle").c_str();
		if (idles.contains(candidate))
		{
			found = candidate;
			break;
		}
	}

	if (found.size() == 0)
	{
		if (const auto it = sources.find(motion); it != sources.end())
		{
			found = it->second;
		}
	}

	return by_name.try_emplace(motion, found).first->second;
}

void attachable_hud_item::load(const shared_str& sect_name)
{
	m_sect_name					= sect_name;

	m_model_combined			= !pSettings->line_exist(sect_name, "item_visual");
	m_visual_name = m_model_combined ? pSettings->r_string(sect_name, "visual") : pSettings->r_string(sect_name, "item_visual");
	m_model						 = PKinematics(::Render->model_Create(m_visual_name.c_str()));

	LoadRigParams();

	m_attach_place_idx = READ_IF_EXISTS(pSettings, r_u16, sect_name, "attach_place_idx", 0);

	if (pSettings->line_exist(sect_name, "aim_bone_name"))
		m_aim_bone_id = m_model->LL_BoneID(pSettings->r_string(sect_name, "aim_bone_name"));

	HudMass = std::max(READ_IF_EXISTS(pSettings, r_float, sect_name, "hud_mass", 1.f), 0.1f);
	IkEnabled = READ_IF_EXISTS(pSettings, r_bool, sect_name, "hud_ik", true);

	hud_read_motion_list(sect_name, "additive_motions", AdditiveMotions);
	AdditiveReference = READ_IF_EXISTS(pSettings, r_string, sect_name, "additive_reference", nullptr);
	hud_read_motion_list(sect_name, "retarget_motions", RetargetMotions);
	RetargetReference = READ_IF_EXISTS(pSettings, r_string, sect_name, "retarget_reference", nullptr);
	LoadRetargetLayers();
	hud_read_motion_list(sect_name, "left_hand_free_motions", LeftHandFreeMotions);

	const char* left_ik_bone = READ_IF_EXISTS(pSettings, r_string, sect_name, "left_hand_ik_bone", READ_IF_EXISTS(pSettings, r_string, "camera_rig", "left_hand_ik_bone", nullptr));
	if (!m_model_combined && left_ik_bone != nullptr)
	{
		LeftIkBone = m_model->LL_BoneID(left_ik_bone);
		LeftIkFixed = pSettings->line_exist(sect_name, "left_hand_ik_pos") || pSettings->line_exist(sect_name, "left_hand_ik_rot");
		Fvector ypr = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "left_hand_ik_rot", zero_vel);
		ypr.mul(PI / 180.f);
		LeftIkOffset.setHPB(ypr.x, ypr.y, ypr.z);
		LeftIkOffset.c = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "left_hand_ik_pos", zero_vel);

		LeftHandAttachTarget = READ_IF_EXISTS(pSettings, r_string, sect_name, "left_hand_attach_target", READ_IF_EXISTS(pSettings, r_string, "camera_rig", "left_hand_attach_target", "anm_idle"));

		hud_read_motion_list(sect_name, "left_hand_attach_motions", LeftHandAttachMotions);
		hud_read_motion_list(sect_name, "left_hand_attach_animated_arm_motions", LeftHandAttachAnimatedArmMotions);
	}

	m_measures.load				(sect_name, m_model, m_model_combined);
}

struct SHudRigField
{
	const char*						Key;
	float attachable_hud_item::*	Float;
	Fvector attachable_hud_item::*	Vector;
	float							Min = -flt_max;
	float							Max = flt_max;
};

static const SHudRigField g_hud_rig_fields[] =
{
	{ "aim_reload_motion", &attachable_hud_item::AimReloadMotion, nullptr },
	{ "aim_reload_hud_offset_pos", nullptr, &attachable_hud_item::AimReloadOffsetPos },
	{ "aim_reload_hud_offset_rot", nullptr, &attachable_hud_item::AimReloadOffsetRot },
	{ "aim_reload_zoom_out", &attachable_hud_item::AimReloadZoomOut, nullptr, 0.f, 1.f },
	{ "aim_reload_pose_inertia", &attachable_hud_item::AimReloadPoseInertia, nullptr, 0.f },
	{ "aim_reload_pose_inertia_out", &attachable_hud_item::AimReloadPoseInertiaOut, nullptr, 0.f },
	{ "aim_reload_hud_push_pos", nullptr, &attachable_hud_item::AimReloadPushPos },
	{ "crouch_hud_offset_pos", nullptr, &attachable_hud_item::CrouchOffsetPos },
	{ "crouch_hud_offset_rot", nullptr, &attachable_hud_item::CrouchOffsetRot },
	{ "crouch_pose_inertia", &attachable_hud_item::CrouchPoseInertia, nullptr, 0.f },
	{ "crouch_pose_inertia_out", &attachable_hud_item::CrouchPoseInertiaOut, nullptr },
	{ "crouch_pose_delay", &attachable_hud_item::CrouchPoseDelay, nullptr, 0.f },
	{ "walk_hud_offset_pos", nullptr, &attachable_hud_item::WalkOffsetPos },
	{ "walk_hud_offset_rot", nullptr, &attachable_hud_item::WalkOffsetRot },
	{ "walk_pose_delay", &attachable_hud_item::WalkPoseDelay, nullptr, 0.f },
	{ "walk_pose_inertia", &attachable_hud_item::WalkPoseInertia, nullptr, 0.f },
	{ "walk_pose_inertia_out", &attachable_hud_item::WalkPoseInertiaOut, nullptr, 0.f },
	{ "carry_pose_damping", &attachable_hud_item::CarryPoseDamping, nullptr, 0.f },
	{ "look_sway_pos", nullptr, &attachable_hud_item::LookSwayPos },
	{ "aim_sway_scale", &attachable_hud_item::AimSwayScale, nullptr, 0.f },
	{ "look_sway_inertia", &attachable_hud_item::LookSwayInertia, nullptr, 0.f },
	{ "look_sway_damping", &attachable_hud_item::LookSwayDamping, nullptr, 0.f },
	{ "left_hand_sway_look", nullptr, &attachable_hud_item::LeftSwayLook },
	{ "left_hand_sway_bob", nullptr, &attachable_hud_item::LeftSwayBob },
	{ "left_hand_sway_jump", nullptr, &attachable_hud_item::LeftSwayJump },
	{ "right_hand_sway_look", nullptr, &attachable_hud_item::RightSwayLook },
	{ "right_hand_sway_jump", nullptr, &attachable_hud_item::RightSwayJump },
	{ "left_hand_follow_lag", &attachable_hud_item::LeftFollowLag, nullptr, 0.f },
	{ "left_hand_follow_turn", &attachable_hud_item::LeftFollowTurn, nullptr, 0.f, 1.f },
	{ "left_hand_follow_move", nullptr, &attachable_hud_item::LeftFollowMove },
	{ "left_hand_follow_move_rot", &attachable_hud_item::LeftFollowMoveRot, nullptr, 0.f, 1.f },
	{ "left_hand_bob_pos", nullptr, &attachable_hud_item::LeftBobPos },
	{ "left_hand_bob_rot", nullptr, &attachable_hud_item::LeftBobRot },
	{ "hand_sway_inertia", &attachable_hud_item::HandSwayInertia, nullptr, 0.f },
	{ "hand_sway_damping", &attachable_hud_item::HandSwayDamping, nullptr, 0.f },
	{ "turn_pivot_look", nullptr, &attachable_hud_item::TurnPivotLook },
	{ "turn_pivot_max", &attachable_hud_item::TurnPivotMax, nullptr, 0.f },
	{ "turn_pivot_inertia", &attachable_hud_item::TurnPivotInertia, nullptr, 0.f },
	{ "turn_pivot_damping", &attachable_hud_item::TurnPivotDamping, nullptr, 0.f },
	{ "turn_pivot_lift_fall", &attachable_hud_item::TurnPivotLiftFall, nullptr, 1.f },
	{ "aim_turn_pivot_scale", &attachable_hud_item::AimTurnPivotScale, nullptr, 0.f },
	{ "turn_pivot_grip_lag", &attachable_hud_item::TurnPivotGripLag, nullptr, 0.f, 1.f },
	{ "turn_pivot_lift", &attachable_hud_item::TurnPivotLift, nullptr },
	{ "turn_pivot_lift_pos", &attachable_hud_item::TurnPivotLiftPos, nullptr },
	{ "turn_pivot_lift_max", &attachable_hud_item::TurnPivotLiftMax, nullptr, 0.f },
	{ "turn_pivot_lift_inertia", &attachable_hud_item::TurnPivotLiftInertia, nullptr, 0.f },
	{ "turn_tilt", &attachable_hud_item::TurnTilt, nullptr },
	{ "turn_tilt_max", &attachable_hud_item::TurnTiltMax, nullptr, 0.f },
	{ "aim_blend_inertia", &attachable_hud_item::AimBlendInertia, nullptr, 0.f },
	{ "sprint_pose_inertia", &attachable_hud_item::SprintPoseInertia, nullptr, 0.f },
	{ "sprint_blend_in", &attachable_hud_item::SprintBlendIn, nullptr, 0.f },
	{ "sprint_left_grip_range", &attachable_hud_item::SprintLeftGripRange, nullptr, 0.f },
	{ "sprint_left_cancel_return", &attachable_hud_item::SprintLeftCancelReturn, nullptr, 0.f },
	{ "shot_anim_variation", &attachable_hud_item::ShotVariation, nullptr },
	{ "shot_anim_twist", &attachable_hud_item::ShotTwist, nullptr },
	{ "aim_shot_motion", &attachable_hud_item::AimShotMotion, nullptr },
	{ "idle_fade", &attachable_hud_item::IdleFade, nullptr },
	{ "inertia_damping", &attachable_hud_item::InertiaDamping, nullptr },
	{ "additive_blend_in", &attachable_hud_item::AdditiveBlendIn, nullptr, 0.f },
	{ "additive_blend_out", &attachable_hud_item::AdditiveBlendOut, nullptr, 0.01f },
	{ "left_hand_attach_blend", &attachable_hud_item::LeftHandAttachBlend, nullptr, 0.f },
	{ "left_hand_ik_blend", &attachable_hud_item::LeftHandIkBlend, nullptr, 0.f },
	{ "left_hand_finger_grip", &attachable_hud_item::LeftHandFingerGrip, nullptr, 0.f, 1.f },
};

void attachable_hud_item::LoadRigParams()
{
	const shared_str& sect_name = m_sect_name;

	const auto load_bones = [&](const char* key, xr_vector<u16>& bones)
	{
		bones.clear();
		const char* list = READ_IF_EXISTS(pSettings, r_string, sect_name, key, nullptr);
		string128 bone_name;
		for (int i = 0, count = list != nullptr ? _GetItemCount(list) : 0; i < count; ++i)
		{
			const u16 bone = m_model->LL_BoneID(_GetItem(list, i, bone_name));
			if (bone != BI_NONE)
			{
				bones.push_back(bone);
			}
		}
	};

	load_bones("left_hand_item_bones", LeftHandItemBones);
	load_bones("right_hand_item_bones", RightHandItemBones);

	const char* pivot_bone = READ_IF_EXISTS(pSettings, r_string, sect_name, "turn_pivot_bone", READ_IF_EXISTS(pSettings, r_string, "camera_rig", "turn_pivot_bone", "wpn_body"));
	TurnPivotBone = m_model_combined ? BI_NONE : m_model->LL_BoneID(pivot_bone);

	if (IKinematicsAnimated* animated = m_model->dcast_PKinematicsAnimated())
	{
		animated->LL_SetInertialization(CCameraRig::HudInertialization());
		animated->LL_SetSmoothBlend(CCameraRig::Enabled());

		const char* exclude = READ_IF_EXISTS(pSettings, r_string, sect_name, "inertialization_exclude_bones", READ_IF_EXISTS(pSettings, r_string, "camera_rig", "hud_inertialization_exclude_bones", nullptr));
		if (exclude != nullptr)
		{
			string128 bone_name;
			for (int i = 0, count = _GetItemCount(exclude); i < count; ++i)
			{
				const u16 bone = m_model->LL_BoneID(_GetItem(exclude, i, bone_name));
				if (bone != BI_NONE)
				{
					animated->LL_SetInertializationBone(bone, false);
				}
			}
		}
	}

	for (const SHudRigField& it : g_hud_rig_fields)
	{
		const SCameraRigParam* param = CCameraRig::FindHudParam(it.Key);
		if (it.Vector != nullptr)
		{
			const Fvector def = param != nullptr ? Fvector().set(param->Value[0], param->Value[1], param->Value[2]) : Fvector().set(0.f, 0.f, 0.f);
			this->*it.Vector = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, it.Key, def);
		}
		else
		{
			const float def = param != nullptr ? param->Value[0] : 0.f;
			this->*it.Float = std::clamp(READ_IF_EXISTS(pSettings, r_float, sect_name, it.Key, def), it.Min, it.Max);
		}
	}
}

float* attachable_hud_item::RigField(const char* key, u32& count)
{
	for (const SHudRigField& it : g_hud_rig_fields)
	{
		if (xr_strcmp(it.Key, key) == 0)
		{
			count = it.Vector != nullptr ? 3 : 1;
			return it.Vector != nullptr ? &(this->*it.Vector).x : &(this->*it.Float);
		}
	}

	count = 0;
	return nullptr;
}

static CBlend* hud_playing_cycle(IKinematicsAnimated* ka, u16 part, MotionID motion, u8 channel = 0, MotionID reference = MotionID())
{
	for (u32 i = 0, count = ka->LL_PartBlendsCount(part); i < count; i++)
	{
		CBlend* B = ka->LL_PartBlend(part, i);
		if (B != nullptr && B->channel == channel && B->motionID == motion && B->additive_ref == reference && !B->stop_at_end && B->blend_state() != CBlend::eFalloff)
		{
			return B;
		}
	}

	return nullptr;
}

static CBlend* hud_play_cycle(IKinematicsAnimated* ka, u16 part, MotionID motion, bool mix, float speed, float fade, const SHudCycleMode& mode)
{
	if (mode.Type == EHudCycleMode::Additive)
	{
		if (CBlend* Playing = mode.Loop ? hud_playing_cycle(ka, part, motion, ADDITIVE_CHANNEL, mode.Reference) : nullptr)
		{
			return Playing;
		}

		ka->LL_FadeCycle(part, 1.f / mode.BlendOut, 1 << ADDITIVE_CHANNEL);
		CMotionDef* def = ka->LL_GetMotionDef(motion);
		const bool blend_in = mode.BlendIn > 0.f;
		CBlend* B = ka->LL_PlayCycle(part, motion, blend_in, blend_in ? 1.f / mode.BlendIn : 1.f, 1.f / mode.BlendOut, (def != nullptr ? def->Speed() : 1.f) * speed, !mode.Loop, nullptr, nullptr, ADDITIVE_CHANNEL);
		if (B != nullptr)
		{
			B->additive_ref = mode.Reference;
		}

		return B;
	}

	if (mode.FadeAdditive)
	{
		ka->LL_FadeCycle(part, 1.f / mode.BlendOut, 1 << ADDITIVE_CHANNEL);
	}

	if (mode.Type == EHudCycleMode::KeepBase)
	{
		if (CBlend* B = hud_playing_cycle(ka, part, motion))
		{
			return B;
		}
	}

	CBlend* B = ka->PlayCycle(part, motion, mix || fade > 0.f);
	if (B == nullptr)
	{
		return nullptr;
	}

	B->speed *= speed;
	if (fade <= 0.f)
	{
		return B;
	}

	B->blendAccrue = 1.f / fade;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(part); i < count; i++)
	{
		CBlend* old = ka->LL_PartBlend(part, i);
		if (old != nullptr && old != B && old->channel == B->channel)
		{
			old->blendFalloff = 1.f / fade;
		}
	}

	return B;
}

void attachable_hud_item::anim_play(const shared_str& item_anm_name, EHudMixType bMixIn, float speed, player_hud_motion* anm)
{
	if (IKinematicsAnimated* ka = m_model->dcast_PKinematicsAnimated())
	{
		MotionID M2 = ka->ID_Cycle_Safe(item_anm_name);
		if (!M2.valid())
		{
			M2 = ka->ID_Cycle_Safe("idle");
		}
		else if (bDebug)
		{
			Msg("playing item animation [%s]", item_anm_name.c_str());
		}

		R_ASSERT3(M2.valid(), "model has no motion [idle] ", m_visual_name.c_str());

		if (!m_model_combined)
		{
			u16 root_id = m_model->LL_GetBoneRoot();
			CBoneInstance& root_binst = m_model->LL_GetBoneInstance(root_id);
			root_binst.set_callback_overwrite(true);
			root_binst.mTransform.identity();
		}

		u16 pc = ka->partitions().count();
		for (u16 pid = 0; pid < pc; ++pid)
		{
			const shared_str& part_name = ka->partitions().part(pid).Name;

			bool skip = true;

			if (anm != nullptr)
			{
				if (std::find(anm->m_bone_parts.begin(), anm->m_bone_parts.end(), part_name) != anm->m_bone_parts.end())
				{
					skip = false;
				}
				else if (std::find(m_hand_motions.m_banned_bone_parts.begin(), m_hand_motions.m_banned_bone_parts.end(), part_name) == m_hand_motions.m_banned_bone_parts.end())
				{
					skip = false;
				}
			}

			if (skip)
			{
				continue;
			}

			SHudCycleMode item_mode = m_parent->CycleMode;
			item_mode.Reference = MotionID();
			CBlend* B = hud_play_cycle(ka, pid, M2, bMixIn > EHudMixType::eMixHands, speed, m_parent->CycleFade, item_mode);
			R_ASSERT(B);
		}

		m_model->CalculateBones_Invalidate();

		m_parent_hud_item->item().anim_play_attachment(item_anm_name, speed, bMixIn > EHudMixType::eMixHands);
	}
}

void attachable_hud_item::anim_play_bonepart(const shared_str& anim, bool bMixIn)
{
	if (IKinematicsAnimated* ka = m_model->dcast_PKinematicsAnimated())
	{
		attachable_hud_item_motion* anm = m_hand_motions.find_item_motion(anim);

		if (anm == nullptr)
		{
			Msg("Missing anim [%s]", *anim);
			return;
		}

		MotionID M2 = ka->ID_Cycle_Safe(anm->m_name);
		if (!M2.valid())
		{
			M2 = ka->ID_Cycle_Safe("idle");
		}
		else if (bDebug)
		{
			Msg("playing item animation [%s]", anm->m_name.c_str());
		}

		R_ASSERT3(M2.valid(), "model has no motion [idle] ", m_visual_name.c_str());

		u16 root_id = m_model->LL_GetBoneRoot();
		CBoneInstance& root_binst = m_model->LL_GetBoneInstance(root_id);
		root_binst.set_callback_overwrite(true);
		root_binst.mTransform.identity();

		u16 pc = ka->partitions().count();
		for (u16 pid = 0; pid < pc; ++pid)
		{
			bool skip = true;
			for (const shared_str& bone_part : anm->m_bone_parts)
			{
				if (ka->partitions().part(pid).Name == bone_part)
				{
					skip = false;
					break;
				}
			}

			if (skip)
			{
				continue;
			}

			CBlend* B = ka->PlayCycle(pid, M2, bMixIn);
			R_ASSERT(B);
			B->speed *= anm->m_anim_speed;
		}

		m_model->CalculateBones_Invalidate();
	}
}

void attachable_hud_item::PlayRetargetBase(const shared_str& alias)
{
	player_hud_motion* base = m_hand_motions.find_motion(alias);
	if (base == nullptr || base->m_animations.empty() || g_player_hud->GetModel() == nullptr)
	{
		return;
	}

	const motion_descr& M = base->m_animations[0];
	g_player_hud->CycleFade = IdleFade;
	g_player_hud->CycleMode = {};
	g_player_hud->CycleMode.Type = EHudCycleMode::KeepBase;

	const CMotionDef* md = nullptr;
	g_player_hud->anim_play(m_attach_place_idx, M.mid, false, md, 1.f, nullptr);
	if (m_model->dcast_PKinematicsAnimated() != nullptr)
	{
		anim_play(base->m_base_name != base->m_additional_name ? base->m_additional_name : M.name, eNoMix, 1.f, base);
	}

	g_player_hud->CycleFade = 0.f;
	g_player_hud->CycleMode = {};
}

void attachable_hud_item::UpdateAttachPose()
{
	shared_str pose;
	IkMark = HudAttachMarks::Ik;
	FingersMark = HudAttachMarks::Fingers;
	if (m_parent_hud_item != nullptr)
	{
		for (const auto& [sect, attachment] : m_parent_hud_item->item().m_attachments)
		{
			if ((attachment.state.test(eAStateVisible) || attachment.state.test(eAStatePermanent)) && pSettings->line_exist(sect.c_str(), "anim_pose"))
			{
				pose = pSettings->r_string(sect.c_str(), "anim_pose");
				IkMark = READ_IF_EXISTS(pSettings, r_string, sect.c_str(), "ik_mark", HudAttachMarks::Ik);
				FingersMark = READ_IF_EXISTS(pSettings, r_string, sect.c_str(), "ik_fingers_mark", HudAttachMarks::Fingers);
				break;
			}
		}
	}

	if (pose == AttachPose)
	{
		return;
	}

	AttachPose = pose;
	AttachPoseReference = nullptr;
	m_hand_motions.PoseOverrides.clear();
	if (pose.size() != 0 && m_hand_motions.find_motion(pose) != nullptr)
	{
		if (const player_hud_motion* base = m_hand_motions.find_motion("anm_idle"); base != nullptr && !base->m_animations.empty())
		{
			AttachPoseReference = base->m_animations[0].name;
		}

		for (const char* idle : { "anm_idle", "anm_idle_aim", "anm_idle_moving", "anm_idle_sprint" })
		{
			if (m_hand_motions.find_motion(idle) != nullptr)
			{
				m_hand_motions.PoseOverrides[idle] = pose;
			}
		}
	}

	LeftHandAttachRefTried = false;
	LeftHandAttachRefValid = false;
	FingerGripPoseMotion = nullptr;
}

u32 attachable_hud_item::anim_play(const shared_str& anm_name_b, EHudMixType bMixIn, const CMotionDef*& md, u8& rnd_idx, bool disable_random)
{
	R_ASSERT				(strstr(anm_name_b.c_str(),"anm_")==anm_name_b.c_str());

	UpdateAttachPose();

	player_hud_motion* anm	= m_hand_motions.find_motion(anm_name_b);
	R_ASSERT2(anm, make_string<const char*>("model [%s] has no motion alias defined [%s]", m_sect_name.c_str(), anm_name_b.c_str()));
	R_ASSERT2(anm->m_animations.size(), make_string<const char*>("model [%s] has no motion defined in motion_alias [%s]", m_visual_name.c_str(), anm_name_b.c_str()));

	rnd_idx					= disable_random ? (u8)0 : (u8)Random.randI(anm->m_animations.size()) ;
	const motion_descr& M	= anm->m_animations[ rnd_idx ];

	if (anm->m_animations.size() == 1)
	{
		rnd_idx = u8(-1);
	}

	float speed = CalcMotionSpeed(anm_name_b) * anm->m_anim_speed;

	bool need_mix_hands = bMixIn >= EHudMixType::eMixHands;

	const bool idle = strstr(anm_name_b.c_str(), "anm_idle") == anm_name_b.c_str();
	const bool rig = CCameraRig::Enabled();
	shared_str retarget_reference;
	MotionID HandsMotion = M.mid;
	const bool Layer = RetargetLayer(anm_name_b, HandsMotion, retarget_reference);
	const bool retarget = Layer || RetargetMode(anm_name_b, retarget_reference);
	if (Layer && retarget_reference.size() == 0)
	{
		retarget_reference = anm->m_base_name;
	}

	if (retarget)
	{
		PlayRetargetBase(Layer ? anm_name_b : shared_str("anm_idle"));
	}

	g_player_hud->CycleFade = idle && rig ? IdleFade : 0.f;

	const bool additive = rig && (retarget || hud_motion_listed(AdditiveMotions, anm_name_b));
	SHudCycleMode& mode = g_player_hud->CycleMode;
	mode.Type = additive ? EHudCycleMode::Additive : AdditivePlaying && idle ? EHudCycleMode::KeepBase : EHudCycleMode::Replace;
	mode.BlendIn = AdditiveBlendIn;
	mode.BlendOut = AdditiveBlendOut;
	mode.Loop = additive && idle && strstr(anm_name_b.c_str(), "_start") == nullptr && strstr(anm_name_b.c_str(), "_end") == nullptr;
	mode.FadeAdditive = !additive && AdditivePlaying;
	mode.Reference = MotionID();
	const shared_str& reference = retarget ? retarget_reference : AdditiveReference;
	if (additive && reference.size() != 0 && !m_model_combined && g_player_hud->GetModel() != nullptr)
	{
		mode.Reference = g_player_hud->GetModel()->ID_Cycle_Safe(reference);
	}

	AdditivePlaying = additive;
	RetargetPlaying = retarget;
	RetargetLayerPlaying = Layer && additive;
	if (retarget)
	{
		RetargetMotion = HandsMotion;
		RetargetReferenceMotion = mode.Reference.valid() ? mode.Reference : HandsMotion;
	}
	LeftHandFree = rig && hud_motion_listed(LeftHandFreeMotions, anm_name_b);

	IKinematicsAnimated* kaModel = m_model_combined ? m_model->dcast_PKinematicsAnimated() : nullptr;
	u32 ret = g_player_hud->anim_play(m_attach_place_idx, HandsMotion, need_mix_hands, md, speed, kaModel);
	if(m_model->dcast_PKinematicsAnimated() != nullptr)
	{
		shared_str item_anm_name;
		if(anm->m_base_name!=anm->m_additional_name)
			item_anm_name = anm->m_additional_name;
		else
			item_anm_name = M.name;

		anim_play(item_anm_name, bMixIn, speed, anm);
	}

	g_player_hud->CycleFade = 0.f;
	g_player_hud->CycleMode = {};

	R_ASSERT2		(m_parent_hud_item, "parent hud item is nullptr");
	CPhysicItem&	parent_object = m_parent_hud_item->object();

	if (IsGameTypeSingle() && parent_object.H_Parent() == Level().CurrentControlEntity())
	{
		CActor* current_actor = static_cast<CActor*>(Level().CurrentControlEntity());
		VERIFY(current_actor);

		if (m_attach_place_idx == 1 && g_player_hud->attached_item(0) == nullptr || m_attach_place_idx == 0)
		{
			string_path ce_path;
			string_path anm_name;
			xr_strconcat(anm_name, "camera_effects\\weapon\\", M.name.c_str(), ".anm");
			if (CCameraRig::Enabled())
			{
				if (!current_actor->Cameras().Rig().PlayAnim(anm_name, crcMotion, speed, false))
				{
					current_actor->Cameras().Rig().StopChannel(crcMotion);
				}
			}
			else if (FS.exist(ce_path, "$game_anims$", anm_name))
			{
				CAnimatorCamEffector* e = new CAnimatorCamEffector();
				e->SetType(ECamEffectorType(Random.randI(29999, 31999)));
				e->SetHudAffect(false);
				e->SetCyclic(false);
				e->Start(anm_name);
				current_actor->Cameras().AddCamEffector(e);
			}
		}
	}
	return ret;
}

void attachable_hud_item::AddOffsets(weapon_inertion::base_params& base, Fvector& pos, Fvector& rot, float koef)
{
	Fvector tmp = zero_vel;

	tmp = base.position;
	tmp.mul(koef);
	pos.add(tmp);

	tmp = base.rotation;
	tmp.mul(koef);
	rot.add(tmp);
}

void attachable_hud_item::AddSuicideOffset(weapon_inertion& inertion_params, const shared_str& section, Fvector& pos, Fvector& rot)
{
	if (READ_IF_EXISTS(pSettings, r_bool, section, "prohibit_suicide", false))
		return;

	if (inertion_params.no_other_hud_moving_while_suicide)
	{
		rot.set(zero_vel);
		pos.set(zero_vel);
	}

	AddOffsets(inertion_params.move_suicide_offset, pos, rot);

}

void attachable_hud_item::GetCurrentTargetOffset_aim(weapon_inertion& inertion_params, Fvector& pos, Fvector& rot, float& factor, u32& real)
{
	pos.set(zero_vel);
	rot.set(zero_vel);
	factor = 1.0f;

	float koef = 1.0f;

	if ((real & mcCrouch) && (real & mcAccel))
	{
		koef = inertion_params.aim_move_slow_crouch_factor;
	}
	else if (real & mcCrouch)
	{
		koef = inertion_params.aim_move_crouch_factor;
	}
	else if (real & mcAccel)
	{
		koef = inertion_params.aim_move_slow_factor;
	}

	if (tocrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_to_crouch_offset, pos, rot, koef);
	}

	if (fromcrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_from_crouch_offset, pos, rot, koef);
	}

	if (toslowcrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_to_slow_crouch_offset, pos, rot, koef);
	}

	if (fromslowcrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_from_slow_crouch_offset, pos, rot, koef);
	}

	if (torlookout_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_to_rlookout_offset, pos, rot, koef);
	}

	if (fromrlookout_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_from_rlookout_offset, pos, rot, koef);
	}

	if (tollookout_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_to_llookout_offset, pos, rot, koef);
	}

	if (fromllookout_time_remains > 0)
	{
		AddOffsets(inertion_params.aim_move_from_llookout_offset, pos, rot, koef);
	}

	if ((real & mcRLookout) && !(real & mcLLookout))
	{
		AddOffsets(inertion_params.aim_move_rlookout_offset, pos, rot, koef);
		factor = inertion_params.move_rlookout_offset_speed_factor;
	}

	if ((real & mcLLookout) && !(real & mcRLookout))
	{
		AddOffsets(inertion_params.aim_move_llookout_offset, pos, rot, koef);
		factor = inertion_params.move_llookout_offset_speed_factor;
	}

	if ((real & mcJump) && !(real & mcFall) && !(real & mcLanding) && !(real & mcLanding2))
	{
		AddOffsets(inertion_params.aim_move_jump_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcFall) && !(real & mcJump) && !(real & mcLanding) && !(real & mcLanding2))
	{
		AddOffsets(inertion_params.aim_move_fall_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcLanding) && !(real & mcJump) && !(real & mcFall) && !(real & mcLanding2))
	{
		AddOffsets(inertion_params.aim_move_landing_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcLanding2) && !(real & mcJump) && !(real & mcFall) && !(real & mcLanding))
	{
		AddOffsets(inertion_params.aim_move_landing2_offset, pos, rot, koef);
		factor = 1.0f;
	}
}

void attachable_hud_item::GetCurrentTargetOffset(weapon_inertion& inertion_params, Fvector& pos, Fvector& rot, float& factor, u32& real)
{
	factor = inertion_params.move_stabilize_factor;

	pos.set(zero_vel);
	rot.set(zero_vel);

	float koef = 1.0f;

	if ((real & mcCrouch) && (real & mcAccel))
	{
		koef = inertion_params.move_slow_crouch_factor;
	}
	else if (real & mcCrouch)
	{
		koef = inertion_params.move_crouch_factor;
	}
	else if (real & mcAccel)
	{
		koef = inertion_params.move_slow_factor;
	}

	if (tocrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.move_to_crouch_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromcrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.move_from_crouch_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (toslowcrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.move_to_slow_crouch_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromslowcrouch_time_remains > 0)
	{
		AddOffsets(inertion_params.move_from_slow_crouch_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (torlookout_time_remains > 0)
	{
		AddOffsets(inertion_params.move_to_rlookout_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromrlookout_time_remains > 0)
	{
		AddOffsets(inertion_params.move_from_rlookout_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (tollookout_time_remains > 0)
	{
		AddOffsets(inertion_params.move_to_llookout_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromllookout_time_remains > 0)
	{
		AddOffsets(inertion_params.move_from_llookout_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcRLookout) && !(real & mcLLookout))
	{
		AddOffsets(inertion_params.move_rlookout_offset, pos, rot, koef);
		factor = inertion_params.move_rlookout_offset_speed_factor;
	}

	if ((real & mcLLookout) && !(real & mcRLookout))
	{
		AddOffsets(inertion_params.move_llookout_offset, pos, rot, koef);
		factor = inertion_params.move_llookout_offset_speed_factor;
	}

	if ((real & mcLStrafe) && !(real & mcRStrafe))
	{
		AddOffsets(inertion_params.move_left_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcRStrafe) && !(real & mcLStrafe))
	{
		AddOffsets(inertion_params.move_right_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcFwd) && !(real & mcBack))
	{
		AddOffsets(inertion_params.move_forward_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcBack) && !(real & mcFwd))
	{
		AddOffsets(inertion_params.move_back_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcCrouch) && !(real & mcAccel))
	{
		AddOffsets(inertion_params.move_crouch_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcCrouch) && (real & mcAccel))
	{
		AddOffsets(inertion_params.move_slow_crouch_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcJump) && !(real & mcFall) && !(real & mcLanding) && !(real & mcLanding2))
	{
		AddOffsets(inertion_params.move_jump_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcFall) && !(real & mcJump) && !(real & mcLanding) && !(real & mcLanding2))
	{
		AddOffsets(inertion_params.move_fall_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcLanding) && !(real & mcJump) && !(real & mcFall) && !(real & mcLanding2))
	{
		AddOffsets(inertion_params.move_landing_offset, pos, rot, koef);
		factor = 1.0f;
	}

	if ((real & mcLanding2) && !(real & mcJump) && !(real & mcFall) && !(real & mcLanding))
	{
		AddOffsets(inertion_params.move_landing2_offset, pos, rot, koef);
		factor = 1.0f;
	}
}

void attachable_hud_item::UpdateInertion(u32 delta, CActor* actor)
{
	CHudItem* itm = m_parent_hud_item;
	CHudItem* det = m_attach_place_idx == 1 ? nullptr : (m_parent->attached_item(1) != nullptr) ? m_parent->attached_item(1)->m_parent_hud_item : nullptr;

	if (itm == nullptr)
	{
		return;
	}

	time_accumulator += delta;

	shared_str section = itm->HudSection();
	auto& current_params = m_measures.m_weapon_inertion;

	u32 wishful = actor->GetMovementState(eWishful);
	u32 real = actor->GetMovementState(eReal);

	if ((wishful & mcCrouch) && !(real & mcCrouch))
	{
		tocrouch_time_remains = current_params.to_crouch_time;
		fromcrouch_time_remains = 0;
	}
	else if (!(wishful & mcCrouch) && (real & mcCrouch))
	{
		fromcrouch_time_remains = current_params.from_crouch_time;
		tocrouch_time_remains = 0;
	}

	if ((wishful & mcCrouch) && (wishful & mcAccel) && !(real & mcAccel))
	{
		toslowcrouch_time_remains = current_params.to_slow_crouch_time;
		fromslowcrouch_time_remains = 0;
	}
	else if ((wishful & mcCrouch) && !(wishful & mcAccel) && (real & mcAccel))
	{
		fromslowcrouch_time_remains = current_params.from_slow_crouch_time;
		toslowcrouch_time_remains = 0;
	}

	if ((wishful & mcRLookout) && !(real & mcRLookout))
	{
		torlookout_time_remains = current_params.to_llookout_time;
		fromrlookout_time_remains = 0;
	}
	else if (!(wishful & mcRLookout) && (real & mcRLookout))
	{
		fromrlookout_time_remains = current_params.from_rlookout_time;
		torlookout_time_remains = 0;
	}

	if ((wishful & mcLLookout) && !(real & mcLLookout))
	{
		tollookout_time_remains = current_params.to_llookout_time;
		fromllookout_time_remains = 0;
	}
	else if (!(wishful & mcLLookout) && (real & mcLLookout))
	{
		fromllookout_time_remains = current_params.from_llookout_time;
		tollookout_time_remains = 0;
	}

	Fvector pos = m_measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
	Fvector rot = m_measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];

	Fvector targetpos = zero_vel;
	Fvector targetrot = zero_vel;

	float factor = 1.0f;

	bool IsZooming = m_attach_place_idx == 1 && static_cast<CCustomDevice*>(m_parent_hud_item)->IsZoomed()
		|| ((itm->WpnCanShoot() || itm->cast_weapon_binoculars() != nullptr) && (static_cast<CWeapon*>(itm)->IsZoomed() || static_cast<CWeapon*>(itm)->m_bIsAimStarted));

	bool ActorVisForContr = actor->CheckActorVisibilityForController();

	if (itm->GetState() == CHUDState::eHiding || det != nullptr && det->GetState() == CHUDState::eHiding)
	{
		factor = current_params.move_weaponhide_factor;
	}
	else if (IsZooming)
	{
		GetCurrentTargetOffset_aim(current_params, targetpos, targetrot, factor, real);
		factor = current_params.move_unzoom_factor;
	}
	else
	{
		GetCurrentTargetOffset(current_params, targetpos, targetrot, factor, real);
		if (actor->SuicideNow && ActorVisForContr)
		{
			AddSuicideOffset(current_params, section, targetpos, targetrot);
		}
	}

	const float StepTime = delta * 0.001f;
	const bool Airborne = (real & (mcJump | mcFall | mcLanding | mcLanding2)) != 0;
	if (Airborne)
	{
		AirTime += StepTime;
	}
	else
	{
		if (AirTime > 0.3f)
		{
			LandingRecoverTime = 0.f;
		}

		AirTime = 0.f;
		LandingRecoverTime += StepTime;
	}

	constexpr float LandingRecover = 0.6f;
	if (CCameraRig::Enabled() && !Airborne && LandingRecoverTime < LandingRecover && itm->GetState() != CHUDState::eHiding)
	{
		const float Fade = hud_smooth(std::clamp((LandingRecoverTime - LandingRecover * 0.5f) / (LandingRecover * 0.5f), 0.f, 1.f));
		factor = lerp(factor, std::max(factor, current_params.move_stabilize_factor), 1.f - Fade);
	}

	targetpos.add(pos);
	targetrot.add(rot);

	float speed_pos = 0.0f;
	float speed_rot = 0.0f;

	if (!actor->SuicideNow && !actor->PlanningSuicide)
	{
		speed_rot = current_params.move_speed_rot * factor / 100.0f;
		speed_pos = current_params.move_speed_pos * factor / 100.0f;
	}
	else
	{
		speed_rot = current_params.move_suicide_speed_rot;
		speed_pos = current_params.move_suicide_speed_pos;
	}

	const SHudJitterParams& jitter = itm->GetCurJitterParams();

	const bool spring = CCameraRig::Enabled() && !actor->SuicideNow && !actor->PlanningSuicide;
	const auto spring_step = [this](Fvector& offset, Fvector& velocity, float speed)
	{
		constexpr float step = 0.008f;
		const float omega = std::min(speed / step * CCameraRig::MassSpeed(HudMass), 1.5f / step);
		Fvector moved = { 0.f, 0.f, 0.f };
		HudSpring::Step(moved, velocity, offset, omega, InertiaDamping, step);
		offset = moved;
	};

	if (!spring)
	{
		InertiaVelPos.set(0.f, 0.f, 0.f);
		InertiaVelRot.set(0.f, 0.f, 0.f);
	}

	const Fquaternion target_rotation = HudRotation::FromEuler(targetrot);

	while (time_accumulator > 8)
	{
		pos = targetpos;

		Fvector cur_pos = hands_attach_pos();
		Fquaternion cur_rotation = HudRotation::FromEuler(hands_attach_rot());

		pos.sub(cur_pos);
		rot = HudRotation::Delta(cur_rotation, target_rotation);

		if (actor->SuicideNow)
		{
			if (pos.magnitude() > speed_pos)
			{
				pos.set_length(speed_pos);
			}
		
			if (rot.magnitude() > speed_rot)
			{
				rot.set_length(speed_rot);
			}
		}
		else if (spring)
		{
			spring_step(pos, InertiaVelPos, speed_pos);
			spring_step(rot, InertiaVelRot, speed_rot);
		}
		else
		{
			if (pos.magnitude() > 0.0001f)
			{
				pos.mul(speed_pos);
			}

			if (rot.magnitude() > 0.0001f)
			{
				rot.mul(speed_rot);
			}
		}

		cur_pos.add(pos);
		cur_rotation = HudRotation::Apply(rot, cur_rotation);

		if (actor->IsHandJitter())
		{
			pos.x = ::Random.randF(0.0f, 1000.0f) - 500.0f;
			pos.y = ::Random.randF(0.0f, 500.0f) - 250.0f;
			pos.z = ::Random.randF(0.0f, 1000.0f) - 500.0f;
			pos.set_length(jitter.pos_amplitude * actor->GetHandJitterScale(itm));
			cur_pos.add(pos);

			rot.x = ::Random.randF(0.0f, 1000.f) - 500.0f;
			rot.y = ::Random.randF(0.0f, 1000.f) - 500.0f;
			rot.z = ::Random.randF(0.0f, 1000.f) - 500.0f;
			rot.set_length(jitter.rot_amplitude * actor->GetHandJitterScale(itm));
			cur_rotation = HudRotation::Apply(rot, cur_rotation);
		}

		const Fvector cur_rot = HudRotation::ToEuler(cur_rotation);

		cur_pos.sub(m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos]);
		m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos].add(cur_pos);
		m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisRot].set(cur_rot);

		time_accumulator -= 8;
	}

	if (actor->SuicideNow && ActorVisForContr && !(itm->HudDesc().ProhibitSuicide || itm->cast_weapon() && itm->cast_weapon()->SuicideByAnimation))
	{
		pos = hands_attach_pos();
		rot = HudRotation::Delta(target_rotation, HudRotation::FromEuler(hands_attach_rot()));

		pos.sub(targetpos);
	
		if (pos.magnitude() < jitter.pos_amplitude * 2.0f && rot.magnitude() < jitter.rot_amplitude * 2.0f)
		{
			actor->DoSuicideShot();
		}
	}

	fromcrouch_time_remains = (fromcrouch_time_remains > delta) ? fromcrouch_time_remains - delta : 0;
	tocrouch_time_remains = (tocrouch_time_remains > delta) ? tocrouch_time_remains - delta : 0;
	fromslowcrouch_time_remains = (fromslowcrouch_time_remains > delta) ? fromslowcrouch_time_remains - delta : 0;
	toslowcrouch_time_remains = (toslowcrouch_time_remains > delta) ? toslowcrouch_time_remains - delta : 0;

	fromrlookout_time_remains = (fromrlookout_time_remains > delta) ? fromrlookout_time_remains - delta : 0;
	torlookout_time_remains = (torlookout_time_remains > delta) ? torlookout_time_remains - delta : 0;

	fromllookout_time_remains = (fromllookout_time_remains > delta) ? fromllookout_time_remains - delta : 0;
	tollookout_time_remains = (tollookout_time_remains > delta) ? tollookout_time_remains - delta : 0;
}

player_hud::player_hud(bool invert)
{
	m_model = nullptr;
	m_attached_items[0] = nullptr;
	m_attached_items[1] = nullptr;
	m_transform.identity();
	m_transformL.identity();
	m_binverted = invert;
	m_blocked_part_idx = u16(-1);
	m_bhands_visible = false;
	m_legs_model = nullptr;
	m_watch_device = new CWatchDevice();

	m_bone_callback_params.emplace(r_finger0,  new BoneCallbackParams());
	m_bone_callback_params.emplace(r_finger01, new BoneCallbackParams());
	m_bone_callback_params.emplace(r_finger02, new BoneCallbackParams());

	if (!pSettings->section_exist("hud_movement_layers"))
	{
		return;
	}

	shared_str layer_name;

	for (u8 i = 0; i < EMovementLayers::eLayersCount; i++)
	{
		layer_name.printf("movement_layer_%d", (EMovementLayers)i);
		if (pSettings->line_exist("hud_movement_layers", layer_name))
		{
			const char* layer_line = pSettings->r_string("hud_movement_layers", *layer_name);

			movement_layer* anm = new movement_layer();

			m_default_movement_layers_params[i].Load("hud_movement_layers", layer_name, false);

			anm->Load(m_default_movement_layers_params[i].camera_name);
			anm->speed = m_default_movement_layers_params[i].speed_power.x;
			anm->power = m_default_movement_layers_params[i].speed_power.y;
			anm->blend_factors = m_default_movement_layers_params[i].blend_params;

			m_movement_layers[i] = anm;
		}
	}
}

void player_hud::destroy_watches()
{
	m_watch_device->Unbind();

	if (m_watches_model)
	{
		IRenderVisual* watches_visual = m_watches_model->dcast_RenderVisual();
		::Render->model_Delete(watches_visual);
		m_watches_model = nullptr;
		m_watches_bone = BI_NONE;
	}
}

player_hud::~player_hud()
{
	destroy_watches();
	xr_delete(m_watch_device);

	if (m_model)
	{
		IRenderVisual* v = m_model->dcast_RenderVisual();
		::Render->model_Delete(v);
		m_model = nullptr;
	}

	xr_vector<attachable_hud_item*>::iterator it	= m_pool.begin();
	xr_vector<attachable_hud_item*>::iterator it_e	= m_pool.end();
	for(;it!=it_e;++it)
	{
		attachable_hud_item* a	= *it;
		xr_delete				(a);
	}
	m_pool.clear				();

	xr_delete(m_animator_item);

	delete_data(m_movement_layers);
	delete_data(m_script_layers);
	delete_data(m_bone_callback_params);
}

void player_hud::load(const shared_str& player_hud_sect)
{
	if (player_hud_sect == m_sect_name)
	{
		return;
	}

	FingerGripBones.clear();
	ArmRetargetMotion = MotionID();
	for (attachable_hud_item* item : m_pool)
	{
		item->FingerGripPoseMotion = nullptr;
	}

	if (!m_need_reload)
	{
		return;
	}

	bool b_reload = (m_model != nullptr);

	if(m_model)
	{
		IRenderVisual* v = m_model->dcast_RenderVisual();
		::Render->model_Delete(v);
	}

	if(m_legs_model)
	{
		IRenderVisual* v = m_legs_model->dcast_RenderVisual();
		::Render->model_Delete(v);

		m_legs_model = nullptr;
	}

	m_sect_name = player_hud_sect;

	const shared_str& model_name = READ_IF_EXISTS(pSettings, r_string, player_hud_sect, "visual", nullptr);
	
	if (model_name.size())
	{
		auto CreatedModel = ::Render->model_Create(model_name.c_str());
		m_model = dynamic_cast<IKinematicsAnimated*>(CreatedModel);
	}

	if (m_model)
	{
		m_model->LL_SetInertialization(CCameraRig::HudInertialization());
		m_model->LL_SetSmoothBlend(CCameraRig::Enabled());
	}

	if (m_model)
	{
		u16 bone_r_finger0 = m_model->dcast_PKinematics()->LL_BoneID("r_finger0");
		u16 bone_r_finger01 = m_model->dcast_PKinematics()->LL_BoneID("r_finger01");
		u16 bone_r_finger02 = m_model->dcast_PKinematics()->LL_BoneID("r_finger02");
	
		if (bone_r_finger0 != BI_NONE && bone_r_finger01 != BI_NONE && bone_r_finger02 != BI_NONE)
		{
			m_model->dcast_PKinematics()->LL_GetBoneInstance(bone_r_finger0).set_callback(bctCustom, FingerCallback, m_bone_callback_params[r_finger0]);
			m_model->dcast_PKinematics()->LL_GetBoneInstance(bone_r_finger01).set_callback(bctCustom, FingerCallback, m_bone_callback_params[r_finger01]);
			m_model->dcast_PKinematics()->LL_GetBoneInstance(bone_r_finger02).set_callback(bctCustom, FingerCallback, m_bone_callback_params[r_finger02]);
		}
	}

	auto pathOmfs = EngineExternal().GetPlayerHudOmfAdditional();
	if (m_model && pathOmfs && pathOmfs[0])
	{
		string_path nm = {};
		for (int i = 0, n = _GetItemCount(pathOmfs); i < n; ++i)
		{
			auto path = _GetItem(pathOmfs, i, nm);
			m_model->append_motion_from_path(model_name.c_str(), path);
		}
	}

	if(pSettings->line_exist(player_hud_sect, "legs_visual")) {
		auto model_name = pSettings->r_string(player_hud_sect, "legs_visual");
		m_legs_model = PKinematics(::Render->model_Create(model_name));
	}

	destroy_watches();
	m_watch_device->Load("watch");

	if (pSettings->line_exist(player_hud_sect, "visual_watches"))
	{
		auto model_name = pSettings->r_string(player_hud_sect, "visual_watches");
		m_watches_model = PKinematics(::Render->model_Create(model_name));

		m_watches_pos = zero_vel;
		m_watches_rot = zero_vel;
		m_watches_scale = 1.0f;
		pSettings->read_if_exists<Fvector>(m_watches_pos, player_hud_sect, "watches_pos");
		pSettings->read_if_exists<Fvector>(m_watches_rot, player_hud_sect, "watches_rot");
		pSettings->read_if_exists<float>(m_watches_scale, player_hud_sect, "watches_scale");

		if (m_model)
		{
			m_watches_bone = m_model->dcast_PKinematics()->LL_BoneID(pSettings->r_string(player_hud_sect, "watches_bone"));
		}
		m_watch_device->BindModel(m_watches_model);
	}

	if (m_model)
	{
		u16 l_arm = m_model->dcast_PKinematics()->LL_BoneID("l_clavicle");
		if (l_arm != BI_NONE)
		{
			m_model->dcast_PKinematics()->LL_GetBoneInstance(l_arm).set_callback(bctCustom, [](CBoneInstance* B) {g_player_hud->LeftArmCallback(B); }, NULL);
		}

		auto& _sect = pSettings->r_section(player_hud_sect);
		auto _b = _sect.Data.begin();
		auto _e = _sect.Data.end();

		m_ancors.clear();

		const auto load_chain = [&](const char* key, u16* chain)
		{
			string128 bone;
			const char* names = READ_IF_EXISTS(pSettings, r_string, player_hud_sect, key, READ_IF_EXISTS(pSettings, r_string, "camera_rig", key, nullptr));
			const bool valid = names != nullptr && _GetItemCount(names) == 3;
			for (int i = 0; i < 3; i++)
			{
				chain[i] = valid ? m_model->dcast_PKinematics()->LL_BoneID(_GetItem(names, i, bone)) : BI_NONE;
			}
		};

		load_chain("left_hand_ik_chain", LeftIkChain);
		load_chain("right_hand_ik_chain", RightIkChain);

		for (; _b != _e; ++_b)
		{
			if (strstr(_b->first.c_str(), "ancor_") == _b->first.c_str())
			{
				const shared_str& _bone = _b->second;
				m_ancors.push_back(m_model->dcast_PKinematics()->LL_BoneID(_bone));
			}
		}

		if (!b_reload) {
			m_model->PlayCycle("hand_idle_doun");
		}
		else
		{
			if (m_attached_items[1])
			{
				m_attached_items[1]->m_parent_hud_item->on_a_hud_attach();
			}

			if (m_attached_items[0])
			{
				m_attached_items[0]->m_parent_hud_item->on_a_hud_attach();
			}
		}

		m_model->dcast_PKinematics()->CalculateBones_Invalidate();
		m_model->dcast_PKinematics()->CalculateBones(TRUE);
	}
	if(m_legs_model) {
		m_legs_model->CalculateBones_Invalidate();
		m_legs_model->CalculateBones(true);
	}

	if(Actor()) {
		float m_fLegs_shift = READ_IF_EXISTS(pSettings, r_float, "actor_hud", "legs_shift_delta", -0.55f);
		Actor()->m_fLegs_shift = READ_IF_EXISTS(pSettings, r_float, player_hud_sect, "legs_shift_delta", m_fLegs_shift);
	}

	if (m_watches_model)
	{
		m_watches_model->CalculateBones_Invalidate();
		m_watches_model->CalculateBones(true);
	}
}

bool player_hud::render_item_ui_query()
{
	bool res = false;
	if(m_attached_items[0])
		res |= m_attached_items[0]->render_item_ui_query();

	if(m_attached_items[1])
		res |= m_attached_items[1]->render_item_ui_query();

	if (m_watches_model && need_render_hands())
		res |= m_watch_device->RenderUIQuery();

	return res;
}

void player_hud::render_item_ui()
{
	if(m_attached_items[0] && m_attached_items[0]->render_item_ui_query())
		m_attached_items[0]->render_item_ui();

	if(m_attached_items[1] && m_attached_items[1]->render_item_ui_query())
		m_attached_items[1]->render_item_ui();

	if (m_watches_model && need_render_hands() && m_watch_device->RenderUIQuery())
		m_watch_device->RenderUI(m_watches_transform);
}

bool player_hud::need_render_hands()
{
	const bool b_r0 = (m_attached_items[0] && m_attached_items[0]->need_renderable());
	const bool b_r1 = (m_attached_items[1] && m_attached_items[1]->need_renderable());
	const bool animatorPlaying = m_animator_item && m_animator_item->IsPlaying;

	return (b_r0 && !m_attached_items[0]->m_model_combined) || b_r1 || animatorPlaying || m_bhands_visible;
}

void player_hud::render_hud()
{
	bool b_r0 = (m_attached_items[0] && m_attached_items[0]->need_renderable());
	bool b_r1 = (m_attached_items[1] && m_attached_items[1]->need_renderable());
	bool animatorPlaying = m_animator_item && m_animator_item->IsPlaying;

	if (need_render_hands())
	{
		if (m_model || animatorPlaying)
		{
			::Render->set_Transform(&m_transform);
			::Render->add_Visual(m_model->dcast_RenderVisual(), true);
		}

		if (m_watches_model)
		{
			::Render->set_Transform(&m_watches_transform);
			::Render->add_Visual(m_watches_model->dcast_RenderVisual(), true);
		}
	}

	if(b_r0) {
		m_attached_items[0]->render();
	}

	if(b_r1) {
		m_attached_items[1]->render();
	}

	if (m_animator_item && m_animator_item->IsPlaying)
		m_animator_item->render();

	if(m_show_legs > 0 && Actor() && m_legs_model && Actor()->Holder() == nullptr && Actor()->active_cam() == eacFirstEye)
	{
		bool isClimb = Actor()->GetMovementState(ACTOR_DEFS::EMovementStates::eReal) & mcClimb;
		if(!isClimb) 
		{
			IKinematics* actor_model = Actor()->Visual()->dcast_PKinematics();

			actor_model->CalculateBones(true);

			m_legs_model->CalculateBones_Invalidate();
			m_legs_model->CalculateBones(true);

			u16 legs_bones_cnt = m_legs_model->LL_BoneCount();
			if(legs_bones_cnt == actor_model->LL_BoneCount())
			{
				for(u16 i = 0; i < legs_bones_cnt; ++i)
				{
					auto& BoneInstance = m_legs_model->LL_GetBoneInstance(i);

					BoneInstance.mTransform.set(actor_model->LL_GetBoneInstance(i).mTransform);
					BoneInstance.mRenderTransform.mul_43(BoneInstance.mTransform, m_legs_model->LL_GetData(i).m2b_transform);
				}
			}
			else
			{
				for(auto& [bonename, ID] : *m_legs_model->LL_Bones())
				{
					auto BoneID = actor_model->LL_BoneID(bonename);

					if (BoneID != BI_NONE) 
					{
						auto& BoneInstance = m_legs_model->LL_GetBoneInstance(ID);

						BoneInstance.mTransform.set(actor_model->LL_GetBoneInstance(BoneID).mTransform);
						BoneInstance.mRenderTransform.mul_43(BoneInstance.mTransform, m_legs_model->LL_GetData(ID).m2b_transform);
					}
				}
			}

			if (auto BoneID = m_legs_model->LL_BoneID("bip01_spine"); BoneID != BI_NONE)
			{
				auto& BoneInstance = m_legs_model->LL_GetData(BoneID);
				m_legs_model->Bone_Calculate(&BoneInstance, &m_legs_model->LL_GetTransform(BoneInstance.GetParentID()));
			}

			auto bHud = ::Render->get_HUD();
			::Render->set_HUD(false);

			::Render->set_Transform(&Actor()->XFORM());
			::Render->add_Visual(m_legs_model->dcast_RenderVisual(), true);

			::Render->set_HUD(bHud);
		}
	}
}

#include "../xrEngine/motion.h"

u32 player_hud::motion_length(const shared_str& anim_name, const shared_str& hud_name, const CMotionDef*& md)
{
	float speed						= CalcMotionSpeed(anim_name);
	attachable_hud_item* pi = create_hud_item(hud_name);
	player_hud_motion*	pm			= pi->m_hand_motions.find_motion(anim_name);
	if(!pm)
		return						-1; 

	R_ASSERT2						(pm, 
		make_string<const char*>("hudItem model [%s] has no motion with alias [%s]", hud_name.c_str(), anim_name.c_str())
		);

	IKinematicsAnimated* modelMotion = pi->m_model_combined ? pi->m_model->dcast_PKinematicsAnimated() : m_model;
	return motion_length(pm->m_animations[0].mid, md, speed, modelMotion);
}

u32 player_hud::motion_length(const MotionID& M, const CMotionDef*& md, float speed, IKinematicsAnimated* model)
{
	md = model->LL_GetMotionDef(M);
	VERIFY(md);
	if (md != nullptr && md->flags & esmStopAtEnd)
	{
		CMotion* motion = model->LL_GetRootMotion(M);
		return iFloor(0.5f + 1000.f * motion->GetLength() / (md->Dequantize(md->speed) * speed));
	}
	return 0;
}

const Fvector& player_hud::attach_rot() const
{
	static Fvector m_last_rot = zero_vel;

	if (m_animator_item)
	{
		return m_last_rot = m_animator_item->hands_attach_rot();
	}

	if (m_attached_items[0])
		return m_last_rot=m_attached_items[0]->hands_attach_rot();
	else
	{
		if (m_attached_items[1])
			return m_last_rot=m_attached_items[1]->hands_attach_rot();
	}
	return m_last_rot;
}

const Fvector& player_hud::attach_pos() const
{
	static Fvector m_last_pos = zero_vel;

	if (m_animator_item)
	{
		return m_last_pos = m_animator_item->hands_attach_pos();
	}

	if (m_attached_items[0])
		return m_last_pos=m_attached_items[0]->hands_attach_pos();
	else
	{
		if (m_attached_items[1])
			return m_last_pos=m_attached_items[1]->hands_attach_pos();
	}
	return m_last_pos;
}

void player_hud::LeftArmCallback(CBoneInstance* B)
{
	B->mTransform.mulA_44(m_transformL);
	B->mTransform.mulA_44(Fmatrix(m_transform).invert());
}

void angle_inertion(Fvector& c_hpb, const Fvector& t_hpb, float speed)
{
	c_hpb.x = angle_inertion(c_hpb.x, t_hpb.x, speed, PI, Device.fTimeDelta);
	c_hpb.x = angle_inertion(c_hpb.x, t_hpb.x, speed, PI, Device.fTimeDelta);
	c_hpb.x = angle_inertion(c_hpb.x, t_hpb.x, speed, PI, Device.fTimeDelta);
}

static Fmatrix scale_layer_offset(const Fmatrix& offset, float factor)
{
	Fvector rotation;
	offset.getXYZ(rotation);
	rotation.mul(factor);

	Fmatrix result;
	result.setXYZ(rotation);
	result.translate_over(Fvector().mul(offset.c, factor));
	return result;
}

void player_hud::update(const Fmatrix& cam_trans)
{
	if(!m_attached_items[0] && !m_attached_items[1] && !m_animator_item)
	{
		m_transform.set(cam_trans);
		m_transformL.set(cam_trans);
		return;
	}

	Fmatrix trans = cam_trans;
	update_inertion(trans);
	Fvector turn_offset = Fvector().sub(trans.c, cam_trans.c);

	const float turn_smooth = CCameraRig::TurnSwaySmooth();
	if (turn_smooth > 0.f)
	{
		const Fvector local = { turn_offset.dotproduct(cam_trans.i), turn_offset.dotproduct(cam_trans.j), turn_offset.dotproduct(cam_trans.k) };
		TurnOffset.lerp(TurnOffset, local, HudBlend::Smooth(turn_smooth, Device.fTimeDelta));
		turn_offset.set(0.f, 0.f, 0.f).mad(cam_trans.i, TurnOffset.x).mad(cam_trans.j, TurnOffset.y).mad(cam_trans.k, TurnOffset.z);
		trans.c.add(cam_trans.c, turn_offset);
	}

	CActor* actor = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_actor() : nullptr;
	const Fmatrix trans_turn = trans;
	if (actor != nullptr)
	{
		actor->Cameras().Rig().ApplyHud(trans, m_attached_items[0] != nullptr ? m_attached_items[0] : m_attached_items[1]);

		CWeapon* weapon = m_attached_items[0] != nullptr && m_attached_items[0]->m_parent_hud_item != nullptr ? m_attached_items[0]->m_parent_hud_item->cast_weapon() : nullptr;
		const u32 key = (actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal) & (ACTOR_DEFS::EMoveCommand::mcAnyMove | ACTOR_DEFS::EMoveCommand::mcSprint | ACTOR_DEFS::EMoveCommand::mcAccel | ACTOR_DEFS::EMoveCommand::mcCrouch)) | (weapon != nullptr && weapon->IsZoomed() ? 0x80000000 : 0);
		if (key != MovementKey)
		{
			MovementKey = key;
			UpdateMovementLayers();
		}

		UpdateSprintAdditiveChain();
		UpdateRetargetLayerWeight();
	}

	UpdateCarryPose(actor);

	const float ik_share = CCameraRig::HudIkShare();
	LayersIk = ik_share > 0.f && actor != nullptr && m_attached_items[0] != nullptr && !m_attached_items[0]->m_model_combined && m_attached_items[1] == nullptr && m_animator_item == nullptr && RightIkChain[2] != BI_NONE;

	Fmatrix trans_2 = trans;
	const Fmatrix trans_rig = trans;

	if (m_animator_item)
	{
		m_animator_item->update_hud_additional(trans);
	}

	if (m_attached_items[0])
	{
		m_attached_items[0]->update_hud_additional(trans);
	}

	if (m_attached_items[1])
	{
		m_attached_items[1]->update_hud_additional(trans_2);

		if (m_attached_items[0] == nullptr)
		{
			trans = trans_2;
		}
	}
	else
	{
		trans_2 = trans;
	}

	{
		m_attach_offsetr.setHPB(VPUSH(Fvector(attach_rot()).mul(PI / 180.0f)));
		m_attach_offsetr.c.set(attach_pos());
		m_transform.mul(trans, m_attach_offsetr);
	}

	{
		bool left_hand_active = m_attached_items[1];

		Fmatrix attach_offset;
		attach_offset.setHPB(VPUSH(Fvector(left_hand_active ? m_attached_items[1]->hands_attach_rot() : attach_rot()).mul(PI / 180.f)));
		attach_offset.c.set(left_hand_active ? m_attached_items[1]->hands_attach_pos() : attach_pos());
		m_transformL.mul(trans_2, m_attach_offsetl.set(attach_offset));
	}

	Fmatrix script_layers = Fidentity;

	UpdateSprintPose();
	SprintYield = SprintPose;
	Fmatrix movement_layers = Fidentity;

	for (script_layer* anm : m_script_layers)
	{
		if (!anm || !anm->anim || (!anm->active && anm->blend_scale == 0.0f))
		{
			continue;
		}
	
		bool need_stop = false;
	
		if (anm->active)
		{
			anm->blend_scale += anm->mix ? Device.fTimeDelta / anm->blend_factors.x : 1.0f;
		}
		else
		{
			anm->blend_scale -= anm->mix ? Device.fTimeDelta / anm->blend_factors.y : 1.0f;
		}
	
		clamp(anm->blend_scale, 0.0f, 1.0f);
	
		if (anm->blend_scale > 0.0f)
		{
			if (anm->anim->IsLooped() || anm->anim->anim_param().t_current <= anm->anim->anim_param().max_t)
			{
				anm->anim->Update(Device.fTimeDelta);
			}
			else
			{
				anm->CallEndCallback();
				need_stop = true;
			}
		}
		else
		{
			anm->CallEndCallback();
			anm->Stop(true);
			continue;
		}

		Fmatrix layer = anm->XFORM();
		if (SprintYield > 0.f)
		{
			layer = scale_layer_offset(layer, 1.f - SprintYield);
		}

		if (anm->part == 0 || anm->part == 2)
		{
			m_transform.mulB_43(layer);
			script_layers.mulB_43(layer);
		}
		
		if (anm->part == 1 || anm->part == 2)
		{
			m_transformL.mulB_43(layer);
		}

		if (need_stop)
		{
			anm->Stop(!anm->mix);
		}
	}

	if (ScriptInertiaPending)
	{
		ScriptInertiaPending = false;
		const Fmatrix gap = Fmatrix().mul_43(Fmatrix().invert(script_layers), ScriptLayersLast);
		ScriptInertiaPos.set(gap.c);
		ScriptInertiaRot.set(gap);
		ScriptInertiaTime = 0.f;
	}

	if (ScriptInertiaHalfLife > 0.f && ScriptInertiaTime < ScriptInertiaHalfLife * 8.f)
	{
		const float omega = 1.6783469f / ScriptInertiaHalfLife;
		const float t = omega * ScriptInertiaTime;
		const float weight = (1.f + t) * expf(-t);
		Fquaternion rotation;
		rotation.slerp(Fquaternion().identity(), ScriptInertiaRot, weight);
		Fmatrix gap;
		gap.rotation(rotation);
		gap.c.mul(ScriptInertiaPos, weight);
		m_transform.mulB_43(gap);
		if (m_attached_items[1] == nullptr)
		{
			m_transformL.mulB_43(gap);
		}
		script_layers.mulB_43(gap);
		ScriptInertiaTime += Device.fTimeDelta;
	}

	ScriptLayersLast = script_layers;

	bool need_blend_0 = m_attached_items[0] != nullptr && m_attached_items[0]->m_parent_hud_item->NeedMovementBlend() || m_animator_item != nullptr && m_animator_item->NeedMovementBlend();
	bool need_blend_1 = m_attached_items[1] != nullptr && m_attached_items[1]->m_parent_hud_item->NeedMovementBlend() || m_animator_item != nullptr && m_animator_item->NeedMovementBlend();

	for (movement_layer* anm : m_movement_layers)
	{
		if (anm == nullptr || anm->anim == nullptr || !anm->playing && anm->blend_scale <= 0.0f)
		{
			continue;
		}

		if (anm->playing && (need_blend_0 || need_blend_1))
		{
			anm->blend_scale += Device.fTimeDelta / anm->blend_factors.x;
		}
		else
		{
			anm->blend_scale -= Device.fTimeDelta / anm->blend_factors.y;
		}

		clamp(anm->blend_scale, 0.0f, 1.0f);

		if (anm->blend_scale > 0.0f)
		{
			anm->anim->Update(Device.fTimeDelta);

			m_transform.mulB_43(anm->XFORM());
			m_transformL.mulB_43(anm->XFORM());
			movement_layers.mulB_43(anm->XFORM());
		}
		else
		{
			if (anm->playing)
			{
				anm->Stop(true);
			}
		}
	}

	MovementLayersOffset = movement_layers;

	const Fmatrix carry_pose = CarryPoseOffset();
	m_transform.mulB_43(carry_pose);
	m_transformL.mulB_43(carry_pose);

	if (LayersIk)
	{
		const hud_item_measures& measures = m_attached_items[0]->m_measures;
		const Fvector& base_pos = measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
		const Fvector& base_rot = measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];

		const float offset_share = m_attached_items[0]->IkEnabled ? ik_share : 0.f;
		Fquaternion rigid_rotation;
		rigid_rotation.slerp(HudRotation::FromEuler(attach_rot()), HudRotation::FromEuler(base_rot), offset_share);
		Fmatrix rigid_attach;
		rigid_attach.rotation(rigid_rotation);
		rigid_attach.c.lerp(attach_pos(), base_pos, offset_share);

		const float rigid_share = 1.f - ik_share;
		Fmatrix rigid = trans_turn;
		rigid.c.mad(turn_offset, -ik_share);
		rigid.mulB_43(scale_layer_offset(Fmatrix().mul_43(Fmatrix().invert(trans_turn), trans_rig), rigid_share));

		attachable_hud_item* item = m_attached_items[0];
		const Fmatrix collision_inv = Fmatrix().invert(item->CollisionXform);
		Fmatrix look_pre, look;
		if (item->m_parent_hud_item->GetHudLookInertia(look_pre, look))
		{
			rigid.mulB_43(Fmatrix().mul_43(Fmatrix().invert(trans_rig), look_pre));
			rigid.mulB_43(scale_layer_offset(look, rigid_share));
			rigid.mulB_43(Fmatrix().mul_43(Fmatrix().invert(Fmatrix().mul_43(look_pre, look)), trans).mulB_43(collision_inv));
		}
		else
		{
			rigid.mulB_43(Fmatrix().mul_43(Fmatrix().invert(trans_rig), trans).mulB_43(collision_inv));
		}

		rigid.mulB_43(scale_layer_offset(item->CollisionXform, rigid_share));
		rigid.mulB_43(rigid_attach);
		rigid.mulB_43(scale_layer_offset(script_layers, 1.f - ik_share));
		rigid.mulB_43(scale_layer_offset(movement_layers, 1.f - ik_share));
		rigid.mulB_43(scale_layer_offset(carry_pose, 1.f - ik_share));

		LayersIkOffset.invert(rigid).mulB_43(m_transform);
		m_transform.set(rigid);

		m_transformL.set(rigid);
	}

	if ((m_model &&(m_attached_items[0] && !m_attached_items[0]->m_model_combined || m_attached_items[1])) || (m_animator_item && m_animator_item->IsPlaying))
	{
		m_model->UpdateTracks();
		m_model->dcast_PKinematics()->CalculateBones_Invalidate();
		m_model->dcast_PKinematics()->CalculateBones(true);
	}

	UpdateShotFingers(true);

	if(m_attached_items[0])
		m_attached_items[0]->update(true);

	if(m_attached_items[1])
		m_attached_items[1]->update(true);

	if (m_animator_item && m_animator_item->IsPlaying)
		m_animator_item->update(true);

	update_hands_ik(cam_trans, actor);
	UpdateShotFingers(false);

	{
		if (m_watches_bone != BI_NONE)
		{
			Fmatrix AttachBone = m_model->dcast_PKinematics()->LL_GetTransform(m_watches_bone);
			m_watches_transform.mul(m_transform, AttachBone);
			m_watches_attach_offset.setHPB(VPUSH(Fvector(m_watches_rot).mul(PI / 180.0f)));
			m_watches_attach_offset.c.set(m_watches_pos);
			m_watches_transform.mulB_43(m_watches_attach_offset);

			{
				Fmatrix m = Fidentity;
				m.scale(m_watches_scale, m_watches_scale, m_watches_scale);
				m_watches_transform.mulB_43(m);
			}
		}
	}

	m_watch_device->Update(Device.fTimeDelta);

	if (m_watches_model)
	{
		m_watches_model->CalculateBones_Invalidate();
		m_watches_model->CalculateBones(true);
		if (need_render_hands())
		{
			m_watch_device->SyncHudLights(m_watches_transform);
		}
		else
		{
			m_watch_device->TurnOffHudLights();
		}
	}
	else
	{
		m_watch_device->TurnOffHudLights();
	}
}

xr_vector<u16> player_hud::left_arm_blend_bones() const
{
	xr_vector<u16> bones;
	if (m_model == nullptr || LeftIkChain[0] == BI_NONE || LeftIkChain[1] == BI_NONE)
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	const u16 clavicle = kinematics->LL_GetData(LeftIkChain[0]).GetParentID();
	if (clavicle != BI_NONE && clavicle != kinematics->LL_GetBoneRoot())
	{
		bones.push_back(clavicle);
	}

	bones.push_back(LeftIkChain[0]);
	bones.push_back(LeftIkChain[1]);
	return bones;
}

bool attachable_hud_item::RetargetMode(const shared_str& motion, shared_str& reference) const
{
	reference = nullptr;
	if (m_model_combined || !CCameraRig::Enabled())
	{
		return false;
	}

	const auto base_name = [this](const char* alias)
	{
		for (const player_hud_motion& it : m_hand_motions.m_anims)
		{
			if (it.m_alias_name == alias)
			{
				return it.m_base_name;
			}
		}

		return shared_str();
	};

	if (!hud_motion_listed(RetargetMotions, motion))
	{
		return false;
	}

	const shared_str& source = hud_retarget_source_idle(base_name(motion.c_str()));
	const player_hud_motion* own_idle = OwnIdle();
	const shared_str own = own_idle != nullptr ? own_idle->m_base_name : shared_str();
	if (source.size() != 0 ? source != own : RetargetReference.size() != 0)
	{
		reference = source.size() != 0 ? source : RetargetReference;
		return true;
	}

	reference = own.size() != 0 ? own : AttachPoseReference;
	return AttachPoseReference.size() != 0;
}

bool attachable_hud_item::RetargetLayer(const shared_str& alias, MotionID& motion, shared_str& reference) const
{
	const auto It = RetargetLayers.find(alias);
	if (It == RetargetLayers.end() || m_model_combined || !CCameraRig::Enabled() || g_player_hud->GetModel() == nullptr || hud_motion_listed(RetargetMotions, alias))
	{
		return false;
	}

	const MotionID Layer = g_player_hud->GetModel()->ID_Cycle_Safe(It->second.Motion);
	if (!Layer.valid())
	{
		return false;
	}

	motion = Layer;
	reference = It->second.Reference;
	return true;
}

void attachable_hud_item::LoadRetargetLayers()
{
	RetargetLayers.clear();
	for (auto It = RetargetMotions.begin(); It != RetargetMotions.end();)
	{
		if (!pSettings->section_exist(*It))
		{
			++It;
			continue;
		}

		const shared_str Reference = READ_IF_EXISTS(pSettings, r_string, *It, "retarget_reference", nullptr);
		for (const CInifile::Item& Line : pSettings->r_section(*It).Data)
		{
			if (strstr(Line.first.c_str(), "anm_") == Line.first.c_str() && Line.second.size() != 0)
			{
				string128 Motion;
				RetargetLayers[Line.first] = { _GetItem(Line.second.c_str(), 0, Motion), Reference };
			}
		}

		It = RetargetMotions.erase(It);
	}
}

const player_hud_motion* attachable_hud_item::OwnIdle() const
{
	const player_hud_motion* numbered = nullptr;
	for (const player_hud_motion& it : m_hand_motions.m_anims)
	{
		if (it.m_animations.empty())
		{
			continue;
		}

		if (it.m_alias_name == "anm_idle")
		{
			return &it;
		}

		const char* name = it.m_alias_name.c_str();
		if (numbered == nullptr && strstr(name, "anm_idle_") == name && isdigit(static_cast<unsigned char>(name[9])))
		{
			numbered = &it;
		}
	}

	return numbered;
}

MotionID attachable_hud_item::AliasMotion(const char* alias) const
{
	for (const player_hud_motion& it : m_hand_motions.m_anims)
	{
		if (it.m_alias_name == alias && !it.m_animations.empty())
		{
			return it.m_animations[0].mid;
		}
	}

	return MotionID();
}

bool attachable_hud_item::SprintLayered() const
{
	if (m_model_combined || !CCameraRig::Enabled() || !AliasMotion("anm_idle_sprint").valid())
	{
		return false;
	}

	const shared_str alias = "anm_idle_sprint";
	return hud_motion_listed(RetargetMotions, alias) || hud_motion_listed(AdditiveMotions, alias);
}

shared_str attachable_hud_item::SprintLayerReference() const
{
	shared_str reference;
	RetargetMode("anm_idle_sprint", reference);
	if (reference.size() != 0)
	{
		return reference;
	}

	if (AdditiveReference.size() != 0)
	{
		return AdditiveReference;
	}

	if (const player_hud_motion* idle = OwnIdle())
	{
		return idle->m_base_name;
	}

	return reference;
}

u8 attachable_hud_item::LeftHandAttachMotionMode(const shared_str& motion) const
{
	shared_str reference;
	if (RetargetMode(motion, reference))
	{
		return 0;
	}

	if (hud_motion_listed(LeftHandAttachAnimatedArmMotions, motion))
	{
		return 2;
	}

	return hud_motion_listed(LeftHandAttachMotions, motion) ? 1 : 0;
}

static float hud_motion_amount(IKinematicsAnimated* ka, MotionID motion)
{
	float amount = 0.f;
	float total = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == 0)
		{
			total += B->Weight();
			if (B->motionID == motion)
			{
				amount += B->Weight();
			}
		}
	}

	return total > EPS ? amount / total : 0.f;
}

static float hud_motion_time(IKinematicsAnimated* ka, MotionID motion, u8 channel = 0)
{
	float time = -1.f;
	float amount = -1.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == channel && B->motionID == motion && B->blendAmount > amount)
		{
			amount = B->blendAmount;
			time = B->timeCurrent;
		}
	}

	return time;
}

static float hud_mark_time(CMotionDef* def, float motion_time)
{
	const float speed = def->Speed();
	return speed <= EPS || fsimilar(speed, 1.f) ? motion_time : motion_time / speed;
}

static float hud_mark_ramp(float delta, float blend)
{
	return blend > EPS ? std::clamp(delta / blend, 0.f, 1.f) : (delta >= 0.f ? 1.f : 0.f);
}

struct SHudMarkSpan
{
	const motion_marks::interval*	First = nullptr;
	const motion_marks::interval*	Active = nullptr;
	int								ActiveIndex = -1;
};

static SHudMarkSpan hud_mark_span(const CMotionDef* def, const char* name, float time)
{
	SHudMarkSpan span;
	for (const motion_marks& marks : def->marks)
	{
		if (xr_strcmp(marks.name, name) != 0)
		{
			continue;
		}

		for (int index = 0; index < int(marks.intervals.size()); ++index)
		{
			const motion_marks::interval& interval = marks.intervals[index];
			if (span.First == nullptr || interval.first < span.First->first)
			{
				span.First = &interval;
			}

			if (interval.first <= time && (span.Active == nullptr || interval.first > span.Active->first))
			{
				span.Active = &interval;
				span.ActiveIndex = index;
			}
		}
	}

	return span;
}

static bool hud_motion_local_pose(IKinematicsAnimated* ka, MotionID motion, float time, const xr_vector<u16>& bones, xr_vector<Fquaternion>& pose, xr_vector<Fvector>* positions = nullptr)
{
	pose.clear();
	if (positions != nullptr)
	{
		positions->clear();
	}

	IKinematics* kinematics = ka->dcast_PKinematics();
	for (u16 bone : bones)
	{
		Fmatrix bone_ms, parent_ms;
		if (!ka->LL_GetMotionBoneTransform(motion, time, bone, bone_ms) || !ka->LL_GetMotionBoneTransform(motion, time, kinematics->LL_GetData(bone).GetParentID(), parent_ms))
		{
			pose.clear();
			if (positions != nullptr)
			{
				positions->clear();
			}

			return false;
		}

		const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent_ms), bone_ms);
		pose.push_back(Fquaternion().set(local));
		if (positions != nullptr)
		{
			positions->push_back(local.c);
		}
	}

	return !pose.empty();
}

static float hud_ancor_deviation(const Fmatrix& current, const Fmatrix& reference)
{
	Fquaternion q_current, q_reference;
	q_current.set(current);
	q_reference.set(reference);
	const float dot = std::min(std::abs(q_current.x * q_reference.x + q_current.y * q_reference.y + q_current.z * q_reference.z + q_current.w * q_reference.w), 1.f);
	return std::max(current.c.distance_to(reference.c) / 0.1f, 2.f * acosf(dot) / deg2rad(30.f));
}

static MotionID hud_dominant_motion(IKinematicsAnimated* ka, u8 channel = 0)
{
	MotionID motion;
	float amount = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == channel && B->blend_state() != CBlend::eFREE_SLOT && B->blendAmount > amount)
		{
			amount = B->blendAmount;
			motion = B->motionID;
		}
	}

	return motion;
}

static float hud_motion_settle(IKinematicsAnimated* ka, const player_hud_motion_container& motions, float settle_time, bool ahead)
{
	xr_vector<MotionID> idles;
	for (const player_hud_motion& motion : motions.m_anims)
	{
		if (strstr(motion.m_alias_name.c_str(), "anm_idle") == motion.m_alias_name.c_str())
		{
			for (const motion_descr& descr : motion.m_animations)
			{
				idles.push_back(descr.mid);
			}
		}
	}

	float settled = 0.f;
	float total = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B == nullptr || B->channel != 0 || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || B->timeTotal <= EPS || std::find(idles.begin(), idles.end(), B->motionID) != idles.end())
		{
			continue;
		}

		if (B->blend_state() == CBlend::eFalloff)
		{
			settled += B->Weight();
		}
		else if (ahead)
		{
			const float speed = std::abs(B->speed) > EPS ? std::abs(B->speed) : 1.f;
			settled += B->Weight() * (1.f - hud_mark_ramp(std::max(B->timeTotal - B->timeCurrent, 0.f) / speed, settle_time));
		}

		total += B->Weight();
	}

	return total > 0.f ? std::clamp(settled / total, 0.f, 1.f) : 0.f;
}

struct SHudFingerHold
{
	MotionID	Motion;
	float		Hold = 0.f;
	float		StartHold = 0.f;
	float		StartTime = 0.f;
	float		Idle = 0.f;
};

static SHudFingerHold hud_finger_hold(IKinematicsAnimated* ka, const shared_str& mark, float blend)
{
	SHudFingerHold result;
	if (ka == nullptr)
	{
		return result;
	}

	float total = 0.f;
	float best = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B == nullptr || B->channel != 0 || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f)
		{
			continue;
		}

		total += B->Weight();

		CMotionDef* def = ka->LL_GetMotionDef(B->motionID);
		if (def == nullptr || def->marks.empty() || B->Weight() <= best)
		{
			continue;
		}

		const float time = hud_mark_time(def, B->timeCurrent);

		const SHudMarkSpan span = hud_mark_span(def, mark.c_str(), time);
		if (span.First == nullptr)
		{
			continue;
		}

		best = B->Weight();
		result.Motion = B->motionID;
		const float speed = def->Speed();
		const motion_marks::interval* interval = span.Active != nullptr ? span.Active : span.First;
		result.StartTime = speed <= EPS || fsimilar(speed, 1.f) ? interval->first : interval->first * speed;
		if (span.Active == nullptr)
		{
			result.StartHold = 1.f;
			result.Idle = 0.f;
		}
		else if (time <= interval->second)
		{
			result.StartHold = std::max(1.f - hud_mark_ramp(time - interval->first, blend), hud_mark_ramp(time - (interval->second - blend), blend));
			result.Idle = 0.f;
		}
		else
		{
			result.StartHold = 1.f;
			result.Idle = hud_mark_ramp(time - interval->second, blend);
		}
	}

	result.Hold = total > 0.f ? best / total : 0.f;
	return result;
}

static bool hud_attach_mark(IKinematicsAnimated* ka, const shared_str& mark, float blend, float& weight, MotionID& motion, bool& pre_start)
{
	if (ka == nullptr)
	{
		return false;
	}

	const CBlend* best = nullptr;
	CMotionDef* best_def = nullptr;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B == nullptr || B->channel != 0 || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || (best != nullptr && B->blendAmount <= best->blendAmount))
		{
			continue;
		}

		CMotionDef* def = ka->LL_GetMotionDef(B->motionID);
		if (def == nullptr)
		{
			continue;
		}

		if (std::any_of(def->marks.begin(), def->marks.end(), [&mark](const motion_marks& marks) { return marks.name == mark && !marks.is_empty(); }))
		{
			best = B;
			best_def = def;
		}
	}

	if (best == nullptr)
	{
		return false;
	}

	motion = best->motionID;
	pre_start = false;
	const float time = hud_mark_time(best_def, best->timeCurrent);
	CMotion* root = ka->LL_GetRootMotion(best->motionID);
	const float length = root != nullptr ? hud_mark_time(best_def, root->GetLength()) : 0.f;

	const SHudMarkSpan span = hud_mark_span(best_def, mark.c_str(), time);
	if (span.Active == nullptr)
	{
		pre_start = true;
		weight = 1.f;
	}
	else if (span.Active->second >= length - EPS_L || time < span.Active->second)
	{
		pre_start = span.Active == span.First;
		weight = pre_start ? 1.f - hud_mark_ramp(time - span.Active->first, blend) : 0.f;
	}
	else
	{
		weight = hud_mark_ramp(time - span.Active->second, blend);
	}

	return true;
}

bool player_hud::HandAttachRelative(attachable_hud_item* item, MotionID hands_motion, float hands_time, MotionID item_motion, float item_time, Fmatrix& relative, Fmatrix* ancor_ms)
{
	IKinematicsAnimated* weapon = item->m_model->dcast_PKinematicsAnimated();
	if (m_model == nullptr || weapon == nullptr || LeftIkChain[2] == BI_NONE || item->LeftIkBone == BI_NONE)
	{
		return false;
	}

	Fmatrix ancor, wrist, attach_bone;
	if (!m_model->LL_GetMotionBoneTransform(hands_motion, hands_time, m_ancors[item->m_attach_place_idx], ancor)
		|| !m_model->LL_GetMotionBoneTransform(hands_motion, hands_time, LeftIkChain[2], wrist)
		|| !weapon->LL_GetMotionBoneTransform(item_motion, item_time, item->LeftIkBone, attach_bone))
	{
		return false;
	}

	Fmatrix attach;
	attach.mul_43(ancor, item->m_attach_offset).mulB_43(attach_bone);
	relative.mul_43(Fmatrix().invert(attach), wrist);
	if (ancor_ms != nullptr)
	{
		ancor_ms->set(ancor);
	}

	return true;
}

bool player_hud::LeftHandAttachReference(attachable_hud_item* item)
{
	IKinematicsAnimated* weapon = item->m_model->dcast_PKinematicsAnimated();
	player_hud_motion* anm = item->m_hand_motions.find_motion(item->LeftHandAttachTarget);
	if (m_model == nullptr || weapon == nullptr || anm == nullptr || anm->m_animations.empty() || LeftIkChain[2] == BI_NONE)
	{
		return false;
	}

	const motion_descr& M = anm->m_animations[0];
	const shared_str& item_anim = anm->m_base_name != anm->m_additional_name ? anm->m_additional_name : M.name;
	MotionID item_motion = weapon->ID_Cycle_Safe(item_anim);
	if (!item_motion.valid())
	{
		item_motion = weapon->ID_Cycle_Safe("idle");
	}

	const float hands_time = std::max(hud_motion_time(m_model, M.mid), 0.f);
	const float item_time = std::max(hud_motion_time(weapon, item_motion), 0.f);
	if (!HandAttachRelative(item, M.mid, hands_time, item_motion, item_time, item->LeftHandAttachRef, &item->LeftHandAttachAncorRef))
	{
		return false;
	}

	const xr_vector<u16> arm_bones = left_arm_subtree();
	hud_motion_local_pose(m_model, M.mid, hands_time, arm_bones, item->LeftHandAttachPoseRef, &item->LeftHandAttachPosRef);
	return !arm_bones.empty() && m_model->LL_GetMotionBoneTransform(M.mid, hands_time, arm_bones[0], item->LeftHandAttachArmRootRef);
}

bool player_hud::RetargetHandTarget(u16 ancor, u16 wrist, MotionID motion, u8 channel, MotionID reference, Fmatrix& result, float strength, float* deviation)
{
	if (m_model == nullptr || wrist == BI_NONE || !motion.valid() || !reference.valid())
	{
		return false;
	}

	const auto relative = [&](MotionID sample, float at, Fmatrix& out)
	{
		Fmatrix ancor_ms, wrist_ms;
		if (!m_model->LL_GetMotionBoneTransform(sample, at, ancor, ancor_ms) || !m_model->LL_GetMotionBoneTransform(sample, at, wrist, wrist_ms))
		{
			return false;
		}

		out.mul_43(Fmatrix().invert(ancor_ms), wrist_ms);
		return true;
	};

	Fquaternion rotation;
	Fvector position = { 0.f, 0.f, 0.f };
	float total = 0.f;
	for (u32 i = 0, count = m_model->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = m_model->LL_PartBlend(0, i);
		Fmatrix sample;
		if (B == nullptr || B->channel != channel || B->blend_state() == CBlend::eFREE_SLOT || B->blendAmount <= 0.f || !(B->additive_ref == reference) || !relative(B->motionID, B->timeCurrent, sample))
		{
			continue;
		}

		const Fquaternion q = Fquaternion().set(sample);
		total += B->Weight();
		const float share = B->Weight() / total;
		if (share >= 1.f)
		{
			rotation = q;
			position = sample.c;
		}
		else
		{
			rotation.slerp(Fquaternion(rotation), q, share);
			position.lerp(position, sample.c, share);
		}
	}

	Fmatrix source, reference_relative, target;
	if (total > 0.f)
	{
		source.rotation(rotation);
		source.c = position;
	}
	else
	{
		const float time = hud_motion_time(m_model, motion, channel);
		if (time < 0.f || !relative(motion, time, source))
		{
			return false;
		}
	}

	const u8 BaseMask = u8(~((1 << channel) | (1 << LAYER_ADDITIVE_CHANNEL)));
	Fmatrix AncorBase, WristBase;
	if (!m_model->LL_GetBoneTransformMasked(ancor, BaseMask, AncorBase) || !m_model->LL_GetBoneTransformMasked(wrist, BaseMask, WristBase) || !relative(reference, 0.f, reference_relative))
	{
		return false;
	}

	target.mul_43(Fmatrix().invert(AncorBase), WristBase);

	if (strength < 1.f)
	{
		Fquaternion blended;
		blended.slerp(Fquaternion().set(reference_relative), Fquaternion().set(source), std::max(strength, 0.f));
		Fvector blended_pos;
		blended_pos.lerp(reference_relative.c, source.c, std::max(strength, 0.f));
		source.rotation(blended);
		source.c = blended_pos;
	}

	if (deviation != nullptr)
	{
		*deviation = hud_ancor_deviation(source, reference_relative);
	}

	result.mul_43(source, Fmatrix().mul_43(Fmatrix().invert(reference_relative), target));
	result.c.add(source.c, Fvector().sub(target.c, reference_relative.c));
	return true;
}

static float hud_channel_weight(IKinematicsAnimated* ka, u8 channel)
{
	float weight = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == channel && B->blend_state() != CBlend::eFREE_SLOT)
		{
			weight += B->blendAmount;
		}
	}

	return std::clamp(weight, 0.f, 1.f);
}

bool player_hud::SprintBaseTransform(u16 bone, Fmatrix& result) const
{
	return bone != BI_NONE && m_model->LL_GetBoneTransformMasked(bone, u8(~(1 << LAYER_ADDITIVE_CHANNEL)), result);
}

bool player_hud::SprintBoneDelta(u16 bone, Fmatrix& result) const
{
	Fmatrix base;
	if (bone == BI_NONE || !SprintBaseTransform(bone, base))
	{
		return false;
	}

	result.mul_43(m_model->dcast_PKinematics()->LL_GetTransform(bone), Fmatrix().invert(base));
	return true;
}

static float hud_cycle_progress(IKinematicsAnimated* ka)
{
	float progress = -1.f;
	float amount = 0.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == 0 && B->stop_at_end && B->blend_state() != CBlend::eFalloff && B->timeTotal > EPS && B->blendAmount >= amount)
		{
			amount = B->blendAmount;
			progress = std::clamp(B->timeCurrent / B->timeTotal, 0.f, 1.f);
		}
	}

	return progress;
}

struct SHandsIkFrame
{
	attachable_hud_item*	Item = nullptr;
	IKinematics*			Model = nullptr;
	CWeapon*				Weapon = nullptr;
	u32						State = 0;
	bool					Active = false;
	bool					HasRight = false;
	bool					HasLeft = false;
	bool					HasAttach = false;
	u16						Ancor = BI_NONE;
	bool					AncorInRightHand = false;
	bool					RetargetRight = false;
	bool					RetargetLeft = false;
	float					SprintLayer = 0.f;
	Fmatrix					SprintAncorDelta = Fidentity;
	Fmatrix					SprintRightBase = Fidentity;
	Fmatrix					SprintLeftBase = Fidentity;
	Fmatrix					SprintRightLayer = Fidentity;
	Fmatrix					SprintLeftLayer = Fidentity;
	bool					SprintRight = false;
	bool					SprintLeft = false;
	bool					SprintLayerRight = false;
	bool					SprintLayerLeft = false;
	float					SprintLeftLayerWeight = 1.f;
	bool					SprintLeftPin = false;
	float					SprintLeftPinWeight = 0.f;
	Fmatrix					SprintLeftPinTarget = Fidentity;
	bool					ReloadState = false;
	bool					Firing = false;
	bool					MoveWeapon = false;
	Fmatrix					SprintLayerAncor = Fidentity;
	SHudFingerHold			Finger;
	bool					FingerMark = false;
	bool					FingerHold = false;
	float					FingerStart = 0.f;
	float					FingerSettle = 0.f;
	Fmatrix					Delta = Fidentity;
	Fmatrix					RightTarget = Fidentity;
	Fmatrix					LeftTarget = Fidentity;
	Fmatrix					AttachTarget = Fidentity;
	float					AttachWeight = 0.f;
	float					AttachTargetWeight = -1.f;
	float					TargetWeight = 0.f;
	xr_vector<u16>			ArmBones;
	bool					AttachPose = false;
};

static Fmatrix hud_bone_local(IKinematics* model, u16 bone)
{
	return Fmatrix().mul_43(Fmatrix().invert(model->LL_GetTransform(model->LL_GetData(bone).GetParentID())), model->LL_GetTransform(bone));
}

static void hud_blend_local(IKinematics* model, u16 bone, const Fquaternion& target, float weight, const Fvector* position = nullptr)
{
	const Fmatrix parent = model->LL_GetTransform(model->LL_GetData(bone).GetParentID());
	const Fmatrix bone_ms = model->LL_GetTransform(bone);
	const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent), bone_ms);

	Fquaternion anim, result;
	anim.set(local);
	result.slerp(anim, target, weight);

	Fmatrix new_local;
	new_local.rotation(result);
	new_local.c = position != nullptr ? Fvector().lerp(local.c, *position, weight) : local.c;
	const Fmatrix new_ms = Fmatrix().mul_43(parent, new_local);
	CCameraRig::TransformBone(model, bone, Fmatrix().mul_43(new_ms, Fmatrix().invert(bone_ms)));
}

static void hud_blend_target(Fmatrix& target, const Fmatrix& held, float weight)
{
	Fquaternion q_anim, q_held, q_blend;
	q_anim.set(target);
	q_held.set(held);
	q_blend.slerp(q_anim, q_held, weight);

	Fvector position;
	position.lerp(target.c, held.c, weight);
	target.rotation(q_blend);
	target.c = position;
}

static Fmatrix hud_wrist_relative(IKinematics* Model, u16 Ancor, u16 Wrist)
{
	return Fmatrix().mul_43(Fmatrix().invert(Model->LL_GetTransform(Ancor)), Model->LL_GetTransform(Wrist));
}

void player_hud::update_hands_ik(const Fmatrix& cam_trans, CActor* actor)
{
	SHandsIkFrame F;
	if (!HandsIkBegin(F, actor))
	{
		return;
	}

	const Fmatrix AnimRelative = F.HasLeft ? hud_wrist_relative(F.Model, F.Ancor, LeftIkChain[2]) : Fidentity;

	HandsIkSprint(F);
	HandsIkWeaponPose(F, actor);
	HandsIkFingerState(F);

	if (!F.MoveWeapon && !F.RetargetRight && !F.RetargetLeft && !F.SprintRight && !F.SprintLeft && !F.SprintLayerRight && !F.SprintLayerLeft && !(F.HasAttach && (LeftIkWeight > 0.f || F.Item->LeftHandAttachBlend > 0.f)) && !F.FingerHold && F.FingerSettle <= 0.f && F.FingerStart <= 0.f && !HandsIkSwayNeeded(F))
	{
		HandsIkFingerInertia(F);
		LeftWristLast = AnimRelative;
		return;
	}

	HandsIkWeaponDelta(F);
	HandsIkTurnPivot(F, cam_trans, actor);
	HandsIkHandTargets(F);
	HandsIkAttachTarget(F);
	UpdateHandJump(actor, F.Item->HandSwayInertia);
	HandsIkLeftSway(F, cam_trans, actor);
	HandsIkRightSway(F, actor);
	HandsIkReach(F);
	const Fmatrix WristBefore[2] = { F.HasLeft ? F.Model->LL_GetTransform(LeftIkChain[2]) : Fidentity, F.HasRight ? F.Model->LL_GetTransform(RightIkChain[2]) : Fidentity };
	const Fmatrix AncorBefore = F.Model->LL_GetTransform(F.Ancor);
	HandsIkSolve(F);
	HandsIkFingers(F);
	HandsIkFingerInertia(F);
	F.Item->update(true);
	HandsIkItemBones(F, WristBefore, AncorBefore);
	if (F.HasLeft)
	{
		LeftWristLast = hud_wrist_relative(F.Model, F.Ancor, LeftIkChain[2]);
	}
}

bool player_hud::HandsIkBegin(SHandsIkFrame& F, CActor* actor)
{
	if (LeftHandAttachSnapFrame != u32(-1) && Device.dwFrame > LeftHandAttachSnapFrame && m_model != nullptr)
	{
		for (u16 bone : left_arm_subtree())
		{
			m_model->LL_SetInertializationBone(bone, true);
		}

		LeftHandAttachSnapFrame = u32(-1);
	}

	attachable_hud_item* item = m_attached_items[0];
	IKinematics* model = m_model != nullptr ? m_model->dcast_PKinematics() : nullptr;
	if (item == nullptr || item->m_model_combined || model == nullptr || actor == nullptr || !CCameraRig::Enabled())
	{
		LeftIkWeight = 0.f;
		RetargetWeight = 0.f;
		AimReloadPoseValue = 0.f;
		AimReloadPoseVelocity = 0.f;
		AimReloadPushValue = 0.f;
		AimReloadPushVelocity = 0.f;
		AimReloadDropValue = 0.f;
		AimReloadDropVelocity = 0.f;
		return false;
	}

	const u32 state = item->m_parent_hud_item->GetState();
	bool active = m_attached_items[1] == nullptr && item->LeftIkBone != BI_NONE;
	if (active)
	{
		active = state == CHUDState::eIdle || item->m_parent_hud_item->WpnCanShoot() && (state == CWeapon::eFire || state == CWeapon::eFire2);
	}

	const char* SprintAlias = SprintAdditiveStage == ESprintAdditiveStage::Start ? "anm_idle_sprint_start" : SprintAdditiveStage == ESprintAdditiveStage::Loop ? "anm_idle_sprint" : SprintAdditiveStage == ESprintAdditiveStage::End ? "anm_idle_sprint_end" : nullptr;
	SprintLeftFree = SprintAlias != nullptr ? hud_motion_listed(item->LeftHandFreeMotions, SprintAlias) : SprintLeftFree && hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL) > 0.f;
	active = active && !item->LeftHandFree && !SprintLeftFree && !PreviewGripBlocked;

	LeftIkWeight = HudBlend::Ramp(LeftIkWeight, active, item->LeftHandIkBlend, Device.fTimeDelta);


	LeftGripActive = active;

	const bool has_right = RightIkChain[0] != BI_NONE && RightIkChain[1] != BI_NONE && RightIkChain[2] != BI_NONE;
	const bool has_left = m_attached_items[1] == nullptr && LeftIkChain[0] != BI_NONE && LeftIkChain[1] != BI_NONE && LeftIkChain[2] != BI_NONE;
	const bool has_attach = has_left && item->LeftIkBone != BI_NONE;
	const u16 ancor = m_ancors[item->m_attach_place_idx];

	u16 ancor_parent = ancor;
	while (ancor_parent != BI_NONE && ancor_parent != RightIkChain[2])
	{
		ancor_parent = model->LL_GetData(ancor_parent).GetParentID();
	}

	const bool ancor_in_right_hand = ancor_parent != BI_NONE;

	RetargetWeight = HudBlend::Ramp(RetargetWeight, item->RetargetPlaying && !(item->RetargetLayerPlaying && hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL) > 0.f), 0.f, std::max(item->IdleFade, 0.1f), Device.fTimeDelta);
	const bool retarget = RetargetWeight > 0.f && !ancor_in_right_hand;
	if (retarget)
	{
		RetargetRightValid = has_right && RetargetHandTarget(ancor, RightIkChain[2], item->RetargetMotion, ADDITIVE_CHANNEL, item->RetargetReferenceMotion, RetargetRightTarget);
		RetargetLeftValid = has_left && RetargetHandTarget(ancor, LeftIkChain[2], item->RetargetMotion, ADDITIVE_CHANNEL, item->RetargetReferenceMotion, RetargetLeftTarget);
	}

	F.Item = item;
	F.Model = model;
	F.Weapon = item->m_parent_hud_item->cast_weapon();
	F.State = state;
	F.Active = active;
	F.HasRight = has_right;
	F.HasLeft = has_left;
	F.HasAttach = has_attach;
	F.Ancor = ancor;
	F.AncorInRightHand = ancor_in_right_hand;
	F.RetargetRight = retarget && has_right && RetargetRightValid;
	F.RetargetLeft = retarget && has_left && RetargetLeftValid;
	return true;
}

void player_hud::HandsIkSprint(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	const u16 ancor = F.Ancor;
	const bool has_right = F.HasRight;
	const bool has_left = F.HasLeft;
	const bool ancor_in_right_hand = F.AncorInRightHand;

	const float sprint_layer = hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL);
	const bool sprint_action = sprint_layer > 0.f && F.State != CHUDState::eIdle && !item->RetargetPlaying && !item->AdditivePlaying;
	SprintHandWeight = HudBlend::Ramp(SprintHandWeight, sprint_action, item->SprintLayerBlend(), Device.fTimeDelta);

	const bool sprint_hands = SprintHandWeight > 0.f && sprint_layer > 0.f && SprintBoneDelta(ancor, F.SprintAncorDelta);
	F.SprintRight = sprint_hands && has_right && !ancor_in_right_hand && SprintBaseTransform(RightIkChain[2], F.SprintRightBase);
	F.SprintLeft = sprint_hands && has_left && !SprintLeftFree && SprintBaseTransform(LeftIkChain[2], F.SprintLeftBase);

	const MotionID sprint_motion = sprint_layer > 0.f && SprintAdditiveEnabled() ? hud_dominant_motion(m_model, LAYER_ADDITIVE_CHANNEL) : MotionID();
	F.SprintLayerRight = sprint_motion.valid() && has_right && !ancor_in_right_hand && RetargetHandTarget(ancor, RightIkChain[2], sprint_motion, LAYER_ADDITIVE_CHANNEL, SprintReferenceMotion, F.SprintRightLayer, hud_smooth(sprint_layer));
	HandsIkSprintLeft(F, sprint_motion, sprint_layer);
	F.SprintLayer = sprint_layer;
	SprintLayerSmooth(F);
}

void player_hud::HandsIkSprintLeft(SHandsIkFrame& F, MotionID SprintMotion, float SprintLayer)
{
	const attachable_hud_item* Item = F.Item;
	const bool Valid = SprintMotion.valid() && F.HasLeft;
	const bool Grip = SprintLeftFree && Item->SprintLeftGripRange > 0.f;

	float Deviation = 0.f;
	F.SprintLayerLeft = Valid && (!SprintLeftFree || Grip) && RetargetHandTarget(F.Ancor, LeftIkChain[2], SprintMotion, LAYER_ADDITIVE_CHANNEL, SprintReferenceMotion, F.SprintLeftLayer, hud_smooth(SprintLayer), &Deviation);
	F.SprintLeftLayerWeight = Grip ? 1.f - hud_smooth(std::clamp(Deviation / Item->SprintLeftGripRange, 0.f, 1.f)) : 1.f;

	SSprintLeftCancel& Cancel = SprintLeftCancel;
	if (SprintLayer <= 0.f)
	{
		Cancel.Active = false;
	}

	if (!Cancel.Active)
	{
		return;
	}

	if (SprintAdditiveStage == ESprintAdditiveStage::Start || SprintAdditiveStage == ESprintAdditiveStage::Loop)
	{
		Cancel.Weight = HudBlend::Ramp(Cancel.Weight, false, Item->SprintLayerBlend(), Device.fTimeDelta);
		if (Cancel.Weight <= 0.f)
		{
			Cancel.Active = false;
			return;
		}
	}
	else
	{
		Cancel.Weight = 1.f;
		Cancel.Time += Device.fTimeDelta;
		Cancel.Hold = Item->SprintLeftCancelReturn > 0.f ? 1.f - hud_smooth(std::clamp(Cancel.Time / Item->SprintLeftCancelReturn, 0.f, 1.f)) : 0.f;
	}

	F.SprintLeftPinWeight = hud_smooth(Cancel.Weight);
	F.SprintLeftPin = Valid && RetargetHandTarget(F.Ancor, LeftIkChain[2], SprintMotion, LAYER_ADDITIVE_CHANNEL, SprintReferenceMotion, F.SprintLeftPinTarget, 0.f);
	if (F.SprintLeftPin)
	{
		hud_blend_target(F.SprintLeftPinTarget, Cancel.Relative, Cancel.Hold);
	}
}

void player_hud::BeginSprintLeftCancel()
{
	SprintLeftCancel.Active = true;
	SprintLeftCancel.Weight = 1.f;
	SprintLeftCancel.Hold = 1.f;
	SprintLeftCancel.Time = 0.f;
	SprintLeftCancel.Relative = LeftWristLast;
}

void player_hud::SprintLayerSmooth(SHandsIkFrame& F)
{
	const float Time = CCameraRig::HudInertialization() > 0.f ? F.Item->SprintLayerBlend() : 0.f;
	const bool Valid[2] = { F.SprintLayerRight, F.SprintLayerLeft };
	Fmatrix* Targets[2] = { &F.SprintRightLayer, &F.SprintLeftLayer };
	const bool Continuous = SprintLayerFrame + 1 == Device.dwFrame;
	SprintLayerFrame = Device.dwFrame;

	const float Factor = HudBlend::Smooth(Time / 3.f, Device.fTimeDelta);
	for (u32 i = 0; i < 2; ++i)
	{
		if (!Valid[i] || Time <= 0.f)
		{
			SprintLayerResidual[i] = Fidentity;
			continue;
		}

		Fmatrix& Target = *Targets[i];
		if (SprintLayerSeam && Continuous)
		{
			SprintLayerResidual[i].mul_43(SprintLayerPrev[i], Fmatrix().invert(Target));
		}
		else
		{
			Fquaternion Rotation;
			Rotation.slerp(Fquaternion().set(SprintLayerResidual[i]), Fquaternion().identity(), Factor);
			const Fvector Position = Fvector().mul(SprintLayerResidual[i].c, 1.f - Factor);
			SprintLayerResidual[i].rotation(Rotation);
			SprintLayerResidual[i].c = Position;
		}

		Target.mulA_43(SprintLayerResidual[i]);
		SprintLayerPrev[i] = Target;
	}

	SprintLayerSeam = false;
}

void player_hud::HandsIkWeaponPose(SHandsIkFrame& F, CActor* actor)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	CWeapon* weapon = F.Weapon;
	const u32 state = F.State;
	const u16 ancor = F.Ancor;

	const bool reload_state = weapon != nullptr && (state == CWeapon::eReload || state == CWeapon::eMisfire || state == CWeapon::eLightMis || state == CWeapon::eMagCheck
		|| state == CWeapon::eLoadChamber || state == CWeapon::eUnloadChamber || state == CWeapon::eChamberCheck);
	Fmatrix sprint_layer_ancor = Fidentity;
	if (F.SprintLayer > 0.f)
	{
		Fmatrix full, base;
		if (m_model->LL_GetBoneTransformMasked(ancor, u8(-1), full) && m_model->LL_GetBoneTransformMasked(ancor, u8(~(1 << LAYER_ADDITIVE_CHANNEL)), base))
		{
			sprint_layer_ancor.mul_43(full, Fmatrix().invert(base));
		}
	}

	if (state == CHUDState::eIdle && AimReloadWeight <= 0.f && ShotWeight <= 0.f)
	{
		AimReloadRef.mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));
	}

	const bool aim_reload = reload_state && weapon->IsZoomed() && (item->AimReloadMotion < 1.f || item->AimReloadZoomOut > 0.f || !fis_zero(item->AimReloadOffsetPos.square_magnitude()) || !fis_zero(item->AimReloadOffsetRot.square_magnitude()) || !fis_zero(item->AimReloadPushPos.square_magnitude()));
	if (aim_reload && AimReloadWeight <= 0.f)
	{
		AimReloadRelease = 1.f;
	}

	const float aim_reload_blend = CCameraRig::AimReloadBlend();
	AimReloadWeight = HudBlend::Ramp(AimReloadWeight, aim_reload, aim_reload_blend, Device.fTimeDelta);
	const float aim_reload_target = AimReloadWeight * hud_smooth(AimReloadRelease);
	const float aim_reload_half_life = aim_reload_target < AimReloadPoseValue ? item->AimReloadPoseInertiaOut : item->AimReloadPoseInertia;
	HudSpring::Step(AimReloadPoseValue, AimReloadPoseVelocity, aim_reload_target, aim_reload_half_life, 1.f, Device.fTimeDelta);
	const float aim_reload_push = aim_reload && AimReloadRelease >= 1.f ? 1.f : 0.f;
	HudSpring::Step(AimReloadPushValue, AimReloadPushVelocity, aim_reload_push, item->AimReloadPoseInertiaOut, 1.f, Device.fTimeDelta);
	const float aim_reload_drop = aim_reload && AimReloadRelease >= 1.f ? AimReloadWeight : 0.f;
	HudSpring::Step(AimReloadDropValue, AimReloadDropVelocity, aim_reload_drop, aim_reload_drop < AimReloadDropValue ? item->AimReloadPoseInertiaOut : item->AimReloadPoseInertia, 1.f, Device.fTimeDelta);

	const bool firing = weapon != nullptr && (state == CWeapon::eFire || state == CWeapon::eFire2) && (item->ShotVariation > 0.f || item->ShotTwist > 0.f || item->AimShotMotion < 1.f);
	if (state == CHUDState::eIdle && ShotWeight <= 0.f)
	{
		ShotRef.mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));
	}

	const float shot_blend = CCameraRig::ShotAnimBlend();
	ShotWeight = HudBlend::Ramp(ShotWeight, firing, shot_blend, Device.fTimeDelta);

	F.ReloadState = reload_state;
	F.Firing = firing;
	F.SprintLayerAncor = sprint_layer_ancor;
	F.MoveWeapon = F.HasRight && (LayersIk || AimReloadWeight > 0.f || ShotWeight > 0.f);
}

void player_hud::HandsIkFingerState(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	const bool has_left = F.HasLeft;

	const SHudFingerHold finger = has_left ? hud_finger_hold(m_model, item->FingersMark, 0.3f) : SHudFingerHold();
	if (has_left && FingerGripBones.empty())
	{
		FingerGripBones = left_hand_fingers();
	}

	const bool finger_mark = finger.Motion.valid() && finger.Hold > 0.f;
	const bool finger_hold = finger_mark && finger.Idle > 0.f && !FingerGripBones.empty();
	float finger_settle = 0.f;
	if (F.HasAttach && F.Weapon != nullptr && !FingerGripBones.empty() && !item->LeftHandFree && (item->AttachPose.size() != 0 || item->LeftHandAttachBlend > 0.f))
	{
		const float settle_time = item->LeftHandAttachBlend > 0.f ? item->LeftHandAttachBlend : std::max(item->IdleFade, 0.1f);
		finger_settle = hud_motion_settle(m_model, item->m_hand_motions, settle_time, !finger_mark && LeftHandAttachWeight <= 0.f);
	}

	F.Finger = finger;
	F.FingerMark = finger_mark;
	F.FingerHold = finger_hold;
	float finger_start = 0.f;
	if (finger_mark && finger.StartHold > 0.f && !FingerGripBones.empty())
	{
		if (FingerStartMotion != finger.Motion || !fsimilar(FingerStartTime, finger.StartTime) || FingerStartPose.size() != FingerGripBones.size())
		{
			FingerStartMotion = finger.Motion;
			FingerStartTime = finger.StartTime;
			hud_motion_local_pose(m_model, finger.Motion, finger.StartTime, FingerGripBones, FingerStartPose, &FingerStartPos);
		}

		if (FingerStartPose.size() == FingerGripBones.size() && FingerStartPos.size() == FingerGripBones.size())
		{
			finger_start = hud_smooth(finger.StartHold) * finger.Hold;
		}
	}

	F.FingerSettle = finger_settle;
	F.FingerStart = finger_start;
}

void player_hud::HandsIkWeaponDelta(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	CWeapon* weapon = F.Weapon;
	const u16 ancor = F.Ancor;
	const bool move_weapon = F.MoveWeapon;
	const Fmatrix& sprint_layer_ancor = F.SprintLayerAncor;

	Fmatrix& delta = F.Delta;
	delta = move_weapon && LayersIk ? LayersIkOffset : Fidentity;
	if (move_weapon && AimReloadWeight > 0.f)
	{
		const Fmatrix anim = Fmatrix().mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));

		if (F.ReloadState)
		{
			const float progress = hud_cycle_progress(m_model);
			const float tail = CCameraRig::AimReloadTail();
			if (tail <= 0.f)
			{
				AimReloadRelease = 1.f;
			}
			else if (progress >= 0.f)
			{
				AimReloadRelease = std::clamp((1.f - progress) / tail, 0.f, 1.f);
			}
		}

		const float release = AimReloadRelease;
		const float blend = AimReloadWeight * hud_smooth(release);
		const float weight = hud_smooth(blend);
		const Fmatrix motion = Fmatrix().mul_43(Fmatrix().invert(AimReloadRef), anim);
		const Fmatrix damped = Fmatrix().mul_43(AimReloadRef, scale_layer_offset(motion, lerp(1.f, item->AimReloadMotion, weight)));
		const Fmatrix correction = Fmatrix().mul_43(damped, Fmatrix().invert(anim));
		delta.mulB_43(Fmatrix().mul_43(Fmatrix().mul_43(sprint_layer_ancor, correction), Fmatrix().invert(sprint_layer_ancor)));
	}

	if (move_weapon && ShotWeight > 0.f)
	{
		const Fmatrix anim = Fmatrix().mul_43(Fmatrix().invert(sprint_layer_ancor), model->LL_GetTransform(ancor));

		if (F.Firing)
		{
			const float progress = hud_cycle_progress(m_model);
			const float tail = CCameraRig::ShotAnimTail();
			ShotRelease = progress < 0.f || tail <= 0.f ? 1.f : std::clamp((1.f - progress) / tail, 0.f, 1.f);
		}

		const float blend = ShotWeight * hud_smooth(ShotRelease);
		const float weight = hud_smooth(blend);
		const Fmatrix motion = Fmatrix().mul_43(Fmatrix().invert(ShotRef), anim);

		const float aim = lerp(1.f, item->AimShotMotion, weapon->GetAimFactor());

		Fvector rot;
		motion.getXYZ(rot);
		const float climb = rot.x;
		rot.x *= lerp(1.f, ShotRotScale.x * aim, weight);
		rot.y = rot.y * lerp(1.f, ShotRotScale.y * aim, weight) + climb * ShotTwist.x * aim * weight;
		rot.z = rot.z * lerp(1.f, ShotRotScale.z * aim, weight) + climb * ShotTwist.y * aim * weight;

		Fvector pos = motion.c;
		pos.x *= lerp(1.f, ShotPosScale.x * aim, weight);
		pos.y *= lerp(1.f, ShotPosScale.y * aim, weight);
		pos.z *= lerp(1.f, ShotPosScale.z * aim, weight);

		Fmatrix varied;
		varied.setXYZ(rot);
		varied.translate_over(pos);
		const Fmatrix correction = Fmatrix().mul_43(Fmatrix().mul_43(ShotRef, varied), Fmatrix().invert(anim));
		delta.mulB_43(Fmatrix().mul_43(Fmatrix().mul_43(sprint_layer_ancor, correction), Fmatrix().invert(sprint_layer_ancor)));
	}
}

void player_hud::HandsIkHandTargets(SHandsIkFrame& F)
{
	IKinematics* model = F.Model;
	const u16 ancor = F.Ancor;
	const Fmatrix& delta = F.Delta;

	const float retarget_weight = hud_smooth(RetargetWeight);
	const auto retarget_blend = [&](Fmatrix& target, const Fmatrix& local)
	{
		hud_blend_target(target, Fmatrix().mul_43(delta, Fmatrix().mul_43(model->LL_GetTransform(ancor), local)), retarget_weight);
	};

	const float sprint_hand_weight = hud_smooth(SprintHandWeight);
	const auto sprint_blend = [&](Fmatrix& target, const Fmatrix& base)
	{
		hud_blend_target(target, Fmatrix().mul_43(delta, Fmatrix().mul_43(F.SprintAncorDelta, base)), sprint_hand_weight);
	};

	const float sprint_layer_weight = F.SprintLayer > 0.f ? 1.f : 0.f;
	const auto sprint_layer_blend = [&](Fmatrix& target, const Fmatrix& local, float weight)
	{
		hud_blend_target(target, Fmatrix().mul_43(delta, Fmatrix().mul_43(model->LL_GetTransform(ancor), local)), sprint_layer_weight * weight);
	};

	Fmatrix& right_target = F.RightTarget;
	right_target = F.HasRight ? Fmatrix().mul_43(delta, model->LL_GetTransform(RightIkChain[2])) : Fidentity;
	if (F.SprintLayerRight)
	{
		sprint_layer_blend(right_target, F.SprintRightLayer, 1.f);
	}

	if (F.SprintRight)
	{
		sprint_blend(right_target, F.SprintRightBase);
	}

	if (F.RetargetRight)
	{
		retarget_blend(right_target, RetargetRightTarget);
	}

	Fmatrix& left_target = F.LeftTarget;
	left_target = F.HasLeft ? Fmatrix().mul_43(delta, model->LL_GetTransform(LeftIkChain[2])) : Fidentity;
	if (F.SprintLayerLeft)
	{
		sprint_layer_blend(left_target, F.SprintLeftLayer, F.SprintLeftLayerWeight);
	}

	if (F.SprintLeftPin)
	{
		sprint_layer_blend(left_target, F.SprintLeftPinTarget, F.SprintLeftPinWeight);
	}

	if (F.SprintLeft)
	{
		sprint_blend(left_target, F.SprintLeftBase);
	}

	if (F.RetargetLeft)
	{
		retarget_blend(left_target, RetargetLeftTarget);
	}
}

void player_hud::HandsIkAttachTarget(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	const u16 ancor = F.Ancor;
	const bool active = F.Active;
	const Fmatrix& delta = F.Delta;
	const Fmatrix& left_target = F.LeftTarget;

	Fmatrix& attach_target = F.AttachTarget;
	float& attach_weight = F.AttachWeight;
	float& attach_target_weight = F.AttachTargetWeight;
	attach_target = left_target;
	attach_weight = hud_smooth(LeftIkWeight);
	attach_target_weight = -1.f;
	if (!F.HasAttach)
	{
		return;
	}

	Fmatrix attach;
	attach.mul_43(Fmatrix().invert(m_transform), Fmatrix().mul_43(item->m_item_transform, item->m_model->LL_GetTransform(item->LeftIkBone)));
	attach.mulA_43(delta);
	const Fmatrix animated = Fmatrix().mul_43(Fmatrix().invert(attach), left_target);

	const auto follow_attach = [&](bool snap)
	{
		const float follow = CCameraRig::LeftHandIkFollow();
		if (snap || follow <= 0.f)
		{
			item->LeftIkOffset.set(animated);
			return;
		}

		const float factor = HudBlend::Smooth(follow, Device.fTimeDelta);
		Fquaternion q_from, q_to, q_blend;
		q_from.set(item->LeftIkOffset);
		q_to.set(animated);
		q_blend.slerp(q_from, q_to, factor);

		Fvector position;
		position.lerp(item->LeftIkOffset.c, animated.c, factor);
		item->LeftIkOffset.rotation(q_blend);
		item->LeftIkOffset.c = position;
	};

	if (item->LeftHandAttachBlend <= 0.f)
	{
		if (active && !item->LeftIkFixed)
		{
			follow_attach(LeftIkWeight < 1.f);
		}

		attach_target.mul_43(attach, item->LeftIkOffset);
		return;
	}

	if (!item->LeftHandAttachRefTried)
	{
		item->LeftHandAttachRefTried = true;
		item->LeftHandAttachRefValid = LeftHandAttachReference(item);
		ArmRetargetMotion = MotionID();
	}
	else if (item->LeftHandAttachRefValid && (LeftHandAttachReaching || LeftHandAttachWeight > 0.f || LeftHandAttachHold > 0.f))
	{
		LeftHandAttachReference(item);
	}

	if (!item->LeftHandAttachRefValid)
	{
		LeftHandAttachReaching = false;
	}

	const bool hold = LeftHandAttachHold > 0.f && item->LeftHandAttachRefValid;
	LeftHandAttachHold = std::max(LeftHandAttachHold - Device.fTimeDelta, 0.f);

	const float release_time = std::max(CCameraRig::HudInertialization(), 0.2f);
	LeftHandAttachWeight = HudBlend::Ramp(LeftHandAttachWeight, LeftHandAttachReaching || hold, item->LeftHandAttachBlend, release_time, Device.fTimeDelta);

	float mark_weight = 0.f;
	MotionID mark_motion;
	bool mark_pre_start = false;
	const bool mark_attach = hud_attach_mark(m_model, item->IkMark, item->LeftHandAttachBlend, mark_weight, mark_motion, mark_pre_start);
	if (mark_attach)
	{
		LeftHandAttachWeight = mark_weight;
	}

	attach_weight = hud_smooth(LeftHandAttachWeight);

	float arm_target = 1.f;
	if (LeftHandAttachAnimatedArm)
	{
		const float deviation = hud_ancor_deviation(model->LL_GetTransform(ancor), item->LeftHandAttachAncorRef);
		arm_target = std::clamp((1.f - deviation) / 0.7f, 0.f, 1.f);
	}

	LeftHandAttachArmWeight = LeftHandAttachWeight <= 0.f || mark_attach ? arm_target
		: LeftHandAttachArmWeight + std::clamp(arm_target - LeftHandAttachArmWeight, -Device.fTimeDelta / item->LeftHandAttachBlend, Device.fTimeDelta / item->LeftHandAttachBlend);
	attach_target.mul_43(attach, item->LeftHandAttachRef);

	if (!mark_attach || !mark_pre_start)
	{
		AttachOffsetMotion = MotionID();
		return;
	}

	if (mark_weight <= 0.f)
	{
		return;
	}

	IKinematicsAnimated* weapon_model = item->m_model->dcast_PKinematicsAnimated();
	const MotionID weapon_motion = weapon_model != nullptr ? hud_dominant_motion(weapon_model) : MotionID();
	if (AttachOffsetMotion != mark_motion)
	{
		AttachOffsetMotion = mark_motion;
		AttachOffsetSampled = weapon_motion.valid() && HandAttachRelative(item, mark_motion, 0.f, weapon_motion, 0.f, AttachOffsetRef);
		if (!AttachOffsetSampled)
		{
			AttachOffsetRef.set(animated);
		}
	}

	Fmatrix source = animated;
	if (AttachOffsetSampled)
	{
		const float hands_time = std::max(hud_motion_time(m_model, mark_motion), 0.f);
		const float weapon_time = std::max(hud_motion_time(weapon_model, weapon_motion), 0.f);
		if (!HandAttachRelative(item, mark_motion, hands_time, weapon_motion, weapon_time, source))
		{
			source = animated;
		}
	}

	const float offset = hud_smooth(mark_weight);
	Fmatrix grip_rotation, ref_rotation, source_rotation;
	grip_rotation.rotation(Fquaternion().set(item->LeftHandAttachRef));
	ref_rotation.rotation(Fquaternion().set(AttachOffsetRef));
	source_rotation.rotation(Fquaternion().set(source));

	Fquaternion q_offset;
	q_offset.slerp(Fquaternion().identity(), Fquaternion().set(Fmatrix().mul_43(grip_rotation, Fmatrix().invert(ref_rotation))), offset);

	Fmatrix shifted;
	shifted.mul_43(Fmatrix().rotation(q_offset), source_rotation);
	shifted.c.mad(source.c, Fvector().sub(item->LeftHandAttachRef.c, AttachOffsetRef.c), offset);
	attach_target.mul_43(attach, shifted);
	attach_target_weight = 1.f;
}

static bool hud_is_unit(const Fvector& Value)
{
	return Value.similar(Fvector().set(1.f, 1.f, 1.f));
}

static Fmatrix hud_weighted_transform(const Fmatrix& Transform, float Weight)
{
	Fquaternion Rotation;
	Rotation.slerp(Fquaternion().identity(), Fquaternion().set(Transform), Weight);
	Fmatrix Result;
	Result.rotation(Rotation);
	Result.c.mul(Transform.c, Weight);
	return Result;
}

static bool hud_left_follow_custom(const attachable_hud_item* Item)
{
	return Item->LeftFollowLag > 0.f || Item->LeftFollowTurn < 1.f || !hud_is_unit(Item->LeftFollowMove) || Item->LeftFollowMoveRot < 1.f || !hud_is_unit(Item->LeftBobPos) || !hud_is_unit(Item->LeftBobRot);
}

static bool hud_turn_pivot_custom(const SHandsIkFrame& F)
{
	const attachable_hud_item* Item = F.Item;
	if (F.Weapon == nullptr || F.Weapon->cast_weapon_knife() != nullptr || Item->m_model_combined)
	{
		return false;
	}

	return !fis_zero(Item->TurnPivotLook.square_magnitude()) || Item->TurnPivotGripLag < 1.f || !fis_zero(Item->TurnPivotLift) || !fis_zero(Item->TurnPivotLiftPos) || !fis_zero(Item->TurnTilt);
}

static bool hud_hand_sway_custom(const attachable_hud_item* Item)
{
	return !fis_zero(Item->LeftSwayLook.square_magnitude()) || !fis_zero(Item->LeftSwayBob.square_magnitude()) || !fis_zero(Item->LeftSwayJump.square_magnitude()) || !fis_zero(Item->RightSwayLook.square_magnitude()) || !fis_zero(Item->RightSwayJump.square_magnitude());
}

bool player_hud::HandsIkSwayNeeded(const SHandsIkFrame& F) const
{
	if (!F.HasLeft && !F.HasRight)
	{
		return false;
	}

	const auto Moving = [](const SHudVectorSpring& Spring)
	{
		return Spring.Value.square_magnitude() > EPS_S || Spring.Velocity.square_magnitude() > EPS_S;
	};

	return hud_hand_sway_custom(F.Item) || hud_left_follow_custom(F.Item) || hud_turn_pivot_custom(F) || Moving(TurnPivot.Lag) || Moving(TurnPivot.Lift) || std::abs(TurnPivot.Tilt) > EPS_S || std::abs(TurnPivot.TiltVelocity) > EPS_S || Moving(LeftSway) || Moving(RightSway);
}

Fmatrix player_hud::LeftFollowTarget(const SHandsIkFrame& F, const Fmatrix& CamTrans, const Fmatrix& Seen, CActor* actor)
{
	attachable_hud_item* Item = F.Item;
	Fmatrix Target = Fmatrix().mul_43(Seen, Fmatrix().invert(MovementLayersOffset));

	if (!hud_is_unit(Item->LeftFollowMove) || Item->LeftFollowMoveRot < 1.f)
	{
		const auto& Tune = Item->m_measures.m_hands_positions.hands_offsets_tune;
		const Fquaternion Current = HudRotation::FromEuler(Item->hands_attach_rot());
		Fquaternion Wanted;
		Wanted.slerp(HudRotation::FromEuler(Tune[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault]), Current, Item->LeftFollowMoveRot);

		Fmatrix Attach;
		Attach.rotation(Current);
		Attach.c = Item->hands_attach_pos();
		Fmatrix Rest;
		Rest.rotation(Wanted);
		const Fvector& RestPos = Tune[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
		Rest.c.sub(Item->hands_attach_pos(), RestPos).mul(Item->LeftFollowMove).add(RestPos);
		Target.mulB_43(Fmatrix().mul_43(Fmatrix().invert(Attach), Rest));
	}

	if (Item->LeftFollowTurn < 1.f)
	{
		Fmatrix LookPre, Look;
		if (Item->m_parent_hud_item->GetHudLookInertia(LookPre, Look))
		{
			const Fmatrix Pre = Fmatrix().mul_43(Fmatrix().invert(CamTrans), LookPre);
			const Fmatrix Scaled = Fmatrix().mul_43(scale_layer_offset(Look, Item->LeftFollowTurn), Fmatrix().invert(Look));
			Target.mulA_43(Fmatrix().mul_43(Fmatrix().mul_43(Pre, Scaled), Fmatrix().invert(Pre)));
		}

		Target.c.mad(actor->Cameras().Rig().LookSwayOffset(), Item->LeftFollowTurn - 1.f);
	}

	return Target;
}

void player_hud::HandsIkLeftFollow(SHandsIkFrame& F, const Fmatrix& CamTrans, float Free, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	const Fmatrix Hud = Fmatrix().mul_43(Fmatrix().invert(CamTrans), m_transform);
	const Fmatrix Seen = Fmatrix().mul_43(Hud, F.Delta);
	const bool Continuous = LeftFollow.Frame + 1 == Device.dwFrame;
	LeftFollow.Frame = Device.dwFrame;

	if (!hud_left_follow_custom(Item))
	{
		return;
	}

	const Fmatrix Target = LeftFollowTarget(F, CamTrans, Seen, actor);
	const Fquaternion TargetRotation = Fquaternion().set(Target);
	if (!Continuous || Item->LeftFollowLag <= 0.f)
	{
		LeftFollow = { TargetRotation, Target.c, {}, {}, Device.dwFrame };
	}
	else
	{
		Fvector Offset = HudRotation::Delta(TargetRotation, LeftFollow.Rotation);
		HudSpring::Follow(Offset, LeftFollow.RotationVelocity, zero_vel, Item->LeftFollowLag, Item->HandSwayDamping, Device.fTimeDelta);
		LeftFollow.Rotation = HudRotation::Apply(Offset, TargetRotation);
		HudSpring::Follow(LeftFollow.Position, LeftFollow.PositionVelocity, Target.c, Item->LeftFollowLag, Item->HandSwayDamping, Device.fTimeDelta);
	}

	if (Free <= 0.f)
	{
		return;
	}

	Fmatrix Desired;
	Desired.rotation(LeftFollow.Rotation);
	Desired.c = LeftFollow.Position;

	Fvector LayerRot;
	MovementLayersOffset.getXYZ(LayerRot);
	Fmatrix Bob;
	Bob.setXYZ(Fvector().mul(LayerRot, Item->LeftBobRot));
	Bob.translate_over(Fvector().mul(MovementLayersOffset.c, Item->LeftBobPos));
	Desired.mulB_43(Bob);

	const Fmatrix Correction = Fmatrix().mul_43(Fmatrix().mul_43(Fmatrix().invert(Hud), Desired), Fmatrix().mul_43(Fmatrix().invert(Seen), Hud));
	F.LeftTarget.mulA_43(hud_weighted_transform(Correction, Free));
}

void player_hud::UpdateHandJump(CActor* actor, float HalfLife)
{
	const u32 State = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
	const float VerticalSpeed = actor->character_physics_support()->movement()->GetVelocity().y;
	const bool Airborne = (State & (mcJump | mcFall)) != 0;

	if ((State & mcJump) != 0 && (HandJump.StatePrev & mcJump) == 0)
	{
		HandJump.Time = 0.f;
		HandJump.Power = 1.f;
	}

	constexpr u32 LandingFlags = mcLanding | mcLanding2;
	if ((State & LandingFlags) != 0 && (HandJump.StatePrev & LandingFlags) == 0)
	{
		HandJump.Time = 0.f;
		HandJump.Power = -std::clamp(-HandJump.FallSpeed / 6.f, 0.f, 1.f);
	}

	HandJump.StatePrev = State;
	HandJump.FallSpeed = Airborne ? std::min(VerticalSpeed, HandJump.FallSpeed) : 0.f;
	HandJump.Time += Device.fTimeDelta;
	if (!Airborne && HandJump.Time >= 0.15f)
	{
		HandJump.Power = 0.f;
	}

	const bool Rising = Airborne && VerticalSpeed > 0.f && HandJump.Power >= 1.f;
	const float Target = Rising || HandJump.Time < 0.15f ? HandJump.Power : 0.f;
	HudSpring::Step(HandJump.Weight, HandJump.Velocity, Target, std::abs(Target) > std::abs(HandJump.Weight) ? HalfLife * 0.5f : HalfLife, 1.f, Device.fTimeDelta);
}

static Fmatrix hud_pivot_rotation(const Fvector& Hpb, const Fvector& Pivot)
{
	Fmatrix Rotation;
	Rotation.setHPB(Hpb.x, Hpb.y, Hpb.z);
	Fvector Rotated;
	Rotation.transform_dir(Rotated, Pivot);
	Rotation.c.sub(Pivot, Rotated);
	return Rotation;
}

static Fvector hud_look_sway(CActor* actor, const Fvector& Look)
{
	const float Yaw = std::clamp(actor->fFPCamYawMagnitude, -PI_MUL_2, PI_MUL_2);
	const float Pitch = std::clamp(actor->fFPCamPitchMagnitude, -PI_MUL_2, PI_MUL_2);
	return Fvector().set(-Yaw * Look.y, -Pitch * Look.x, Yaw * Look.z);
}

static Fvector hud_jump_sway(const Fvector& Jump, float Weight)
{
	return Fvector().set(Jump.y, Jump.x, Jump.z).mul(Weight * PI / 180.f);
}

void player_hud::HandsIkLeftSway(SHandsIkFrame& F, const Fmatrix& CamTrans, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	if (!F.HasLeft)
	{
		LeftSway = {};
		return;
	}

	const float AttachWeight = F.HasAttach ? std::clamp(F.AttachTargetWeight >= 0.f ? F.AttachTargetWeight : F.AttachWeight, 0.f, 1.f) : 0.f;
	const float Free = 1.f - hud_smooth(AttachWeight);

	HandsIkLeftFollow(F, CamTrans, Free, actor);

	const Fvector Bob = Fvector().mul(MovementLayersOffset.c, 100.f);
	const Fvector& BobScale = Item->LeftSwayBob;
	const Fvector Target = hud_look_sway(actor, Item->LeftSwayLook).add(Fvector().set(-Bob.x * BobScale.y, Bob.y * BobScale.x, Bob.x * BobScale.z)).mul(Free * PI / 180.f);
	HudSpring::Follow(LeftSway.Value, LeftSway.Velocity, Target, Item->HandSwayInertia, Item->HandSwayDamping, Device.fTimeDelta);

	const Fvector Hpb = hud_jump_sway(Item->LeftSwayJump, HandJump.Weight * Free).add(LeftSway.Value);
	if (Hpb.square_magnitude() > EPS_S)
	{
		F.LeftTarget.mulA_43(hud_pivot_rotation(Hpb, F.LeftTarget.c));
	}
}

void player_hud::HandsIkRightSway(SHandsIkFrame& F, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	if (!F.HasRight)
	{
		RightSway = {};
		return;
	}

	const Fvector Target = hud_look_sway(actor, Item->RightSwayLook).mul(PI / 180.f);
	HudSpring::Follow(RightSway.Value, RightSway.Velocity, Target, Item->HandSwayInertia, Item->HandSwayDamping, Device.fTimeDelta);

	const Fvector Hpb = hud_jump_sway(Item->RightSwayJump, HandJump.Weight).add(RightSway.Value);
	if (Hpb.square_magnitude() <= EPS_S)
	{
		return;
	}

	const Fmatrix Rotation = hud_pivot_rotation(Hpb, F.RightTarget.c);
	F.RightTarget.mulA_43(Rotation);
	if (!F.AncorInRightHand)
	{
		F.Delta.mulA_43(Rotation);
	}
}

static float hud_soft_limit(float Value, float Max)
{
	return Max > 0.f ? Max * tanhf(Value / Max) : Value;
}

static Fmatrix hud_rotation_about(const Fmatrix& Rotation, const Fvector& Pivot)
{
	Fmatrix Result = Rotation;
	Fvector Rotated;
	Result.transform_dir(Rotated, Pivot);
	Result.c.sub(Pivot, Rotated);
	return Result;
}

void player_hud::UpdateTurnPivot(const attachable_hud_item* Item, CActor* actor)
{
	SHudTurnPivot& T = TurnPivot;
	const float Dt = Device.fTimeDelta;
	const float Aim = lerp(1.f, Item->AimTurnPivotScale, Item->m_parent_hud_item->GetAimFactor());
	const float RateSmooth = HudBlend::Smooth(Item->TurnPivotInertia * 0.5f, Dt);
	T.Rate.x += (std::clamp(actor->fFPCamYawMagnitude, -PI_MUL_2, PI_MUL_2) - T.Rate.x) * RateSmooth;
	T.Rate.y += (std::clamp(actor->fFPCamPitchMagnitude, -PI_MUL_2, PI_MUL_2) - T.Rate.y) * RateSmooth;
	const float Yaw = T.Rate.x * Aim;
	const float Pitch = T.Rate.y * Aim;

	const float Max = Item->TurnPivotMax;
	const Fvector& Look = Item->TurnPivotLook;
	const Fvector LagTarget = { deg2rad(hud_soft_limit(-Yaw * Look.y, Max)), deg2rad(hud_soft_limit(-Pitch * Look.x, Max)), deg2rad(hud_soft_limit(Yaw * Look.z, Max)) };
	HudSpring::Follow(T.Lag.Value, T.Lag.Velocity, LagTarget, Item->TurnPivotInertia, Item->TurnPivotDamping, Dt);
	HudSpring::Step(T.Tilt, T.TiltVelocity, deg2rad(hud_soft_limit(-Yaw * Item->TurnTilt, Item->TurnTiltMax)), Item->TurnPivotInertia, 1.f, Dt);

	const float Speed = std::abs(Yaw);
	const float Envelope = Speed > T.LiftSpeed ? Item->TurnPivotLiftInertia : Item->TurnPivotLiftInertia * std::max(Item->TurnPivotLiftFall, 1.f);
	T.LiftSpeed += (Speed - T.LiftSpeed) * HudBlend::Smooth(Envelope, Dt);
	const Fvector LiftTarget = { deg2rad(hud_soft_limit(T.LiftSpeed * Item->TurnPivotLift, Max)), hud_soft_limit(T.LiftSpeed * Item->TurnPivotLiftPos, Item->TurnPivotLiftMax), 0.f };
	HudSpring::Follow(T.Lift.Value, T.Lift.Velocity, LiftTarget, Item->TurnPivotLiftInertia, 1.f, Dt);
}

Fvector player_hud::TurnPivotPoint(const SHandsIkFrame& F) const
{
	const attachable_hud_item* Item = F.Item;
	Fvector Pivot;
	if (Item->TurnPivotBone != BI_NONE)
	{
		Pivot = Item->m_model->LL_GetTransform(Item->TurnPivotBone).c;
		Item->m_attach_offset.transform_tiny(Pivot);
		F.Model->LL_GetTransform(F.Ancor).transform_tiny(Pivot);
	}
	else
	{
		Pivot = F.Model->LL_GetTransform(F.HasRight ? RightIkChain[2] : F.Ancor).c;
	}

	F.Delta.transform_tiny(Pivot);
	return Pivot;
}

Fvector player_hud::TurnGripShift(const attachable_hud_item* Item, const Fvector& Pivot, const Fmatrix& CamTrans, CActor* actor) const
{
	Fvector World;
	m_transform.transform_tiny(World, Pivot);
	Fvector Settled = World;
	Fmatrix LookPre, Look;
	if (Item->m_parent_hud_item->GetHudLookInertia(LookPre, Look))
	{
		Fmatrix().mul_43(Fmatrix().mul_43(LookPre, Fmatrix().invert(Look)), Fmatrix().invert(LookPre)).transform_tiny(Settled);
	}

	Fvector Sway;
	CamTrans.transform_dir(Sway, actor->Cameras().Rig().LookSwayOffset());
	Settled.sub(Sway);

	Fvector Shift;
	Fmatrix().invert(m_transform).transform_dir(Shift, Fvector().sub(Settled, World));
	return Shift.mul(1.f - Item->TurnPivotGripLag);
}

void player_hud::HandsIkTurnPivot(SHandsIkFrame& F, const Fmatrix& CamTrans, CActor* actor)
{
	const attachable_hud_item* Item = F.Item;
	if (!hud_turn_pivot_custom(F))
	{
		TurnPivot = {};
		return;
	}

	UpdateTurnPivot(Item, actor);
	const SHudTurnPivot& T = TurnPivot;

	const Fmatrix ModelToCam = Fmatrix().mul_43(Fmatrix().invert(CamTrans), m_transform);
	const Fmatrix CamToModel = Fmatrix().invert(ModelToCam);
	Fvector Pivot = TurnPivotPoint(F);

	Fvector Shift = Item->TurnPivotGripLag < 1.f ? TurnGripShift(Item, Pivot, CamTrans, actor) : Fvector().set(0.f, 0.f, 0.f);
	Fvector Up;
	CamToModel.transform_dir(Up, Fvector().set(0.f, T.Lift.Value.y, 0.f));
	Shift.add(Up);
	F.Delta.mulA_43(Fmatrix().translate(Shift));
	Pivot.add(Shift);

	const Fmatrix CamRotation = Fmatrix().setHPB(T.Lag.Value.x, T.Lag.Value.y + T.Lift.Value.x, T.Lag.Value.z + T.Tilt);
	const Fmatrix Rotation = Fmatrix().mul_43(Fmatrix().mul_43(CamToModel, CamRotation), ModelToCam);
	F.Delta.mulA_43(hud_rotation_about(Rotation, Pivot));
}

void player_hud::HandsIkItemBones(SHandsIkFrame& F, const Fmatrix* WristBefore, const Fmatrix& AncorBefore)
{
	attachable_hud_item* Item = F.Item;
	IKinematics* ItemModel = Item->m_model;
	if (ItemModel == nullptr || Item->m_model_combined || Item->LeftHandItemBones.empty() && Item->RightHandItemBones.empty())
	{
		return;
	}

	const Fmatrix ItemBefore = Fmatrix().mul_43(AncorBefore, Item->m_attach_offset);
	const Fmatrix ItemAfterInverse = Fmatrix().invert(Fmatrix().mul_43(F.Model->LL_GetTransform(F.Ancor), Item->m_attach_offset));
	const u16 Root = ItemModel->LL_GetBoneRoot();
	const Fvector RootPos = ItemModel->LL_GetTransform(Root).c;
	const Fvector RootBind = Fmatrix().invert(ItemModel->LL_GetData(Root).m2b_transform).c;

	const auto Follow = [&](const xr_vector<u16>& Bones, u16 Wrist, const Fmatrix& Before)
	{
		if (Bones.empty() || Wrist == BI_NONE)
		{
			return;
		}

		const Fmatrix Hand = Fmatrix().mul_43(F.Model->LL_GetTransform(Wrist), Fmatrix().invert(Before));
		const Fmatrix Delta = Fmatrix().mul_43(Fmatrix().mul_43(ItemAfterInverse, Hand), ItemBefore);

		for (u16 Bone : Bones)
		{
			const float Rest = Fmatrix().invert(ItemModel->LL_GetData(Bone).m2b_transform).c.distance_to(RootBind);
			const float Separation = ItemModel->LL_GetTransform(Bone).c.distance_to(RootPos) - Rest;
			const float Weight = hud_smooth(std::clamp((Separation - 0.01f) / 0.03f, 0.f, 1.f));
			if (Weight <= 0.f)
			{
				continue;
			}

			CCameraRig::TransformBone(ItemModel, Bone, hud_weighted_transform(Delta, Weight));
		}
	};

	Follow(Item->LeftHandItemBones, F.HasLeft ? LeftIkChain[2] : BI_NONE, WristBefore[0]);
	Follow(Item->RightHandItemBones, F.HasRight ? RightIkChain[2] : BI_NONE, WristBefore[1]);
}

void player_hud::HandsIkReach(SHandsIkFrame& F)
{
	IKinematics* model = F.Model;
	const bool has_right = F.HasRight;
	const bool has_left = F.HasLeft;

	Fvector shift = zero_vel;
	const auto reach = [&](const u16* chain, const Fvector& target)
	{
		const Fvector root = model->LL_GetTransform(chain[0]).c;
		const float length = (root.distance_to(model->LL_GetTransform(chain[1]).c) + model->LL_GetTransform(chain[1]).c.distance_to(model->LL_GetTransform(chain[2]).c)) * 0.995f;
		const Fvector dir = Fvector().sub(target, root).sub(shift);
		const float distance = dir.magnitude();
		if (distance > length)
		{
			shift.mad(dir, 1.f - length / distance);
		}
	};

	F.TargetWeight = F.AttachTargetWeight >= 0.f ? F.AttachTargetWeight : F.AttachWeight;
	const Fvector left_goal = Fvector().lerp(F.LeftTarget.c, F.AttachTarget.c, F.HasAttach ? F.TargetWeight : 0.f);
	for (int i = 0; i < 4; i++)
	{
		if (has_right)
		{
			reach(RightIkChain, F.RightTarget.c);
		}

		if (has_left)
		{
			reach(LeftIkChain, left_goal);
		}
	}

	if (shift.square_magnitude() > EPS_S)
	{
		Fvector world_shift;
		m_transform.transform_dir(world_shift, shift);
		m_transform.c.add(world_shift);
		F.RightTarget.c.sub(shift);
		F.LeftTarget.c.sub(shift);
		F.AttachTarget.c.sub(shift);
		F.Delta.mulA_43(Fmatrix().translate(Fvector().invert(shift)));
	}
}

void player_hud::HandsIkSolve(SHandsIkFrame& F)
{
	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	const bool has_right = F.HasRight;
	const bool has_attach = F.HasAttach;
	const float attach_weight = F.AttachWeight;
	const float target_weight = F.TargetWeight;
	const Fmatrix& left_target = F.LeftTarget;
	const Fmatrix& attach_target = F.AttachTarget;

	if (!F.AncorInRightHand || !has_right)
	{
		CCameraRig::TransformBone(model, F.Ancor, F.Delta);
	}

	if (has_right)
	{
		CCameraRig::SolveLimb(model, RightIkChain, F.RightTarget, 1.f);
	}

	if (!F.HasLeft)
	{
		return;
	}

	const bool attach_pose = has_attach && item->LeftHandAttachBlend > 0.f && item->LeftHandAttachRefValid && attach_weight > 0.f;

	F.ArmBones = left_arm_subtree();
	F.AttachPose = attach_pose;
	const xr_vector<u16>& arm_bones = F.ArmBones;
	if (attach_pose && !arm_bones.empty() && item->LeftHandAttachPoseRef.size() == arm_bones.size())
	{
		const xr_vector<u16> upper_bones = left_arm_blend_bones();
		const float arm = hud_smooth(LeftHandAttachArmWeight);
		const bool retarget_arm = LeftHandAttachAnimatedArm && ArmRetargetReference(item, arm_bones);
		{
			const Fmatrix current = model->LL_GetTransform(arm_bones[0]);
			Fmatrix attach_root = item->LeftHandAttachArmRootRef;
			Fmatrix sprint_root_delta;
			if (F.SprintLayer > 0.f && SprintBoneDelta(model->LL_GetData(arm_bones[0]).GetParentID(), sprint_root_delta))
			{
				attach_root.mulA_43(sprint_root_delta);
			}

			Fquaternion q_current, q_attach, q_arm, q_result;
			q_current.set(current);
			q_attach.set(attach_root);
			Fvector arm_pos = attach_root.c;
			float root_weight = attach_weight * arm;
			if (retarget_arm)
			{
				Fquaternion q_retarget;
				q_retarget.set(Fmatrix().mul_43(Fmatrix().mul_43(Fmatrix().rotation(q_current), Fmatrix().invert(Fmatrix().rotation(Fquaternion().set(ArmRetargetRootRef)))), Fmatrix().rotation(q_attach)));
				q_arm.slerp(q_retarget, q_attach, arm);
				arm_pos.lerp(Fvector().add(attach_root.c, Fvector().sub(current.c, ArmRetargetRootRef.c)), attach_root.c, arm);
				root_weight = attach_weight;
			}
			else
			{
				q_arm = q_attach;
			}

			q_result.slerp(q_current, q_arm, root_weight);

			Fmatrix result;
			result.rotation(q_result);
			result.c.lerp(current.c, arm_pos, root_weight);
			CCameraRig::TransformBone(model, arm_bones[0], Fmatrix().mul_43(result, Fmatrix().invert(current)));
		}

		for (size_t i = 1; i < arm_bones.size(); ++i)
		{
			const u16 bone = arm_bones[i];

			if (F.FingerMark && std::find(FingerGripBones.begin(), FingerGripBones.end(), bone) != FingerGripBones.end())
			{
				continue;
			}

			const Fquaternion& target_pose = item->LeftHandAttachPoseRef[i];
			if (std::find(upper_bones.begin(), upper_bones.end(), bone) == upper_bones.end())
			{
				hud_blend_local(model, bone, target_pose, attach_weight, item->LeftHandAttachPosRef.size() == arm_bones.size() ? &item->LeftHandAttachPosRef[i] : nullptr);
			}
			else if (!retarget_arm)
			{
				hud_blend_local(model, bone, target_pose, attach_weight * arm);
			}
			else
			{
				const Fmatrix local = hud_bone_local(model, bone);
				const Fmatrix offset = Fmatrix().mul_43(Fmatrix().rotation(target_pose), Fmatrix().invert(Fmatrix().rotation(ArmRetargetPose[i])));
				Fquaternion animated;
				animated.slerp(Fquaternion().set(Fmatrix().mul_43(offset, local)), target_pose, arm);
				hud_blend_local(model, bone, animated, attach_weight);
			}
		}

		Fquaternion q_anim, q_attach, q_blend;
		q_anim.set(left_target);
		q_attach.set(attach_target);
		q_blend.slerp(q_anim, q_attach, target_weight);

		Fmatrix target;
		target.rotation(q_blend);
		target.c.lerp(left_target.c, attach_target.c, target_weight);
		CCameraRig::SolveLimb(model, LeftIkChain, target, 1.f);
	}
	else
	{
		CCameraRig::SolveLimb(model, LeftIkChain, left_target, 1.f);
		if (has_attach && target_weight > 0.f)
		{
			CCameraRig::SolveLimb(model, LeftIkChain, attach_target, target_weight);
		}
	}
}

void player_hud::HandsIkFingerInertia(SHandsIkFrame& F)
{
	const size_t count = FingerGripBones.size();
	const float time = CCameraRig::HudInertialization();
	if (!F.HasLeft || count == 0 || time <= 0.f)
	{
		FingerPrevRot.clear();
		return;
	}

	const bool fresh = FingerPrevRot.size() != count || Device.dwFrame != FingerResidualFrame + 1;
	FingerResidualFrame = Device.dwFrame;
	if (fresh)
	{
		FingerPrevRot.assign(count, Fquaternion().identity());
		FingerPrevPos.assign(count, Fvector().set(0.f, 0.f, 0.f));
		FingerResidualRot.assign(count, Fquaternion().identity());
		FingerResidualPos.assign(count, Fvector().set(0.f, 0.f, 0.f));
	}

	const auto angle = [](const Fquaternion& q)
	{
		return 2.f * acosf(std::min(std::abs(q.w), 1.f));
	};

	const float jump_rot = std::max(deg2rad(8.f), deg2rad(400.f) * Device.fTimeDelta);
	const float jump_pos = std::max(0.002f, 0.1f * Device.fTimeDelta);
	const float factor = HudBlend::Smooth(time / 3.f, Device.fTimeDelta);
	for (size_t i = 0; i < count; ++i)
	{
		const u16 bone = FingerGripBones[i];
		const Fmatrix local = hud_bone_local(F.Model, bone);
		const Fquaternion current = Fquaternion().set(local);
		const Fmatrix current_rot = Fmatrix().rotation(current);

		if (!fresh)
		{
			const Fmatrix step = Fmatrix().mul_43(Fmatrix().rotation(FingerPrevRot[i]), Fmatrix().invert(current_rot));
			const float jump = angle(Fquaternion().set(step));
			const Fvector step_pos = Fvector().sub(FingerPrevPos[i], local.c);
			if (jump > jump_rot || step_pos.magnitude() > jump_pos)
			{
				FingerResidualRot[i].set(Fmatrix().mul_43(Fmatrix().rotation(FingerResidualRot[i]), step));
				FingerResidualPos[i].add(step_pos);
			}

			FingerResidualRot[i].slerp(Fquaternion(FingerResidualRot[i]), Fquaternion().identity(), factor);
			FingerResidualPos[i].mul(1.f - factor);
		}

		FingerPrevRot[i] = current;
		FingerPrevPos[i] = local.c;

		const Fquaternion applied = Fquaternion().set(Fmatrix().mul_43(Fmatrix().rotation(FingerResidualRot[i]), current_rot));
		const Fvector applied_pos = Fvector().add(local.c, FingerResidualPos[i]);
		hud_blend_local(F.Model, bone, applied, 1.f, &applied_pos);
	}

}

void player_hud::HandsIkFingers(SHandsIkFrame& F)
{
	if (!F.HasLeft)
	{
		return;
	}

	attachable_hud_item* item = F.Item;
	IKinematics* model = F.Model;
	const xr_vector<u16>& arm_bones = F.ArmBones;
	const bool finger_hold = F.FingerHold;
	const float finger_settle = F.FingerSettle;

	if (F.FingerStart > 0.f)
	{
		for (size_t i = 0; i < FingerGripBones.size(); ++i)
		{
			hud_blend_local(model, FingerGripBones[i], FingerStartPose[i], F.FingerStart, &FingerStartPos[i]);
		}
	}

	const bool attach_ref = item->LeftHandAttachRefValid && item->LeftHandAttachPoseRef.size() == arm_bones.size();
	const bool idle_ref = (finger_hold || finger_settle > 0.f) && !attach_ref && FingerGripReference(item);
	const auto finger_target = [&](size_t i, Fquaternion& target, const Fvector*& position)
	{
		position = nullptr;
		if (attach_ref)
		{
			const auto it = std::find(arm_bones.begin(), arm_bones.end(), FingerGripBones[i]);
			if (it == arm_bones.end())
			{
				return false;
			}

			const size_t index = it - arm_bones.begin();
			target = item->LeftHandAttachPoseRef[index];
			position = item->LeftHandAttachPosRef.size() == arm_bones.size() ? &item->LeftHandAttachPosRef[index] : nullptr;
			return true;
		}

		if (idle_ref)
		{
			target = item->FingerGripPose[i];
			position = item->FingerGripPos.size() == FingerGripBones.size() ? &item->FingerGripPos[i] : nullptr;
			return true;
		}

		return false;
	};

	if (finger_hold)
	{
		const float weight = hud_smooth(F.Finger.Idle) * F.Finger.Hold * item->LeftHandFingerGrip;
		for (size_t i = 0; i < FingerGripBones.size() && weight > 0.f; ++i)
		{
			Fquaternion grip;
			const Fvector* grip_position = nullptr;
			if (!finger_target(i, grip, grip_position))
			{
				continue;
			}

			hud_blend_local(model, FingerGripBones[i], grip, weight, grip_position);
		}
	}

	if (finger_settle > 0.f)
	{
		const float weight = hud_smooth(finger_settle);
		for (size_t i = 0; i < FingerGripBones.size(); ++i)
		{
			Fquaternion target;
			const Fvector* position = nullptr;
			if (finger_target(i, target, position))
			{
				hud_blend_local(model, FingerGripBones[i], target, weight, position);
			}
		}
	}
}

u32 player_hud::anim_play(u16 part, const MotionID& M, bool bMixIn, const CMotionDef*& md, float speed, IKinematicsAnimated* model)
{
	IKinematicsAnimated* itemModel = model;

	if (!itemModel && m_model)
	{
		itemModel = m_model;

		bool disable_root_part = false;
		u16 part_id = u16(-1);
		if (attached_item(0) && attached_item(1))
		{
			disable_root_part = part == 1;
			part_id = m_model->partitions().part_id((part == 0) ? "right_hand" : "left_hand");
		}

		u16 pc = m_model->partitions().count();
		for (u16 pid = 0; pid < pc; ++pid)
		{
			if (pid == 0 && disable_root_part)
			{
				continue;
			}

			if (pid == 0 || pid == part_id || part_id == u16(-1))
			{
				if (m_blocked_part_idx == pid)
				{
					continue;
				}

				hud_play_cycle(m_model, pid, M, part == 0 && pid == 0 && attached_item(1) ? true : bMixIn, speed, CycleFade, CycleMode);
			}
		}

		m_model->dcast_PKinematics()->CalculateBones_Invalidate();
	}

	return				motion_length(M, md, speed, itemModel);
}

void player_hud::update_inertion(Fmatrix& trans)
{
	auto hi = m_attached_items[0] ? m_attached_items[0] : m_attached_items[1];

	if (hi)
	{
		auto& inertion = hi->m_parent_hud_item->CurrentInertionData();

		Fmatrix								xform;
		Fvector& origin						= trans.c; 
		xform								= trans;

		static Fvector						st_last_dir={0,0,0};

		Fvector								diff_dir;
		diff_dir.sub						(xform.k, st_last_dir);

		Fvector last;						last.normalize_safe(st_last_dir);
		float dot							= last.dotproduct(xform.k);
		if (dot<EPS){
			Fvector v0;
			v0.crossproduct					(st_last_dir,xform.k);
			st_last_dir.crossproduct		(xform.k,v0);
			diff_dir.sub					(xform.k, st_last_dir);
		}

		st_last_dir.mad						(diff_dir, inertion.TendtoSpeed*Device.fTimeDelta);
		if (!CCameraRig::Enabled())
		{
			origin.mad						(diff_dir, inertion.OriginOffset);
		}

		float pitch							= angle_normalize_signed(xform.k.getP());
		origin.mad							(xform.k,	-pitch * inertion.PitchOffsetD);
		origin.mad							(xform.i,	-pitch * inertion.PitchOffsetR);
		origin.mad							(xform.j,	-pitch * inertion.PitchOffsetN);
	}
}

extern u32 hud_adj_mode;

void player_hud::UpdateWeaponOffset(u32 delta)
{
	static const bool isInertion = EngineExternal()[EEngineExternalGame::EnableWeaponInertion];

	if (!isInertion || hud_adj_mode || Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)])
	{
		return;
	}

	CActor* actor = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_actor() : nullptr;

	if (actor == nullptr)
	{
		return;
	}

	if (attachable_hud_item* item = attached_item(0))
	{
		item->UpdateInertion(delta, actor);
	}

	if (attachable_hud_item* item = attached_item(1))
	{
		item->UpdateInertion(delta, actor);
	}
}

void player_hud::OnHudMotionStarted(const shared_str& motion)
{
	if (LeftHandAttachWeight > 0.f && m_model != nullptr && strstr(motion.c_str(), "anm_idle") == motion.c_str())
	{
		for (u16 bone : left_arm_subtree())
		{
			m_model->LL_SetInertializationBone(bone, false);
		}

		LeftHandAttachSnapFrame = Device.dwFrame;

		const attachable_hud_item* item = m_attached_items[0];
		const float arm_return = item != nullptr ? (1.f - LeftHandAttachArmWeight) * item->LeftHandAttachBlend : 0.f;
		LeftHandAttachHold = CCameraRig::HudInertialization() > 0.f ? 0.f : std::max(std::max(item != nullptr ? item->IdleFade : 0.f, 0.3f), arm_return) + 0.05f;
	}
	else
	{
		LeftHandAttachHold = 0.f;
	}

	attachable_hud_item* item = m_attached_items[0];
	const u8 mode = item != nullptr && item->LeftHandAttachBlend > 0.f && CCameraRig::Enabled() ? item->LeftHandAttachMotionMode(motion) : 0;
	LeftHandAttachReaching = mode != 0;
	LeftHandAttachAnimatedArm = mode == 2 || (mode == 0 && LeftHandAttachAnimatedArm && LeftHandAttachWeight > 0.f);

	if (item == nullptr || item->LeftHandAttachBlend <= 0.f || !CCameraRig::Enabled() || m_model == nullptr)
	{
		return;
	}

	bool shown = LeftHandAttachWeight >= 1.f;
	if (!shown)
	{
		const player_hud_motion* anm = item->m_hand_motions.find_motion(item->LeftHandAttachTarget);
		shown = anm != nullptr && !anm->m_animations.empty() && hud_motion_amount(m_model, anm->m_animations[0].mid) >= 0.99f;
	}

	if (LeftHandAttachReaching && shown)
	{
		LeftHandAttachWeight = 1.f;
	}
}

bool player_hud::FingerGripReference(attachable_hud_item* item)
{
	shared_str idle = "anm_idle";
	if (CHudItem* hud_item = item->m_parent_hud_item)
	{
		const shared_str current = hud_item->SetCurrentStateAnimation(hud_item->SetCurrentIdleAnimation());
		if (item->m_hand_motions.find_motion(current) != nullptr)
		{
			idle = current;
		}
	}

	if (item->FingerGripPoseMotion != idle)
	{
		item->FingerGripPoseMotion = idle;
		item->FingerGripPose.clear();

		const player_hud_motion* anm = item->m_hand_motions.find_motion(idle);
		if (m_model != nullptr && anm != nullptr && !anm->m_animations.empty())
		{
			hud_motion_local_pose(m_model, anm->m_animations[0].mid, 0.f, FingerGripBones, item->FingerGripPose, &item->FingerGripPos);
		}
	}

	return !item->FingerGripPose.empty() && item->FingerGripPose.size() == FingerGripBones.size();
}

bool player_hud::ArmRetargetReference(attachable_hud_item* item, const xr_vector<u16>& bones)
{
	const MotionID motion = hud_dominant_motion(m_model);
	if (!motion.valid() || !item->LeftHandAttachRefValid)
	{
		return false;
	}

	if (ArmRetargetMotion == motion)
	{
		return ArmRetargetPose.size() == bones.size();
	}

	ArmRetargetMotion = motion;
	ArmRetargetPose.clear();

	CMotion* root = m_model->LL_GetRootMotion(motion);
	if (root == nullptr)
	{
		return false;
	}

	const u16 ancor = m_ancors[item->m_attach_place_idx];
	const float length = root->GetLength();
	float best_time = 0.f;
	float best = flt_max;
	for (float time = 0.f; time <= length; time += SAMPLE_SPF)
	{
		Fmatrix ancor_ms;
		if (!m_model->LL_GetMotionBoneTransform(motion, time, ancor, ancor_ms))
		{
			return false;
		}

		const float deviation = hud_ancor_deviation(ancor_ms, item->LeftHandAttachAncorRef);
		if (deviation < best)
		{
			best = deviation;
			best_time = time;
		}
	}

	if (bones.empty() || !m_model->LL_GetMotionBoneTransform(motion, best_time, bones[0], ArmRetargetRootRef))
	{
		return false;
	}

	return hud_motion_local_pose(m_model, motion, best_time, bones, ArmRetargetPose) && ArmRetargetPose.size() == bones.size();
}

xr_vector<u16> player_hud::left_hand_fingers() const
{
	xr_vector<u16> bones;
	if (m_model == nullptr || LeftIkChain[2] == BI_NONE)
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	bones.push_back(LeftIkChain[2]);
	for (size_t i = 0; i < bones.size(); ++i)
	{
		for (CBoneData* child : kinematics->LL_GetData(bones[i]).children)
		{
			bones.push_back(child->GetSelfID());
		}
	}

	bones.erase(bones.begin());
	return bones;
}

xr_vector<u16> player_hud::left_arm_subtree() const
{
	xr_vector<u16> bones = left_arm_blend_bones();
	if (bones.empty())
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	bones.resize(1);
	for (size_t i = 0; i < bones.size(); ++i)
	{
		for (CBoneData* child : kinematics->LL_GetData(bones[i]).children)
		{
			bones.push_back(child->GetSelfID());
		}
	}

	return bones;
}

void player_hud::OnShotAnimation(const attachable_hud_item* item)
{
	if (item == nullptr || !CCameraRig::Enabled())
	{
		return;
	}

	const float variation = item->ShotVariation;
	ShotRotScale.set(1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation));
	ShotPosScale.set(1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation));
	ShotTwist.set(::Random.randFs(item->ShotTwist), ::Random.randFs(item->ShotTwist));

	const SCameraRigParams& rig = CCameraRig::Params();
	++ShotFingers.BurstShots;
	const float base = rig.shot_fingers_chance;
	const float chance = base > 0.f ? std::min(base + rig.shot_fingers_burst_chance * float(ShotFingers.BurstShots - 1), std::max(rig.shot_fingers_max_chance, base)) : 0.f;
	ShotFingers.Pending = chance > 0.f && ::Random.randF() < chance;
	ShotFingers.SinceShot = 0.f;
	ShotFingers.Stopping = ShotFingers.Active;
}

static xr_vector<u16> hud_bone_descendants(IKinematics* kinematics, u16 bone, u16 exclude)
{
	xr_vector<u16> bones = { bone };
	for (size_t i = 0; i < bones.size(); ++i)
	{
		for (CBoneData* child : kinematics->LL_GetData(bones[i]).children)
		{
			if (child->GetSelfID() != exclude)
			{
				bones.push_back(child->GetSelfID());
			}
		}
	}

	bones.erase(bones.begin());
	return bones;
}

void player_hud::StartShotFingers(attachable_hud_item* item)
{
	SHudShotFingers& S = ShotFingers;
	const SHudRetargetLayer* layer = nullptr;
	for (const auto& [alias, it] : item->RetargetLayers)
	{
		if (alias == "anm_idle")
		{
			layer = &it;
			break;
		}

		if (layer == nullptr && strstr(alias.c_str(), "anm_idle") == alias.c_str())
		{
			layer = &it;
		}
	}

	const MotionID motion = layer != nullptr && layer->Motion.size() != 0 ? m_model->ID_Cycle_Safe(layer->Motion) : MotionID();
	CMotionDef* def = motion.valid() ? m_model->LL_GetMotionDef(motion) : nullptr;
	if (def == nullptr)
	{
		return;
	}

	const xr_string prefix = READ_IF_EXISTS(pSettings, r_string, "camera_rig", "shot_fingers_mark", "mm_shoot");
	const float speed = def->Speed() <= EPS ? 1.f : def->Speed();
	CMotion* root = m_model->LL_GetRootMotion(motion);
	const float length = root != nullptr ? root->GetLength() / speed : flt_max;

	xr_vector<std::pair<float, float>> intervals;
	for (const motion_marks& marks : def->marks)
	{
		const xr_string_view mark_name = marks.name.c_str();
		if (!mark_name.starts_with(prefix) || mark_name.size() != prefix.size() && mark_name[prefix.size()] != '_')
		{
			continue;
		}

		for (const motion_marks::interval& interval : marks.intervals)
		{
			intervals.emplace_back(interval.first, interval.second);
		}
	}

	std::sort(intervals.begin(), intervals.end());
	for (size_t i = 0; i < intervals.size(); ++i)
	{
		const float next = i + 1 < intervals.size() ? intervals[i + 1].first : length;
		intervals[i].second = std::min({ intervals[i].second, length, next });
	}

	std::erase_if(intervals, [](const std::pair<float, float>& it) { return it.second <= it.first + EPS_L; });
	if (intervals.empty())
	{
		return;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	S.Full = ::Random.randF() < CCameraRig::Params().shot_fingers_full_chance;
	if (S.Full)
	{
		S.Bones = hud_bone_descendants(kinematics, kinematics->LL_GetBoneRoot(), BI_NONE);
	}
	else
	{
		S.Bones.clear();
		const u16 ancor = m_ancors[item->m_attach_place_idx];
		for (const u16 wrist : { RightIkChain[2], LeftIkChain[2] })
		{
			if (wrist != BI_NONE)
			{
				const xr_vector<u16> fingers = hud_bone_descendants(kinematics, wrist, ancor);
				S.Bones.insert(S.Bones.end(), fingers.begin(), fingers.end());
			}
		}
	}

	const std::pair<float, float>& interval = intervals[::Random.randI(intervals.size())];
	S.Speed = speed;
	S.First = interval.first;
	S.Second = interval.second;
	S.Motion = motion;
	if (S.Bones.empty() || !hud_motion_local_pose(m_model, motion, S.First * S.Speed, S.Bones, S.Reference, &S.ReferencePos))
	{
		return;
	}

	S.Time = 0.f;
	S.Fade = 1.f;
	S.Stopping = false;
	S.Active = true;
}

void player_hud::UpdateShotFingers(bool BeforeIk)
{
	SHudShotFingers& S = ShotFingers;
	attachable_hud_item* item = m_attached_items[0];
	if (item == nullptr || item->m_model_combined || m_model == nullptr || !CCameraRig::Enabled())
	{
		S = {};
		return;
	}

	if (!BeforeIk)
	{
		if (S.Active && !S.Full)
		{
			ApplyShotFingers();
		}

		return;
	}

	const float blend = CCameraRig::Params().shot_fingers_blend;
	const bool idle = item->m_parent_hud_item->GetState() == CHUDState::eIdle;
	if (idle)
	{
		S.BurstShots = 0;
	}

	if (S.Pending)
	{
		S.SinceShot += Device.fTimeDelta;
		if (!S.Active && idle && S.SinceShot >= CCameraRig::Params().shot_fingers_delay)
		{
			S.Pending = false;
			StartShotFingers(item);
		}
	}

	if (!S.Active)
	{
		return;
	}

	S.Time += Device.fTimeDelta;
	if (S.Stopping)
	{
		S.Fade = blend > EPS ? S.Fade - Device.fTimeDelta / blend : 0.f;
	}

	const float time = S.First + S.Time;
	if (time >= S.Second || S.Fade <= 0.f)
	{
		S.Active = false;
		return;
	}

	const float edge = std::min(blend, (S.Second - S.First) * 0.5f);
	S.Weight = hud_smooth(hud_mark_ramp(time - S.First, edge) * hud_mark_ramp(S.Second - time, edge) * std::clamp(S.Fade, 0.f, 1.f));
	if (S.Full)
	{
		ApplyShotFingers();
	}
}

void player_hud::ApplyShotFingers()
{
	SHudShotFingers& S = ShotFingers;
	const float time = S.First + S.Time;
	const float weight = S.Weight;
	xr_vector<Fquaternion> pose;
	xr_vector<Fvector> positions;
	if (weight <= 0.f || !hud_motion_local_pose(m_model, S.Motion, time * S.Speed, S.Bones, pose, &positions) || pose.size() != S.Reference.size() || positions.size() != S.ReferencePos.size())
	{
		return;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	for (size_t i = 0; i < S.Bones.size(); ++i)
	{
		const u16 bone = S.Bones[i];
		Fmatrix reference, sample;
		reference.rotation(S.Reference[i]);
		sample.rotation(pose[i]);
		const Fmatrix delta = Fmatrix().mul_43(Fmatrix().invert(reference), sample);

		const Fmatrix parent = kinematics->LL_GetTransform(kinematics->LL_GetData(bone).GetParentID());
		const Fmatrix bone_ms = kinematics->LL_GetTransform(bone);
		const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent), bone_ms);
		Fmatrix moved_local = Fmatrix().mul_43(local, hud_weighted_transform(delta, weight));
		moved_local.c.mad(local.c, Fvector().sub(positions[i], S.ReferencePos[i]), weight);
		const Fmatrix moved = Fmatrix().mul_43(parent, moved_local);
		CCameraRig::TransformBone(kinematics, bone, Fmatrix().mul_43(moved, Fmatrix().invert(bone_ms)));
	}
}

attachable_hud_item* player_hud::create_hud_item(const shared_str& sect)
{
	xr_vector<attachable_hud_item*>::iterator it = m_pool.begin();
	xr_vector<attachable_hud_item*>::iterator it_e = m_pool.end();
	for(;it!=it_e;++it)
	{
		attachable_hud_item* itm = *it;
		if(itm->m_sect_name==sect)
			return itm;
	}
	attachable_hud_item* res	= new attachable_hud_item(this);
	res->load					(sect);
	res->m_hand_motions.load	(res->m_model_combined ? res->m_model->dcast_PKinematicsAnimated() : m_model, sect, res->m_model->dcast_PKinematicsAnimated());
	m_pool.push_back			(res);

	return	res;
}

void player_hud::RemoveHudItem(const shared_str& sect)
{
	attachable_hud_item* itm = nullptr;
	for (attachable_hud_item* Item : m_pool)
	{
		if (Item->m_sect_name == sect)
		{
			itm = Item;
		}
	}

	auto Iter = std::find(m_pool.begin(), m_pool.end(), itm);
	m_pool.erase(Iter);

	xr_delete(itm);
}

bool player_hud::allow_activation(CHudItem* item)
{
	if (m_animator_item)
		return false;

	if(m_attached_items[1])
		return m_attached_items[1]->m_parent_hud_item->CheckCompatibility(item);
	else
	{
		CEntity* pEntity = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_entity() : nullptr;
		if (pEntity)
		{
			CActor* pActor = pEntity->cast_actor();
			if(pActor)
			{
				CHudItem* pDetector = pActor->inventory().ItemFromSlot(DEVICE_SLOT) ? pActor->inventory().ItemFromSlot(DEVICE_SLOT)->cast_hud_item() : nullptr;
				if(pDetector && pDetector->GetState()!=CHUDState::eHidden)
					return pDetector->CheckCompatibility(item);
			}
		}
	}

	return true;
}

void player_hud::attach_item(CHudItem* item)
{
	attachable_hud_item* pi = create_hud_item(item->HudSection());
	int item_idx = pi->m_attach_place_idx;

	if (m_attached_items[item_idx] != pi || pi->m_parent_hud_item != item)
	{
		if (m_attached_items[item_idx])
		{
			m_attached_items[item_idx]->m_parent_hud_item->on_b_hud_detach();
			StopAllBlendAnms(true);
		}

		m_attached_items[item_idx] = pi;
		pi->m_parent_hud_item = item;
		pi->m_parent_hud_item->m_disable_random_animations = pi->m_model_combined;

		if (item_idx == 0 && m_model != nullptr)
		{
			for (u16 pid = 0, count = m_model->partitions().count(); pid < count; ++pid)
			{
				m_model->LL_FadeCycle(pid, 1.f / pi->AdditiveBlendOut, 1 << ADDITIVE_CHANNEL);
			}

			StopSprintAdditive(pi->SprintLayerBlend());
			SprintAdditiveItem = pi;
			ResolveSprintAdditive(pi);

			RetargetWeight = 0.f;
			RetargetLeftValid = false;
			RetargetRightValid = false;
			ArmRetargetMotion = MotionID();
			m_model->LL_ResetInertialization();
		}

		if (IKinematicsAnimated* animated = pi->m_model->dcast_PKinematicsAnimated())
		{
			animated->LL_ResetInertialization();
		}

		pi->AdditivePlaying = false;
		pi->RetargetPlaying = false;

		if (item_idx == 0 && m_attached_items[1])
		{
			m_attached_items[1]->m_parent_hud_item->CheckCompatibility(item);
		}
		item->item().load_attachments(pi->m_model);
		item->on_a_hud_attach();
		UpdateMovementLayers(true);
	}

	pi->m_parent_hud_item = item;
	pi->m_parent_hud_item->m_disable_random_animations = pi->m_model_combined;
}

void player_hud::RestoreHandBlends(const char* ignored_part)
{
	u16 part_id			= m_model->partitions().part_id(ignored_part);
	u32 blends_count	= m_model->LL_PartBlendsCount(part_id);
	for(u32 blend_id=0; blend_id<blends_count; ++blend_id)
	{
		CBlend* parallel_blend			= m_model->LL_PartBlend(part_id, blend_id);
		if(!parallel_blend)
			continue;

		MotionID M			= parallel_blend->motionID;

		u16 parts_count	= m_model->partitions().count();
		for(u16 pid=0; pid<parts_count; ++pid)
		{
			if(pid==part_id)
				continue;
			CBlend* B			= m_model->PlayCycle(pid, M, true);
			if( parallel_blend->blend_state() != CBlend::eFREE_SLOT )
			{
				u16 bop				= B->bone_or_part;
				*B					= *parallel_blend;
				B->bone_or_part		= bop;
			}
		}
	}
}

void player_hud::detach_item_idx(u16 idx)
{
	if (attached_item(idx) == nullptr)
	{
		return;
	}

	m_attached_items[idx]->m_parent_hud_item->on_b_hud_detach();
	StopAllBlendAnms(true);

	CObject* entity = Level().CurrentControlEntity();
	CActor* actor = entity != nullptr ? entity->cast_actor() : nullptr;
	if (actor != nullptr && (idx == 0 || m_attached_items[0] == nullptr))
	{
		CCameraRig& rig = actor->Cameras().Rig();
		rig.StopChannel(crcMotion);
		rig.StopChannel(crcAction);
		rig.StopChannel(crcShot);
		rig.StopChannel(crcSprint);
	}

	m_attached_items[idx]->m_parent_hud_item = nullptr;
	m_attached_items[idx] = nullptr;

	if (idx == 1 && m_attached_items[0])
	{
		m_attached_items[0]->m_parent_hud_item->OnMovementChanged(mcAnyMove);
		RestoreHandBlends("right_hand");
	}
	else if (idx == 0 && m_attached_items[1])
	{
		m_model->PlayCycle(2, m_model->ID_Cycle("hand_idle_doun"), false);
		m_attached_items[1]->m_parent_hud_item->OnMovementChanged(mcAnyMove);
	}
}

void player_hud::detach_item(CHudItem* item)
{
	if( nullptr==item->HudItemData() )		return;
	u16 item_idx						= item->HudItemData()->m_attach_place_idx;

	if( m_attached_items[item_idx]==item->HudItemData() )
	{
		detach_item_idx	(item_idx);
	}
}

void player_hud::calc_transform(u16 attach_slot_idx, const Fmatrix& offset, Fmatrix& result, bool forceLegacyTransform)
{
	if (m_model && !forceLegacyTransform)
	{
		Fmatrix ancor_m = m_model->dcast_PKinematics()->LL_GetTransform(m_ancors[attach_slot_idx]);
		result.mul(m_transform, ancor_m);
	}
	else
	{
		result.set(m_transform);
	}
	result.mulB_43			(offset);
}

void player_hud::UpdateMovementLayers(bool reload_anims)
{
	CObject* current_entity = Level().CurrentEntity();
	CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;

	if (pActor == nullptr)
	{
		return;
	}

	SyncSprintBlend();

	bool stop_force = m_attached_items[0] == nullptr && m_attached_items[1] == nullptr && m_animator_item == nullptr;

	if (reload_anims && m_attached_items[0] != nullptr && m_attached_items[0]->m_parent_hud_item != nullptr)
	{
		for (u8 i = 0; i < EMovementLayers::eLayersCount; i++)
		{
			if (!m_movement_layers[i])
			{
				continue;
			}

			const auto& blend_params = m_attached_items[0]->m_parent_hud_item->HudDesc().m_sMovementBlendParams[i];
			if (blend_params.has_motion)
			{
				m_movement_layers[i]->Reload(blend_params.camera_name, blend_params.speed_power, blend_params.blend_params);
			}
			else
			{
				m_movement_layers[i]->Reload(m_default_movement_layers_params[i].camera_name, m_default_movement_layers_params[i].speed_power, m_default_movement_layers_params[i].blend_params);
			}
		}
	}

	float phase = -1.0f;
	float phase_weight = 0.0f;
	for (u8 i = 0; i < EMovementLayers::eStrafeLeft; i++)
	{
		movement_layer* anm = m_movement_layers[i];
		if (anm != nullptr && anm->blend_scale > phase_weight && anm->Phase() >= 0.0f)
		{
			phase_weight = anm->blend_scale;
			phase = anm->Phase();
		}
	}

	for (movement_layer* anm : m_movement_layers)
	{
		if (!anm)
		{
			continue;
		}

		anm->Stop(stop_force);
	}

	bool need_blend_0 = m_attached_items[0] != nullptr && m_attached_items[0]->m_parent_hud_item != nullptr && m_attached_items[0]->m_parent_hud_item->NeedMovementBlend() || m_animator_item != nullptr && m_animator_item->NeedMovementBlend();
	bool need_blend_1 = m_attached_items[1] != nullptr && m_attached_items[1]->m_parent_hud_item != nullptr && m_attached_items[1]->m_parent_hud_item->NeedMovementBlend() || m_animator_item != nullptr && m_animator_item->NeedMovementBlend();

	if (!need_blend_0 && !need_blend_1)
	{
		return;
	}

	CWeapon* pWeapon = m_attached_items[0] != nullptr && m_attached_items[0]->m_parent_hud_item != nullptr ? m_attached_items[0]->m_parent_hud_item->cast_weapon() : nullptr;

	u32 state = pActor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);

	if (pActor->AnyMove())
	{
		if (m_movement_layers[EMovementLayers::eAimWalk] != nullptr && pWeapon != nullptr && pWeapon->IsZoomed())
		{
			m_movement_layers[EMovementLayers::eAimWalk]->Play(true, phase);
		}
		else if (m_movement_layers[EMovementLayers::eSprint] != nullptr && (state & ACTOR_DEFS::EMoveCommand::mcSprint) != 0)
		{
			m_movement_layers[EMovementLayers::eSprint]->Play(true, phase);
		}
		else if (m_movement_layers[EMovementLayers::eWalkSlow] != nullptr && (state & ACTOR_DEFS::EMoveCommand::mcAccel) != 0 && (state & ACTOR_DEFS::EMoveCommand::mcCrouch) == 0)
		{
			m_movement_layers[EMovementLayers::eWalkSlow]->Play(true, phase);
		}
		else if (m_movement_layers[EMovementLayers::eCrouch] != nullptr && (state & ACTOR_DEFS::EMoveCommand::mcAccel) == 0 && (state & ACTOR_DEFS::EMoveCommand::mcCrouch) != 0)
		{
			m_movement_layers[EMovementLayers::eCrouch]->Play(true, phase);
		}
		else if (m_movement_layers[EMovementLayers::eCrouchSlow] != nullptr && (state & ACTOR_DEFS::EMoveCommand::mcAccel) != 0 && (state & ACTOR_DEFS::EMoveCommand::mcCrouch) != 0)
		{
			m_movement_layers[EMovementLayers::eCrouchSlow]->Play(true, phase);
		}
		else if (m_movement_layers[EMovementLayers::eWalk] != nullptr)
		{
			m_movement_layers[EMovementLayers::eWalk]->Play(true, phase);
		}

		const u16 strafe = (state & ACTOR_DEFS::EMoveCommand::mcSprint) != 0 ? u16(-1) : (state & ACTOR_DEFS::EMoveCommand::mcLStrafe) != 0 ? EMovementLayers::eStrafeLeft : (state & ACTOR_DEFS::EMoveCommand::mcRStrafe) != 0 ? EMovementLayers::eStrafeRight : u16(-1);
		if (strafe != u16(-1) && m_movement_layers[strafe] != nullptr)
		{
			m_movement_layers[strafe]->Play(true, phase);
		}
	}
	else
	{
		if (m_movement_layers[EMovementLayers::eIdleAim] != nullptr && pWeapon != nullptr && pWeapon->IsZoomed())
		{
			m_movement_layers[EMovementLayers::eIdleAim]->Play(true, phase);
		}
		else if (m_movement_layers[EMovementLayers::eIdle] != nullptr)
		{
			m_movement_layers[EMovementLayers::eIdle]->Play(true, phase);
		}
	}
}

void player_hud::SyncSprintBlend()
{
	CObject* entity = Level().CurrentEntity();
	CActor* actor = entity != nullptr ? entity->cast_actor() : nullptr;
	CHudItem* item = m_attached_items[0] != nullptr ? m_attached_items[0]->m_parent_hud_item : nullptr;

	bool sprint = false;
	if (actor != nullptr && item != nullptr && CCameraRig::Enabled())
	{
		const u32 state = item->GetState();
		const bool idle = state == CHUDState::eIdle || state == CHUDState::eSprintStart || state == CHUDState::eSprintEnd;
		CWeapon* weapon = item->cast_weapon();
		const bool sprint_action = weapon != nullptr && smart_cast<CWeaponPistol*>(weapon) == nullptr && (state == CWeapon::eReload || state == CWeapon::eSwitchMode || state == CWeapon::eSafemodeSwitch);
		const bool unpended = !item->IsPending() && state != CHUDState::eHiding && state != CHUDState::eHidden;
		sprint = (idle || sprint_action || unpended) && (actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal) & ACTOR_DEFS::EMoveCommand::mcSprint) != 0;
	}

	UpdateSprintAdditive(sprint);
	Sprinting = sprint;
}

bool player_hud::SprintAdditiveEnabled() const
{
	return m_model != nullptr && m_attached_items[0] != nullptr && m_attached_items[0] == SprintAdditiveItem && SprintLoopMotion.valid();
}

void player_hud::ResolveSprintAdditive(attachable_hud_item* item)
{
	SprintStartMotion = MotionID();
	SprintLoopMotion = MotionID();
	SprintEndMotion = MotionID();
	SprintReferenceMotion = MotionID();
	if (m_model == nullptr || item == nullptr || !item->SprintLayered())
	{
		return;
	}

	SprintStartMotion = item->AliasMotion("anm_idle_sprint_start");
	SprintLoopMotion = item->AliasMotion("anm_idle_sprint");
	SprintEndMotion = item->AliasMotion("anm_idle_sprint_end");

	const shared_str reference = item->SprintLayerReference();
	if (reference.size() != 0)
	{
		SprintReferenceMotion = m_model->ID_Cycle_Safe(reference);
	}
}

bool player_hud::PlaySprintAdditive(attachable_hud_item* item, MotionID motion, bool loop, float progress, float power, bool inertialize)
{
	if (m_model == nullptr || !motion.valid())
	{
		return false;
	}

	CMotionDef* def = m_model->LL_GetMotionDef(motion);
	const float rate = 1.f / item->SprintLayerBlend();
	float Accrue = rate;
	if (motion == SprintStartMotion)
	{
		float BlendIn = item->SprintBlendIn > 0.f ? item->SprintBlendIn : item->SprintLayerBlend();
		if (CMotion* Root = m_model->LL_GetRootMotion(motion))
		{
			const float Speed = std::max(def != nullptr ? def->Speed() : 1.f, EPS);
			BlendIn = std::min(BlendIn, Root->GetLength() / Speed * (1.f - std::clamp(progress, 0.f, 1.f)));
		}

		Accrue = 1.f / std::max(BlendIn, 0.01f);
	}
	const bool Inertial = inertialize && CCameraRig::HudInertialization() > 0.f;
	for (u16 part = 0, count = m_model->partitions().count(); part < count; ++part)
	{
		if (Inertial)
		{
			SprintLayerSeam = true;
			m_model->LL_InertializePart(part, item->SprintLayerBlend(), LAYER_ADDITIVE_CHANNEL);
			m_model->LL_CloseCycle(part, 1 << LAYER_ADDITIVE_CHANNEL);
		}
		else
		{
			m_model->LL_FadeCycle(part, rate, 1 << LAYER_ADDITIVE_CHANNEL);
		}

		CBlend* B = m_model->LL_PlayCycle(part, motion, !Inertial, Accrue, rate, def != nullptr ? def->Speed() : 1.f, !loop, nullptr, nullptr, LAYER_ADDITIVE_CHANNEL);
		if (B != nullptr)
		{
			B->additive_ref = SprintReferenceMotion;
			B->fall_at_end = false;
			B->blendPower = std::clamp(power, 0.05f, 1.f);
			B->timeCurrent = std::clamp(progress, 0.f, 1.f) * std::max(B->timeTotal - END_EPS, 0.f);
		}
	}

	CObject* entity = Level().CurrentControlEntity();
	CActor* actor = entity != nullptr ? entity->cast_actor() : nullptr;
	const char* alias = motion == SprintStartMotion ? "anm_idle_sprint_start" : motion == SprintEndMotion ? "anm_idle_sprint_end" : nullptr;
	if (actor != nullptr && alias != nullptr)
	{
		shared_str name;
		float camera_speed = CalcMotionSpeed(alias);
		for (const player_hud_motion& it : item->m_hand_motions.m_anims)
		{
			if (it.m_alias_name == alias)
			{
				name = it.m_base_name;
				camera_speed *= it.m_anim_speed;
				break;
			}
		}

		string_path camera;
		xr_strconcat(camera, "camera_effects\\weapon\\", name.c_str(), ".anm");
		CCameraRig& rig = actor->Cameras().Rig();
		if (name.size() != 0 && rig.PlayAnim(camera, crcSprint, camera_speed, false))
		{
			rig.SeekChannel(crcSprint, progress);
		}
		else
		{
			rig.StopChannel(crcSprint);
		}
	}

	return true;
}

void player_hud::StopSprintAdditive(float blend)
{
	SprintAdditiveStage = ESprintAdditiveStage::None;
	CObject* entity = Level().CurrentControlEntity();
	if (CActor* actor = entity != nullptr ? entity->cast_actor() : nullptr)
	{
		actor->Cameras().Rig().StopChannel(crcSprint);
	}

	if (m_model == nullptr)
	{
		return;
	}

	for (u16 part = 0, count = m_model->partitions().count(); part < count; ++part)
	{
		m_model->LL_FadeCycle(part, 1.f / std::max(blend, 0.01f), 1 << LAYER_ADDITIVE_CHANNEL);
	}
}

float player_hud::SprintAdditiveProgress(MotionID motion) const
{
	if (m_model == nullptr || !motion.valid())
	{
		return -1.f;
	}

	for (u16 part = 0, count = m_model->partitions().count(); part < count; ++part)
	{
		for (u32 i = 0, blends = m_model->LL_PartBlendsCount(part); i < blends; i++)
		{
			const CBlend* B = m_model->LL_PartBlend(part, i);
			if (B != nullptr && B->channel == LAYER_ADDITIVE_CHANNEL && B->motionID == motion && B->blend_state() != CBlend::eFREE_SLOT && B->blend_state() != CBlend::eFalloff)
			{
				const float length = B->timeTotal - END_EPS;
				return length > EPS ? std::clamp(B->timeCurrent / length, 0.f, 1.f) : 1.f;
			}
		}
	}

	return -1.f;
}

void player_hud::FreezeSprintAdditive(MotionID motion)
{
	if (m_model == nullptr || !motion.valid())
	{
		return;
	}

	for (u16 Part = 0, Count = m_model->partitions().count(); Part < Count; ++Part)
	{
		for (u32 i = 0, Blends = m_model->LL_PartBlendsCount(Part); i < Blends; i++)
		{
			CBlend* B = m_model->LL_PartBlend(Part, i);
			if (B != nullptr && B->channel == LAYER_ADDITIVE_CHANNEL && B->motionID == motion && B->blend_state() != CBlend::eFREE_SLOT)
			{
				B->playing = false;
			}
		}
	}
}

void player_hud::UpdateSprintAdditive(bool sprint)
{
	attachable_hud_item* item = m_attached_items[0];
	if (item != SprintAdditiveItem)
	{
		if (SprintAdditiveStage != ESprintAdditiveStage::None)
		{
			StopSprintAdditive(SprintAdditiveItem != nullptr ? SprintAdditiveItem->SprintLayerBlend() : 0.2f);
		}

		SprintAdditiveItem = item;
	}

	ResolveSprintAdditive(item);
	if (!SprintAdditiveEnabled())
	{
		if (SprintAdditiveStage != ESprintAdditiveStage::None)
		{
			StopSprintAdditive(item != nullptr ? item->SprintLayerBlend() : 0.2f);
		}

		return;
	}

	if (sprint)
	{
		if (SprintAdditiveStage == ESprintAdditiveStage::Start || SprintAdditiveStage == ESprintAdditiveStage::Loop)
		{
			return;
		}

		float progress = 0.f;
		if (SprintAdditiveStage == ESprintAdditiveStage::End)
		{
			const float end = SprintAdditiveProgress(SprintEndMotion);
			progress = end >= 0.f ? (1.f - end) * SprintEndPower : 0.f;
		}

		if (PlaySprintAdditive(item, SprintStartMotion, false, progress))
		{
			SprintAdditiveStage = ESprintAdditiveStage::Start;
		}
		else if (PlaySprintAdditive(item, SprintLoopMotion, true, 0.f))
		{
			SprintAdditiveStage = ESprintAdditiveStage::Loop;
		}
		else
		{
			StopSprintAdditive(item->SprintLayerBlend());
		}

		return;
	}

	if (SprintAdditiveStage == ESprintAdditiveStage::None || SprintAdditiveStage == ESprintAdditiveStage::End)
	{
		return;
	}

	const u32 state = item->m_parent_hud_item != nullptr ? item->m_parent_hud_item->GetState() : CHUDState::eHidden;
	const bool leaving = state == CHUDState::eHiding || state == CHUDState::eHidden;

	SprintEndPower = 1.f;
	SprintLeftCancel.Active = false;
	const bool CancelStart = SprintAdditiveStage == ESprintAdditiveStage::Start;
	if (CancelStart)
	{
		const float start = SprintAdditiveProgress(SprintStartMotion);
		SprintEndPower = start >= 0.f ? start : 1.f;
		FreezeSprintAdditive(SprintStartMotion);
	}

	CWeapon* end_weapon = item->m_parent_hud_item != nullptr ? item->m_parent_hud_item->cast_weapon() : nullptr;
	const bool to_safemode = end_weapon != nullptr && end_weapon->SprintEndsInSafemode();
	if (!leaving && to_safemode)
	{
		end_weapon->OnSprintToSafemode();
	}

	if (!leaving && !to_safemode && PlaySprintAdditive(item, SprintEndMotion, false, 0.f, SprintEndPower))
	{
		SprintAdditiveStage = ESprintAdditiveStage::End;
		if (CancelStart && SprintLeftFree && item->SprintLeftGripRange > 0.f)
		{
			BeginSprintLeftCancel();
		}
	}
	else
	{
		StopSprintAdditive(item->SprintLayerBlend());
	}
}

float player_hud::SprintLayerDeviation() const
{
	const attachable_hud_item* item = m_attached_items[0];
	if (m_model == nullptr || item == nullptr || item->m_attach_place_idx >= m_ancors.size())
	{
		return 0.f;
	}

	const u16 ancor = m_ancors[item->m_attach_place_idx];
	Fmatrix full, base;
	if (ancor == BI_NONE || !m_model->LL_GetBoneTransformMasked(ancor, u8(-1), full) || !m_model->LL_GetBoneTransformMasked(ancor, u8(~(1 << LAYER_ADDITIVE_CHANNEL)), base))
	{
		return 0.f;
	}

	const Fmatrix delta = Fmatrix().mul_43(full, Fmatrix().invert(base));
	Fquaternion rotation;
	rotation.set(delta);
	const float angle = 2.f * acosf(std::min(std::abs(rotation.w), 1.f)) / deg2rad(30.f);
	const float distance = delta.c.magnitude() / 0.1f;
	return _sqrt(angle * angle + distance * distance);
}

float player_hud::SprintPoseTarget()
{
	if (m_model == nullptr)
	{
		return 0.f;
	}

	const float weight = hud_smooth(hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL));
	if (SprintAdditiveStage == ESprintAdditiveStage::Start || SprintAdditiveStage == ESprintAdditiveStage::Loop)
	{
		if (SprintAdditiveStage == ESprintAdditiveStage::Start)
		{
			const float progress = SprintAdditiveProgress(SprintStartMotion);
			SprintPoseShape = progress >= 0.f ? hud_smooth(progress) : SprintPoseShape;
		}
		else
		{
			SprintPoseShape = 1.f;
		}

		SprintExitDeviation = -1.f;
		return weight * SprintPoseShape;
	}

	if (weight <= 0.f)
	{
		SprintExitDeviation = -1.f;
		return 0.f;
	}

	const float deviation = SprintLayerDeviation();
	if (SprintExitDeviation < 0.f)
	{
		SprintExitDeviation = deviation;
		SprintExitPose = SprintTarget;
	}

	if (SprintExitDeviation > 1e-3f)
	{
		return SprintExitPose * std::clamp(deviation / SprintExitDeviation, 0.f, 1.f);
	}

	if (SprintAdditiveStage == ESprintAdditiveStage::End)
	{
		const float progress = SprintAdditiveProgress(SprintEndMotion);
		SprintPoseShape = progress >= 0.f ? 1.f - hud_smooth(progress) : SprintPoseShape;
	}

	return weight * SprintPoseShape;
}

float player_hud::AimReloadPose() const
{
	return std::clamp(AimReloadPoseValue, 0.f, 1.f);
}

float player_hud::AimReloadPush() const
{
	return std::clamp(AimReloadPushValue, 0.f, 1.f);
}

float player_hud::AimReloadDrop() const
{
	return std::clamp(AimReloadDropValue, 0.f, 1.f);
}

void player_hud::UpdateCarryPose(CActor* actor)
{
	attachable_hud_item* item = m_attached_items[0];
	CWeapon* weapon = item != nullptr && item->m_parent_hud_item != nullptr ? item->m_parent_hud_item->cast_weapon() : nullptr;
	if (!CCameraRig::Enabled() || actor == nullptr || weapon == nullptr)
	{
		CrouchPoseValue = 0.f;
		CrouchPoseVelocity = 0.f;
		WalkPoseValue = 0.f;
		WalkPoseVelocity = 0.f;
		WalkTime = 0.f;
		CrouchTime = 0.f;
		return;
	}

	const u32 state = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
	const bool zoom = weapon->IsZoomed() || weapon->m_bIsAimStarted;
	const bool sprint = (state & ACTOR_DEFS::EMoveCommand::mcSprint) != 0;
	const bool crouch = (state & ACTOR_DEFS::EMoveCommand::mcCrouch) != 0 && !zoom;
	const bool walking = (state & ACTOR_DEFS::EMoveCommand::mcAnyMove) != 0 && !sprint && !zoom && weapon->GetState() == CHUDState::eIdle;

	WalkTime = walking ? WalkTime + Device.fTimeDelta : 0.f;
	const float walk_target = walking && WalkTime >= item->WalkPoseDelay ? 1.f : 0.f;
	CrouchTime = crouch ? CrouchTime + Device.fTimeDelta : 0.f;
	const float crouch_target = crouch && CrouchTime >= item->CrouchPoseDelay ? 1.f : 0.f;

	const float crouch_half_life = crouch_target < CrouchPoseValue && item->CrouchPoseInertiaOut >= 0.f ? item->CrouchPoseInertiaOut : item->CrouchPoseInertia;
	HudSpring::Step(CrouchPoseValue, CrouchPoseVelocity, crouch_target, crouch_half_life, item->CarryPoseDamping, Device.fTimeDelta);
	HudSpring::Step(WalkPoseValue, WalkPoseVelocity, walk_target, walk_target < WalkPoseValue ? item->WalkPoseInertiaOut : item->WalkPoseInertia, item->CarryPoseDamping, Device.fTimeDelta);
}

void player_hud::TriggerScriptLayersInertia(float half_life)
{
	if (half_life <= 0.f)
	{
		return;
	}

	ScriptInertiaPending = true;
	ScriptInertiaHalfLife = half_life;
}

Fmatrix player_hud::CarryPoseOffset() const
{
	const attachable_hud_item* item = m_attached_items[0];
	if (item == nullptr || (fis_zero(CrouchPoseValue) && fis_zero(WalkPoseValue)))
	{
		return Fidentity;
	}

	Fvector rot = Fvector().set(0.f, 0.f, 0.f).mad(item->CrouchOffsetRot, CrouchPoseValue).mad(item->WalkOffsetRot, WalkPoseValue);
	rot.mul(PI / 180.f);
	Fmatrix offset;
	offset.setXYZ(rot);
	offset.translate_over(Fvector().set(0.f, 0.f, 0.f).mad(item->CrouchOffsetPos, CrouchPoseValue).mad(item->WalkOffsetPos, WalkPoseValue));
	return offset;
}

void player_hud::UpdateSprintPose()
{
	SprintTarget = SprintPoseTarget();

	const attachable_hud_item* item = m_attached_items[0];
	const float half_life = item != nullptr ? item->SprintPoseInertia : 0.f;
	const CWeapon* weapon = item != nullptr && item->m_parent_hud_item != nullptr ? item->m_parent_hud_item->cast_weapon() : nullptr;
	const float damping = weapon != nullptr && weapon->SafemodeDamping >= 0.f ? weapon->SafemodeDamping : CCameraRig::Params().safemode_damping;
	HudSpring::Step(SprintPose, SprintPoseVelocity, SprintTarget, half_life, damping, Device.fTimeDelta);
	SprintPose = std::clamp(SprintPose, -0.25f, 1.25f);

	if (SprintTarget <= 0.f && std::abs(SprintPose) < 1e-3f && std::abs(SprintPoseVelocity) < 1e-2f)
	{
		SprintPose = 0.f;
		SprintPoseVelocity = 0.f;
	}
}

void player_hud::CutSprintEnd()
{
	if (SprintAdditiveStage == ESprintAdditiveStage::End && SprintAdditiveItem != nullptr)
	{
		StopSprintAdditive(SprintAdditiveItem->SprintLayerBlend());
	}
}

void player_hud::UpdateRetargetLayerWeight()
{
	const attachable_hud_item* item = m_attached_items[0];
	if (m_model == nullptr || item == nullptr || !item->RetargetLayerPlaying)
	{
		return;
	}

	const float Power = std::max(1.f - hud_smooth(hud_channel_weight(m_model, LAYER_ADDITIVE_CHANNEL)), 0.001f);
	for (u16 Part = 0, Count = m_model->partitions().count(); Part < Count; ++Part)
	{
		for (u32 i = 0, Blends = m_model->LL_PartBlendsCount(Part); i < Blends; i++)
		{
			CBlend* B = m_model->LL_PartBlend(Part, i);
			if (B != nullptr && B->channel == ADDITIVE_CHANNEL && B->motionID == item->RetargetMotion && B->blend_state() != CBlend::eFalloff && B->blend_state() != CBlend::eFREE_SLOT)
			{
				const float Fraction = B->blendPower > EPS ? B->blendAmount / B->blendPower : 1.f;
				B->blendPower = Power;
				B->blendAmount = Fraction * Power;
			}
		}
	}
}

void player_hud::UpdateSprintAdditiveChain()
{
	if (SprintAdditiveStage == ESprintAdditiveStage::None)
	{
		return;
	}

	attachable_hud_item* item = SprintAdditiveItem;
	if (item == nullptr || !SprintAdditiveEnabled())
	{
		StopSprintAdditive(item != nullptr ? item->SprintLayerBlend() : 0.2f);
		return;
	}

	if (SprintAdditiveStage == ESprintAdditiveStage::Loop)
	{
		return;
	}

	const bool start = SprintAdditiveStage == ESprintAdditiveStage::Start;
	const float progress = SprintAdditiveProgress(start ? SprintStartMotion : SprintEndMotion);
	if (progress >= 0.f && progress < 1.f)
	{
		return;
	}

	if (start && PlaySprintAdditive(item, SprintLoopMotion, true, 0.f, 1.f, true))
	{
		SprintAdditiveStage = ESprintAdditiveStage::Loop;
		return;
	}

	StopSprintAdditive(item->SprintLayerBlend());
}

void player_hud::OnMovementChanged(ACTOR_DEFS::EMoveCommand cmd)
{
	CObject* cur_entity = Level().CurrentEntity();
	if (CEntity* pEntity = cur_entity != nullptr ? cur_entity->cast_entity() : nullptr)
	{
		if (CActor* pActor = pEntity->cast_actor())
		{
			if (pActor->HUDview())
			{
				if (cmd == 0)
				{
					if (m_attached_items[0] != nullptr)
					{
						if (m_attached_items[0]->m_parent_hud_item->GetState() == CHUDState::EHudStates::eIdle)
						{
							m_attached_items[0]->m_parent_hud_item->PlayAnimIdle();
						}
					}

					if (m_attached_items[1] != nullptr)
					{
						if (m_attached_items[1]->m_parent_hud_item->GetState() == CHUDState::EHudStates::eIdle)
						{
							m_attached_items[1]->m_parent_hud_item->PlayAnimIdle();
						}
					}
				}
				else
				{
					if (m_attached_items[0] != nullptr)
					{
						m_attached_items[0]->m_parent_hud_item->OnMovementChanged(cmd);
					}

					if (m_attached_items[1] != nullptr)
					{
						m_attached_items[1]->m_parent_hud_item->OnMovementChanged(cmd);
					}
				}
			}
			else
			{
				PIItem active_item = pActor->inventory().ActiveItem();

				if (cmd == 0)
				{
					if (CHudItem* pWeap = active_item != nullptr ? active_item->cast_hud_item() : nullptr)
					{
						if (pWeap->GetState() == CHUDState::EHudStates::eIdle)
						{
							pWeap->PlayAnimIdle();
						}
					}

					if (CCustomDevice* pDevice = pActor->GetDevice(true))
					{
						if (pDevice->GetState() == CHUDState::EHudStates::eIdle)
						{
							pDevice->PlayAnimIdle();
						}
					}
				}
				else
				{
					if (CHudItem* pWeap = active_item != nullptr ? active_item->cast_hud_item() : nullptr)
					{
						if (!pWeap->IsHidden())
						{
							pWeap->OnMovementChanged(cmd);
						}
					}

					if (CCustomDevice* pDevice = pActor->GetDevice(true))
					{
						if (!pDevice->IsHidden())
						{
							pDevice->OnMovementChanged(cmd);
						}
					}
				}
			}
		}
	}

	UpdateMovementLayers();
}

script_layer* player_hud::PlayBlendAnm(const shared_str& name, float speed, float power, Fvector2 blend_factors, bool looped, bool mix, bool restart, u8 part, u8 item, u8 state)
{
	if (restart)
	{
		StopBlendAnm(name);
	}

	for (script_layer* cycle_anim : m_script_layers)
	{
		if (cycle_anim->name == name)
		{
			cycle_anim->Play(name, speed, power, blend_factors, looped, mix, part, item, state);
			return cycle_anim;
		}
	}

	return m_script_layers.emplace_back(new script_layer(name, speed, power, blend_factors, looped, mix, part, item, state));
}

void player_hud::StopBlendAnm(const shared_str& name, bool bForce)
{
	for (script_layer* anm : m_script_layers)
	{
		if (anm->name == name)
		{
			anm->Stop(bForce);
			return;
		}
	}
}

void player_hud::StopAllBlendAnms(bool bForce)
{
	for (script_layer* anm : m_script_layers)
	{
		anm->Stop(bForce);
	}
}

bool player_hud::IsBlendAnmActive(const shared_str& name)
{
	for (script_layer* anm : m_script_layers)
	{
		if (anm->name == name)
		{
			if (anm->active)
			{
				return true;
			}
		}
	}

	return false;
}

void script_layer::CallStartCallback()
{
	if (item == 2)
	{
		for (int i = 0; i < item; i++)
		{
			if (g_player_hud->attached_item(i) && g_player_hud->attached_item(i)->m_parent_hud_item)
			{
				g_player_hud->attached_item(i)->m_parent_hud_item->OnBlendStart(state);
			}
		}
	}
	else
	{
		if (g_player_hud->attached_item(item) && g_player_hud->attached_item(item)->m_parent_hud_item)
		{
			g_player_hud->attached_item(item)->m_parent_hud_item->OnBlendStart(state);
		}
	}
}

void script_layer::CallEndCallback()
{
	if (state == EBlendLayers::eNone)
	{
		return;
	}

	if (item == 2)
	{
		for (int i = 0; i < item; i++)
		{
			if (g_player_hud->attached_item(i) && g_player_hud->attached_item(i)->m_parent_hud_item)
			{
				g_player_hud->attached_item(i)->m_parent_hud_item->OnBlendEnd(state);
			}
		}
	}
	else
	{
		if (g_player_hud->attached_item(item) && g_player_hud->attached_item(item)->m_parent_hud_item)
		{
			g_player_hud->attached_item(item)->m_parent_hud_item->OnBlendEnd(state);
		}
	}

	state = EBlendLayers::eNone;
}

bool player_hud::check_anim(const shared_str& anim_name, u16 place_idx)
{
	if (place_idx != u16(-1) && (place_idx >= std::size(m_attached_items) || !m_attached_items[place_idx]))
	{
		return false;
	}

	MotionID Motion;
	if (place_idx < std::size(m_attached_items) && m_attached_items[place_idx])
	{
		if (m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name))
		{
			return true;
		}
	}
	else
	{
		Motion = m_model->ID_Cycle_Safe(anim_name);

		if (Motion && Motion.valid())
		{
			return true;
		}
	}

	return false;
}

bool player_hud::animator_play(const shared_str& anim_name, u16 place_idx, u16 part_id, bool bMixIn, float speed, u8 anm_idx, bool impact_on_item, bool similar_check, PlayCallback Callback, LPVOID CallbackParam, bool UpdateCallbackType)
{
	MotionID motion;
	if(place_idx < std::size(m_attached_items) && m_attached_items[place_idx])
	{
		player_hud_motion* anm = m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name);

		if(anm)
		{
			motion_descr M	= anm->m_animations[ anm_idx ];
			motion = M.mid;
			speed *=anm->m_anim_speed;
			if(impact_on_item)
			{
				shared_str item_anm_name;
				if(anm->m_base_name!=anm->m_additional_name)
					item_anm_name = anm->m_additional_name;
				else
					item_anm_name = M.name;

				m_attached_items[place_idx]->anim_play(item_anm_name, bMixIn ? EHudMixType::eMixAll : EHudMixType::eNoMix, speed);
			}
		}
		else
		{
			Msg("! Animation [%s] not found in %s motion container!", *anim_name, m_attached_items[place_idx]->m_sect_name.c_str());
		}
	}
	else
	{
		if (m_model)
		{
			motion = m_model->ID_Cycle_Safe(anim_name);

			if (!motion || !motion.valid())
			{
				Msg("! Animation [%s] not found in %s motion container!", anim_name.c_str(), section_name().c_str());
			}
		}
	}

	if (m_model)
	{
		u16 pc = m_model->partitions().count();
		for (u16 pid = 0; pid < pc; ++pid)
		{
			u32 blends_count = m_model->LL_PartBlendsCount(pid);
			for (u32 blend_id = 0; blend_id < blends_count; ++blend_id)
			{
				CBlend* blend = m_model->LL_PartBlend(pid, blend_id);
				if (!blend)
				{
					continue;
				}
				MotionID M = blend->motionID;
				if (M != motion)
				{
					if (blend->stop_at_end)
					{
						ResetBlockedPartID();
						return false;
					}
				}
			}
		}
	}
	m_blocked_part_idx = part_id;

	CBlend* B = nullptr;
	if (m_model)
	{
		switch (part_id)
		{
			case 0:
			case 1:
			case 2:
			{
				if (similar_check)
				{
					u32 blends_count = m_model->LL_PartBlendsCount(part_id);
					for (u32 blend_id = 0; blend_id < blends_count; ++blend_id)
					{
						CBlend* blend = m_model->LL_PartBlend(part_id, blend_id);
						if (!blend)
						{
							continue;
						}
						MotionID M = blend->motionID;
						if (M == motion)
						{
							ResetBlockedPartID();
							return false;
						}
					}
				}
				B = m_model->PlayCycle(part_id, motion, bMixIn, Callback, CallbackParam);
			}
			break;
			case u16(-1):
			{
				if (similar_check)
				{
					CMotionDef* m_def = m_model->LL_GetMotionDef(motion);
					u32 blends_count = m_model->LL_PartBlendsCount(m_def->bone_or_part);
					for (u32 blend_id = 0; blend_id < blends_count; ++blend_id)
					{
						CBlend* blend = m_model->LL_PartBlend(m_def->bone_or_part, blend_id);
						if (!blend)
						{
							continue;
						}
						MotionID M = blend->motionID;
						if (M == motion)
						{
							ResetBlockedPartID();
							return false;
						}
					}
				}
				B = m_model->PlayCycle(motion, bMixIn, Callback, CallbackParam);
			}
			break;
			default:
			{
				if (similar_check)
				{
					u16 pc = m_model->partitions().count();
					for (u16 pid = 0; pid < pc; ++pid)
					{
						u32 blends_count = m_model->LL_PartBlendsCount(pid);
						for (u32 blend_id = 0; blend_id < blends_count; ++blend_id)
						{
							CBlend* blend = m_model->LL_PartBlend(pid, blend_id);
							if (!blend)
							{
								continue;
							}
							MotionID M = blend->motionID;
							if (M == motion)
							{
								ResetBlockedPartID();
								return false;
							}
						}
					}
				}
				B = m_model->PlayCycle(0, motion, bMixIn, Callback, CallbackParam);
				B = m_model->PlayCycle(1, motion, bMixIn, Callback, CallbackParam);
				B = m_model->PlayCycle(2, motion, bMixIn, Callback, CallbackParam);
			}
			break;
		}
	}
	if(B)
	{
		B->update_callback = UpdateCallbackType;
		B->speed *= speed;
	}
	else
	{
		if(Callback)
		{
			CBlend B;
			B.CallbackParam = CallbackParam;
			Callback(&B);
		}
		ResetBlockedPartID();
		return false;
	}

	return true;
}

void player_hud::animator_fx_play(const shared_str& anim_name, u16 place_idx, u16 part_id, u8 anm_idx, float blendAccrue, float blendFalloff, float Speed, float Power)
{
	MotionID motion;
	if(place_idx < std::size(m_attached_items) && m_attached_items[place_idx])
	{
		player_hud_motion* anm = m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name);

		if(anm)
		{
			motion_descr M	= anm->m_animations[ anm_idx ];
			motion = M.mid;
			Speed *=anm->m_anim_speed;
		}
		else
		{
			Msg("! Animation [%s] not found in %s motion container!", *anim_name, m_attached_items[place_idx]->m_sect_name.c_str());
		}
	}
	else
	{
		motion = m_model->ID_Cycle_Safe(anim_name);

		if(!motion || !motion.valid())
		{
			Msg("! Animation [%s] not found in %s motion container!", anim_name.c_str(), section_name().c_str());
		}
	}
	CMotionDef* m_def = m_model->LL_GetMotionDef(motion);
	if(m_def)
	{
		switch (part_id)
		{
			case 0:
			case 1:
			case 2:
			{
				auto bones_vec = m_model->partitions().part(part_id).bones;
				for (u32 &it : bones_vec)
					m_model->LL_PlayFX(it, motion, m_def->Accrue()*blendAccrue, m_def->Falloff()*blendFalloff, m_def->Speed()*Speed, m_def->Power()*Power);
			}break;
			case u16(-1):
			{
				auto bones_vec = m_model->partitions().part(m_def->bone_or_part).bones;
				for (u32 &it : bones_vec)
					m_model->LL_PlayFX(it, motion, m_def->Accrue()*blendAccrue, m_def->Falloff()*blendFalloff, m_def->Speed()*Speed, m_def->Power()*Power);
			}break;
			default:
			{
				for (auto &[first,second] : *m_model->dcast_PKinematics()->LL_Bones())
					m_model->LL_PlayFX(second, motion, m_def->Accrue()*blendAccrue, m_def->Falloff()*blendFalloff, m_def->Speed()*Speed, m_def->Power()*Power);
			}break;
		}
	}
}

void player_hud::ReloadRigParams()
{
	if (m_model != nullptr)
	{
		m_model->LL_SetInertialization(CCameraRig::HudInertialization());
		m_model->LL_SetSmoothBlend(CCameraRig::Enabled());
	}

	for (attachable_hud_item* item : m_pool)
	{
		item->LoadRigParams();
	}
}

void player_hud::load_default()
{
	static auto actorHudDefault = READ_IF_EXISTS(pSettings, r_string, 
		"actor", "player_hud_default", "actor_hud");
	load(actorHudDefault);
}

animator_item* player_hud::create_animator_item(CHudAnimatorBase* animator, const shared_str& section)
{
	if (m_animator_item && m_animator_item->m_section != section)
	{
		xr_delete(m_animator_item);
	}

	if (!m_animator_item)
	{
		m_animator_item = new animator_item(animator, this, section);
	}

	return m_animator_item;
}

void player_hud::delete_animator_item()
{
	if (m_animator_item)
	{
		xr_delete(m_animator_item);
	}
}

animator_item::animator_item(CHudAnimatorBase* animator, player_hud* pParent, const shared_str& section) : m_parent(pParent), m_section(section), m_animator(animator)
{
	if (pSettings->line_exist(section, "item_visual"))
	{
		const shared_str& visual_name = pSettings->r_string(section, "item_visual");
		m_item = PKinematics(::Render->model_Create(visual_name.c_str()));

		m_has_fire_point =  pSettings->line_exist(section, "fire_bone");
		if (m_has_fire_point)
		{
			m_fire_bone = m_item->LL_BoneID(pSettings->r_string(section, "fire_bone"));
			m_fire_point_offset = pSettings->r_fvector3(section, "fire_point");
		}
		else
			m_fire_point_offset.set(0, 0, 0);

		m_item_attach[EHudOffsetAxis::eAxisPos] = READ_IF_EXISTS(pSettings, r_fvector3, section.c_str(), "item_position", zero_vel);
		m_item_attach[EHudOffsetAxis::eAxisRot] = READ_IF_EXISTS(pSettings, r_fvector3, section.c_str(), "item_orientation", zero_vel);
	}

	m_hands_positions.Load(section, UI().is_widescreen());

	m_hand_motions.load(pParent->GetModel(), section);
}

animator_item::~animator_item()
{
	m_animator = nullptr;
	IsPlaying = false;
	if (m_item)
	{
		IRenderVisual* v = m_item->dcast_RenderVisual();
		::Render->model_Delete(v);
		m_item = nullptr;
	}
}

void animator_item::update(bool bForce)
{
	if (!m_item)
		return;

	if (!bForce && m_upd_firedeps_frame == Device.dwFrame)
		return;

	Fvector ypr = m_item_attach[EHudOffsetAxis::eAxisRot];
	ypr.mul(PI / 180.f);
	m_attach_offset.setHPB(ypr.x, ypr.y, ypr.z);
	m_attach_offset.translate_over(m_item_attach[EHudOffsetAxis::eAxisPos]);

	m_parent->calc_transform(0, m_attach_offset, m_item_transform);
	m_upd_firedeps_frame = Device.dwFrame;

	IKinematicsAnimated* ka = m_item->dcast_PKinematicsAnimated();
	if (ka)
	{
		ka->UpdateTracks();
		ka->dcast_PKinematics()->CalculateBones_Invalidate();
		ka->dcast_PKinematics()->CalculateBones(true);
	}
}

void animator_item::render()
{
	if (!m_item)
	{
		return;
	}

	::Render->set_Transform(&m_item_transform);
	::Render->add_Visual(m_item->dcast_RenderVisual(), true);
}

void animator_item::anim_play(const shared_str& item_anm_name, bool bMixIn, float speed)
{
	if (m_item->dcast_PKinematicsAnimated())
	{
		IKinematicsAnimated* ka = m_item->dcast_PKinematicsAnimated();

		MotionID M2 = ka->ID_Cycle_Safe(item_anm_name);
		if (!M2.valid())
			M2 = ka->ID_Cycle_Safe("idle");
		else
			if (bDebug)
				Msg("playing item animation [%s]", item_anm_name.c_str());

		u16 root_id = m_item->LL_GetBoneRoot();
		CBoneInstance& root_binst = m_item->LL_GetBoneInstance(root_id);
		root_binst.set_callback_overwrite(true);
		root_binst.mTransform.identity();

		u16 pc = ka->partitions().count();
		for (u16 pid = 0; pid < pc; ++pid)
		{
			CBlend* B = ka->PlayCycle(pid, M2, bMixIn);
			R_ASSERT(B);
			B->speed *= speed;
		}

		m_item->CalculateBones_Invalidate();
	}
}

u32 animator_item::anim_play(const shared_str& anm_name_b, bool bMixIn, const CMotionDef*& md)
{
	player_hud_motion* anm = m_hand_motions.find_motion(anm_name_b);

	u8 rnd_idx = (u8)Random.randI(anm->m_animations.size());
	const motion_descr& M = anm->m_animations[rnd_idx];
	float speed = anm->m_anim_speed;

	u32 ret = m_parent->anim_play(0, M.mid, bMixIn, md, speed, nullptr);
	
	if (m_item)
	{
		if (auto ka = m_item->dcast_PKinematicsAnimated())
		{
			shared_str item_anm_name;
			if (anm->m_base_name != anm->m_additional_name)
				item_anm_name = anm->m_additional_name;
			else
				item_anm_name = M.name;

			anim_play(item_anm_name, bMixIn, speed);

			for (auto& bpart_anim : anm->m_bone_parts) {
				MotionID M3 = ka->ID_Cycle_Safe(bpart_anim);

				if (M3.valid()) {
					CBlend* B = ka->PlayCycle(M3, bMixIn);
					if (B)
					{
						B->speed *= speed;
					}
				}
			}
		}
	}

	IsPlaying = true;

	if (Level().CurrentControlEntity())
	{
		CActor* current_actor = static_cast<CActor*>(Level().CurrentControlEntity());
		VERIFY(current_actor);
		string_path ce_path;
		string_path anm_name;
		xr_strconcat(anm_name, "camera_effects\\weapon\\", M.name.c_str(), ".anm");
		if (FS.exist(ce_path, "$game_anims$", anm_name)) {
			CEffectorCam* ec = current_actor->Cameras().GetCamEffector(eCEWeaponAction);
			if (ec)
				current_actor->Cameras().RemoveCamEffector(eCEWeaponAction);
			CAnimatorCamEffector* e = new CAnimatorCamEffector();
			e->SetType(eCEWeaponAction);
			e->SetHudAffect(false);
			e->SetCyclic(false);
			e->Start(anm_name);
			current_actor->Cameras().AddCamEffector(e);
		}
	}
	return ret;
}

void animator_item::update_hud_additional(Fmatrix& trans)
{
	m_animator->UpdateHudAdditonal(trans);
}

Fvector& animator_item::hands_attach_pos()
{
	return m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
}

Fvector& animator_item::hands_attach_rot()
{
	return m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];
}

void animator_item::setup_firedeps(firedeps& fd)
{
	update							(false);
	if(m_has_fire_point)
	{
		if (m_fire_bone == BI_NONE)
			return;

		Fmatrix& fire_mat								= m_item->LL_GetTransform(m_fire_bone);
		fire_mat.transform_tiny							(fd.vLastFP, m_fire_point_offset);
		m_item_transform.transform_tiny					(fd.vLastFP);

		fd.vLastFD.set									(0.f,0.f,1.f);
		m_item_transform.transform_dir					(fd.vLastFD);
		VERIFY(_valid(fd.vLastFD));
		VERIFY(_valid(fd.vLastFD));

		fd.m_FireParticlesXForm.identity				();
		fd.m_FireParticlesXForm.k.set					(fd.vLastFD);
		Fvector::generate_orthonormal_basis_normalized	(	fd.m_FireParticlesXForm.k,
															fd.m_FireParticlesXForm.j, 
															fd.m_FireParticlesXForm.i);
		VERIFY(_valid(fd.m_FireParticlesXForm));
	}
}

bool animator_item::NeedMovementBlend()
{
	return m_animator->NeedMovementBlend() && IsPlaying;
}

void player_hud::FingerCallback(CBoneInstance* B)
{
	BoneCallbackParams* params = static_cast<BoneCallbackParams*>(B->callback_param());

	Fvector& target = params->m_target;
	Fvector& current = params->m_current;


	current = EulerYawPitchRollInertion(current, target, 1.0f, Device.fTimeDelta);

	Fmatrix rotation;
	rotation.identity();
	rotation.setHPB(current.y, -current.x, current.z);

	B->mTransform.mulB_43(rotation);
}
