#include "StdAfx.h"
#include "player_hud.h"
#include "WatchDevice/WatchDevice.h"
#include "HudItem.h"
#include "Actor.h"
#include "physic_item.h"
#include "ActorEffector.h"
#include "imgui_internal.h"
#include "../xrEngine/IGame_Persistent.h"
#include "InertionData.h"
#include "Inventory.h"
#include "WeaponBinoculars.h"
#include "../xrEngine/GameMtlLib.h"
#include "Cutscenes/CutsceneManager.h"

player_hud* g_player_hud = nullptr;
player_hud* g_player_hud2 = nullptr;

namespace HudGripMarks
{
	constexpr const char* Start = "mm_ik_start";
	constexpr const char* End = "mm_ik_end";
}
Fvector _ancor_pos;
Fvector _wpn_root_pos;
extern float AnimatorHideSpeedFactor;

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
	xr_vector<player_hud_motion>::iterator it = m_anims.begin();
	xr_vector<player_hud_motion>::iterator it_e = m_anims.end();
	for (; it != it_e; ++it)
	{
		const shared_str& s = (true) ? (*it).m_alias_name : (*it).m_base_name;
		if (s == name)
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

	CImGuiManager::Instance().Subscribe
	(
		"CutsceneAdjust",
		CImGuiManager::ERenderPriority::eMedium, [this]
		{
			if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::CutsceneAdjust)]) {
				return;
			}

			extern bool hud_adj_crosshair;
			static bool EnableAdjust = false;

			if (ImGui::Begin("CutsceneAdjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::HudAdjust)]))
			{
				ImGui::Checkbox("Enable", &EnableAdjust);

				if (EnableAdjust != CCutsceneManager::GetInstance().GetAdjust()) {
					CCutsceneManager::GetInstance().SetAdjust(EnableAdjust);
				}

				if (!EnableAdjust) {
					ImGui::End();
					return;
				}
				if (ImGui::Button("Save")) {
					CCutsceneManager::GetInstance().SaveAdjust();
				}
				ImGui::SameLine();
				if (ImGui::Button("Reset")) {
					CCutsceneManager::GetInstance().ResetAdjust();
				}

				const auto& Cutscenes = pSettings->r_section("cutscenes");

				const bool PrevCapture = ImGui::GetIO().WantCaptureKeyboard;
				ImGui::GetIO().WantCaptureKeyboard = true;
				ImGui::BeginListBox("Cutscenes");
				static u64 current_part_idx = 0;
				for (u64 i = 0; i < Cutscenes.Data.size(); ++i) {
					const bool is_selected = (current_part_idx == i);
					if (ImGui::Selectable(Cutscenes.Data[i].first.c_str(), is_selected, ImGuiSelectableFlags_SelectOnNav)) {
						current_part_idx = i;
					}
					if (is_selected) { 
						ImGui::SetItemDefaultFocus(); 
						CCutsceneManager::GetInstance().SetAdjustSection(Cutscenes.Data[i].first);
					}
				}
				ImGui::GetIO().WantCaptureKeyboard = PrevCapture;
				ImGui::EndListBox();

				{
					auto flag = ImGuiTreeNodeFlags_DefaultOpen;
					if (ImGui::TreeNodeEx("Objects"))
					{
						CCutsceneManager::GetInstance().DrawData();
						ImGui::TreePop();
					}
				}

			}
			ImGui::End();
		}
	);
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
	m_collision_xform.set(hud_collision);
	trans.mulB_43(hud_collision);
}

void attachable_hud_item::setup_firedeps(firedeps& fd)
{
	update							(false);
	if(m_measures.m_prop_flags.test(hud_item_measures::e_fire_point))
	{
		if (I_ASSERT_M(m_measures.m_fire_bone != BI_NONE, 
			"Invalid fire bone specified", 
			m_sect_name.c_str(), 
			pSettings->r_string(m_sect_name, "fire_bone")))
		{
			Fmatrix& fire_mat								= m_model->LL_GetTransform(m_measures.m_fire_bone);
			fire_mat.transform_tiny							(fd.vLastFP, m_measures.m_fire_point_offset);
			m_item_transform.transform_tiny					(fd.vLastFP);
			fd.vLastFP.add(Device.vCameraPosition);
	
			fd.vLastFD.set									(m_item_transform.k);
			VERIFY(_valid(fd.vLastFD));
	
			fd.m_FireParticlesXForm.identity				();
			fd.m_FireParticlesXForm.set(m_item_transform);
			VERIFY(_valid(fd.m_FireParticlesXForm));
		}
	}

	if(m_measures.m_prop_flags.test(hud_item_measures::e_fire_point) || m_measures.m_prop_flags.test(hud_item_measures::e_fire_point2))
	{
		Fmatrix& fire_mat			= m_measures.m_fire_bone2 != u16(-1) ? m_model->LL_GetTransform(m_measures.m_fire_bone2) : m_model->LL_GetTransform(m_measures.m_fire_bone);
		fire_mat.transform_tiny		(fd.vLastFP2,m_measures.m_fire_point2_offset);
		m_item_transform.transform_tiny	(fd.vLastFP2);
		fd.vLastFP2.add(Device.vCameraPosition);
		VERIFY(_valid(fd.vLastFP2));
	}

	if(m_measures.m_prop_flags.test(hud_item_measures::e_shell_point))
	{
		Fmatrix& fire_mat = m_measures.m_shell_bone != u16(-1) ? m_model->LL_GetTransform(m_measures.m_shell_bone) : m_model->LL_GetTransform(m_measures.m_fire_bone);
		fire_mat.transform_tiny		(fd.vLastSP,m_measures.m_shell_point_offset);
		m_item_transform.transform_tiny	(fd.vLastSP);
		fd.vLastSP.add(Device.vCameraPosition);
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
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault] : zero_vel);
	xr_strconcat(val_name, "hands_orientation", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault] : zero_vel);
	
	LoadAimParams(section, is_16x9, default_is_self);

	xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimAlt] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimAlt] : zero_vel);
	xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimAlt] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimAlt] : zero_vel);

	xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eSafemode] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eSafemode] : zero_vel);
	xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eSafemode] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eSafemode] : zero_vel);

	xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eCollision] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eCollision] : zero_vel);
	xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
	hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eCollision] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eCollision] : zero_vel);

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
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] = pSettings->read_if_exists<Fvector3>(sSection, "zoom_offset", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim] : zero_vel);
	}

	xr_strconcat(val_name, "aim_hud_offset_rot", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].x = pSettings->read_if_exists<float>(sSection, "zoom_rotate_x", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].x : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].y = pSettings->read_if_exists<float>(sSection, "zoom_rotate_y", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].y : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].z = pSettings->read_if_exists<float>(sSection, "zoom_rotate_z", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].z : 0.0f);
	}

	xr_strconcat(val_name, "gl_hud_offset_pos", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] = pSettings->read_if_exists<Fvector3>(sSection, "grenade_zoom_offset", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL] : zero_vel);
	}

	xr_strconcat(val_name, "gl_hud_offset_rot", widescreenPrefix);
	if (pSettings->line_exist(section, val_name))
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL] = pSettings->read_if_exists<Fvector3>(sSection, val_name, default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL] : zero_vel);
	}
	else
	{
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].x = pSettings->read_if_exists<float>(sSection, "grenade_zoom_rotate_x", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].x : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].y = pSettings->read_if_exists<float>(sSection, "grenade_zoom_rotate_y", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].y : 0.0f);
		hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].z = pSettings->read_if_exists<float>(sSection, "grenade_zoom_rotate_z", default_is_self ? hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].z : 0.0f);
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
		m_item_attach[EHudOffsetAxis::eAxisPos] = pSettings->read_if_exists<Fvector3>(sect_name, "position", zero_vel);
		m_item_attach[EHudOffsetAxis::eAxisRot] = pSettings->read_if_exists<Fvector3>(sect_name, "orientation", zero_vel);
	}
	else
	{
		m_item_attach[EHudOffsetAxis::eAxisPos] = pSettings->read_if_exists<Fvector3>(sect_name, "item_position", zero_vel);
		m_item_attach[EHudOffsetAxis::eAxisRot] = pSettings->read_if_exists<Fvector3>(sect_name, "item_orientation", zero_vel);
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
	m_fire_point2_offset = pSettings->read_if_exists<Fvector3>(sect_name, "fire_point2", zero_vel);

	m_prop_flags.set(e_shell_point, pSettings->line_exist(sect_name, "shell_point"));
	if (m_prop_flags.test(e_shell_point))
	{
		bone_name = pSettings->read_if_exists<str_c>(sect_name, "shell_bone", "");
		m_shell_bone = bone_name.size() ? K->LL_BoneID(bone_name) : u16(-1);
		m_shell_point_offset = pSettings->r_fvector3(sect_name, "shell_point");
	}
	else
	{
		m_shell_point_offset.set(0, 0, 0);
	}

	m_inertion_params.m_tendto_speed = pSettings->read_if_exists<float>(sect_name, "inertion_tendto_speed", 1.0f);
	m_inertion_params.m_tendto_speed_aim = pSettings->read_if_exists<float>(sect_name, "inertion_tendto_aim_speed", 1.0f);
	m_inertion_params.m_tendto_ret_speed = pSettings->read_if_exists<float>(sect_name, "inertion_tendto_ret_speed", 5.0f);
	m_inertion_params.m_tendto_ret_speed_aim = pSettings->read_if_exists<float>(sect_name, "inertion_tendto_ret_aim_speed", 5.0f);

	bool is_16x9 = UI().is_widescreen() && !combined_model;

	m_hands_positions.Load(sect_name, is_16x9);
	m_hands_attach_real[EHudOffsetAxis::eAxisPos] = m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
	m_hands_attach_real[EHudOffsetAxis::eAxisRot] = m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];

	m_weapon_inertion.Load(sect_name, is_16x9);

	m_collision_params.obb_pos = pSettings->read_if_exists<Fvector3>(sect_name, "collision_box_pos", zero_vel);
	m_collision_params.obb_scale = pSettings->read_if_exists<Fvector3>(sect_name, "collision_box_scale", Fvector({1.0f, 1.0f, 1.0f}));

	m_collision_params.damping = pSettings->read_if_exists<float>(sect_name, "collision_damping", 40.0f);
	m_collision_params.stifness = pSettings->read_if_exists<float>(sect_name, "collision_stifness", 400.0f);

	m_collision_params.dist_min = pSettings->read_if_exists<float>(sect_name, "collision_dist_min", 0.5f);
	m_collision_params.dist_max = pSettings->read_if_exists<float>(sect_name, "collision_dist_max", 0.8f);

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

	move_rlookout_offset_speed_factor = pSettings->read_if_exists<float>(section, "hud_move_rlookout_offset_speed_factor", 1.0f);
	move_llookout_offset_speed_factor = pSettings->read_if_exists<float>(section, "hud_move_llookout_offset_speed_factor", 1.0f);

	aim_move_slow_crouch_factor = pSettings->read_if_exists<float>(section, "hud_aim_move_slow_crouch_factor", 1.0f);
	aim_move_crouch_factor = pSettings->read_if_exists<float>(section, "hud_aim_move_crouch_factor", 1.0f);
	aim_move_slow_factor = pSettings->read_if_exists<float>(section, "hud_aim_move_slow_factor", 1.0f);

	move_slow_crouch_factor = pSettings->read_if_exists<float>(section, "hud_move_slow_crouch_factor", 1.0f);
	move_crouch_factor = pSettings->read_if_exists<float>(section, "hud_move_crouch_factor", 1.0f);
	move_slow_factor = pSettings->read_if_exists<float>(section, "hud_move_slow_factor", 1.0f);

	no_other_hud_moving_while_suicide = pSettings->read_if_exists<bool>(section, "no_other_hud_moving_while_suicide", false);

	to_crouch_time = floor(pSettings->read_if_exists<float>(section, "to_crouch_time", 0.0f) * 1000.f);
	from_crouch_time = floor(pSettings->read_if_exists<float>(section, "from_crouch_time", 0.0f) * 1000.f);
	to_slow_crouch_time = floor(pSettings->read_if_exists<float>(section, "to_slow_crouch_time", 0.0f) * 1000.f);
	from_slow_crouch_time = floor(pSettings->read_if_exists<float>(section, "from_slow_crouch_time", 0.0f) * 1000.f);

	to_rlookout_time = floor(pSettings->read_if_exists<float>(section, "to_rlookout_time", 0.0f) * 1000.f);
	from_rlookout_time = floor(pSettings->read_if_exists<float>(section, "from_rlookout_time", 0.0f) * 1000.f);
	to_llookout_time = floor(pSettings->read_if_exists<float>(section, "to_llookout_time", 0.0f) * 1000.f);
	from_llookout_time = floor(pSettings->read_if_exists<float>(section, "from_llookout_time", 0.0f) * 1000.f);

	move_weaponhide_factor = pSettings->read_if_exists<float>(section, "hud_move_weaponhide_factor", 1.0f);
	move_unzoom_factor = pSettings->read_if_exists<float>(section, "hud_move_unzoom_factor", 1.0f);
	move_stabilize_factor = pSettings->read_if_exists<float>(section, "hud_move_stabilize_factor", 2.0f);

	move_speed_pos = pSettings->read_if_exists<float>(section, "hud_move_speed_pos", 0.1f);
	move_speed_rot = pSettings->read_if_exists<float>(section, "hud_move_speed_rot", 0.4f);

	move_suicide_speed_pos = pSettings->read_if_exists<float>(section, "suicide_speed_pos", 0.2f);
	move_suicide_speed_rot = pSettings->read_if_exists<float>(section, "suicide_speed_rot", 0.002f);
}

void weapon_inertion::base_params::Load(const shared_str& section, const shared_str& str, bool is_16x9)
{
	shared_str name;

	name.printf("%s_pos%s", *str, is_16x9 ? "_16x9" : "");
	position = pSettings->read_if_exists<Fvector3>(section, *name, zero_vel);

	name.printf("%s_rot%s", *str, is_16x9 ? "_16x9" : "");
	rotation = pSettings->read_if_exists<Fvector3>(section, *name, zero_vel);
}

attachable_hud_item::~attachable_hud_item()
{
	IRenderVisual* v			= m_model->dcast_RenderVisual();
	::Render->model_Delete		(v);
	m_model						= nullptr;
}

void attachable_hud_item::load(const shared_str& sect_name)
{
	m_sect_name					= sect_name;

	m_model_combined			= !pSettings->line_exist(sect_name, "item_visual");
	m_visual_name = m_model_combined ? pSettings->r_string(sect_name, "visual") : pSettings->r_string(sect_name, "item_visual");
	m_model						 = PKinematics(::Render->model_Create(m_visual_name.c_str()));

	if (IKinematicsAnimated* animated = m_model->dcast_PKinematicsAnimated())
	{
		animated->LL_SetInertialization(CCameraRig::HudInertialization());

		const char* exclude = pSettings->read_if_exists<str_c>(sect_name, "inertialization_exclude_bones", pSettings->read_if_exists<str_c>("camera_rig", "hud_inertialization_exclude_bones", nullptr));
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

	m_attach_place_idx = pSettings->read_if_exists<u16>(sect_name, "attach_place_idx", 0);

	if (pSettings->line_exist(sect_name, "aim_bone_name"))
		m_aim_bone_id = m_model->LL_BoneID(pSettings->r_string(sect_name, "aim_bone_name"));

	m_aim_reload_motion = pSettings->read_if_exists<float>(sect_name, "aim_reload_motion", pSettings->read_if_exists<float>("camera_rig", "aim_reload_motion", 1.f));
	m_hud_mass = std::max(pSettings->read_if_exists<float>(sect_name, "hud_mass", 1.f), 0.1f);
	m_shot_variation = pSettings->read_if_exists<float>(sect_name, "shot_anim_variation", pSettings->read_if_exists<float>("camera_rig", "shot_anim_variation", 0.f));
	m_shot_twist = pSettings->read_if_exists<float>(sect_name, "shot_anim_twist", pSettings->read_if_exists<float>("camera_rig", "shot_anim_twist", 0.f));
	m_aim_shot_motion = pSettings->read_if_exists<float>(sect_name, "aim_shot_motion", pSettings->read_if_exists<float>("camera_rig", "aim_shot_motion", 1.f));
	m_idle_fade = pSettings->read_if_exists<float>(sect_name, "idle_fade", pSettings->read_if_exists<float>("camera_rig", "idle_fade", 0.f));
	m_inertia_damping = pSettings->read_if_exists<float>(sect_name, "inertia_damping", pSettings->read_if_exists<float>("camera_rig", "inertia_damping", 0.55f));

	const char* left_ik_bone = pSettings->read_if_exists<str_c>(sect_name, "left_hand_ik_bone", pSettings->read_if_exists<str_c>("camera_rig", "left_hand_ik_bone", nullptr));
	if (!m_model_combined && left_ik_bone != nullptr)
	{
		m_left_ik_bone = m_model->LL_BoneID(left_ik_bone);
		m_left_ik_fixed = pSettings->line_exist(sect_name, "left_hand_ik_pos") || pSettings->line_exist(sect_name, "left_hand_ik_rot");
		Fvector ypr = pSettings->read_if_exists<Fvector3>(sect_name, "left_hand_ik_rot", zero_vel);
		ypr.mul(PI / 180.f);
		m_left_ik_offset.setHPB(ypr.x, ypr.y, ypr.z);
		m_left_ik_offset.c = pSettings->read_if_exists<Fvector3>(sect_name, "left_hand_ik_pos", zero_vel);

		m_left_grip_blend = std::max(pSettings->read_if_exists<float>(sect_name, "left_hand_grip_blend", pSettings->read_if_exists<float>("camera_rig", "left_hand_grip_blend", 0.f)), 0.f);
		m_left_grip_target = pSettings->read_if_exists<str_c>(sect_name, "left_hand_grip_target", pSettings->read_if_exists<str_c>("camera_rig", "left_hand_grip_target", "anm_idle"));

		const auto read_motions = [&](const char* key, xr_vector<shared_str>& list)
		{
			list.clear();
			if (const char* motions = pSettings->read_if_exists<str_c>(sect_name, key, nullptr))
			{
				string128 motion;
				for (int i = 0, count = _GetItemCount(motions); i < count; ++i)
				{
					list.emplace_back(_GetItem(motions, i, motion));
				}
			}
		};

		read_motions("left_hand_grip_motions", m_left_grip_motions);
		read_motions("left_hand_grip_hand_motions", m_left_grip_hand_motions);
	}

	m_measures.load				(sect_name, m_model, m_model_combined);
}

static CBlend* hud_play_cycle(IKinematicsAnimated* ka, u16 part, MotionID motion, bool mix, float speed, float fade)
{
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

			CBlend* B = hud_play_cycle(ka, pid, M2, bMixIn > EHudMixType::eMixHands, speed, m_parent->m_cycle_fade);
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

u32 attachable_hud_item::anim_play(const shared_str& anm_name_b, EHudMixType bMixIn, const CMotionDef*& md, u8& rnd_idx, bool disable_random)
{
	R_ASSERT				(strstr(anm_name_b.c_str(),"anm_")==anm_name_b.c_str());

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
	g_player_hud->m_cycle_fade = idle && CCameraRig::Enabled() ? m_idle_fade : 0.f;

	IKinematicsAnimated* kaModel = m_model_combined ? m_model->dcast_PKinematicsAnimated() : nullptr;
	u32 ret = g_player_hud->anim_play(m_attach_place_idx, M.mid, need_mix_hands, md, speed, kaModel);	
	if(m_model->dcast_PKinematicsAnimated() != nullptr)
	{
		shared_str item_anm_name;
		if(anm->m_base_name!=anm->m_additional_name)
			item_anm_name = anm->m_additional_name;
		else
			item_anm_name = M.name;

		anim_play(item_anm_name, bMixIn, speed, anm);
	}

	g_player_hud->m_cycle_fade = 0.f;

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
	if (pSettings->read_if_exists<bool>(section, "prohibit_suicide", false))
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
		const float omega = std::min(speed / step * CCameraRig::MassSpeed(m_hud_mass), 1.5f / step);
		Fvector accel;
		accel.mul(offset, omega * omega).mad(velocity, -2.f * m_inertia_damping * omega);
		velocity.mad(accel, step);
		offset.mul(velocity, step);
	};

	if (!spring)
	{
		m_inertia_vel_pos.set(0.f, 0.f, 0.f);
		m_inertia_vel_rot.set(0.f, 0.f, 0.f);
	}

	while (time_accumulator > 8)
	{
		pos = targetpos;
		rot = targetrot;

		Fvector cur_pos = hands_attach_pos();
		Fvector cur_rot = hands_attach_rot();

		pos.sub(cur_pos);
		rot.sub(cur_rot);

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
			spring_step(pos, m_inertia_vel_pos, speed_pos);
			spring_step(rot, m_inertia_vel_rot, speed_rot);
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
		cur_rot.add(rot);

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
			cur_rot.add(rot);
		}

		cur_pos.sub(m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos]);
		m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos].add(cur_pos);
		cur_rot.sub(m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisRot]);
		m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisRot].add(cur_rot);

		time_accumulator -= 8;
	}

	if (actor->SuicideNow && ActorVisForContr && !(itm->HudDesc().ProhibitSuicide || itm->cast_weapon() && itm->cast_weapon()->SuicideByAnimation))
	{
		pos = hands_attach_pos();
		rot = hands_attach_rot();
	
		pos.sub(targetpos);
		rot.sub(targetrot);
	
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
	PendingDefaultSect = nullptr;
	if (player_hud_sect == m_sect_name)
	{
		return;
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

	const shared_str& model_name = pSettings->read_if_exists<str_c>(player_hud_sect, "visual", nullptr);
	
	if (model_name.size())
	{
		auto CreatedModel = ::Render->model_Create(model_name.c_str());
		m_model = dynamic_cast<IKinematicsAnimated*>(CreatedModel);
	}

	if (m_model)
	{
		m_model->LL_SetInertialization(CCameraRig::HudInertialization());
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
		pSettings->r_fvector3_nullable(player_hud_sect, "watches_pos", m_watches_pos);
		pSettings->r_fvector3_nullable(player_hud_sect, "watches_rot", m_watches_rot);
		pSettings->r_float_nullable(player_hud_sect, "watches_scale", m_watches_scale);

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

		const auto load_chain = [&](const char* key, const char* def, u16* chain)
		{
			string128 bone;
			const char* names = pSettings->read_if_exists<str_c>(player_hud_sect, key, pSettings->read_if_exists<str_c>("camera_rig", key, def));
			for (int i = 0; i < 3; i++)
			{
				chain[i] = _GetItemCount(names) == 3 ? m_model->dcast_PKinematics()->LL_BoneID(_GetItem(names, i, bone)) : BI_NONE;
			}
		};

		load_chain("left_hand_ik_chain", "j_shoulder_le, j_elbow_le, j_wrist_le", m_left_ik_chain);
		load_chain("right_hand_ik_chain", "j_shoulder_ri, j_elbow_ri, j_wrist_ri", m_right_ik_chain);

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
		float m_fLegs_shift = pSettings->read_if_exists<float>("actor_hud", "legs_shift_delta", -0.55f);
		Actor()->m_fLegs_shift = pSettings->read_if_exists<float>(player_hud_sect, "legs_shift_delta", m_fLegs_shift);
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
	EnsureDefaultLoaded();
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
	B->mTransform.mulA_43(m_transformL);
	B->mTransform.mulA_43(Fmatrix().invert(m_transform));
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
	EnsureDefaultLoaded();
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
		m_turn_offset.lerp(m_turn_offset, local, 1.f - expf(-Device.fTimeDelta / turn_smooth));
		turn_offset.set(0.f, 0.f, 0.f).mad(cam_trans.i, m_turn_offset.x).mad(cam_trans.j, m_turn_offset.y).mad(cam_trans.k, m_turn_offset.z);
		trans.c.add(cam_trans.c, turn_offset);
	}

	CActor* actor = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_actor() : nullptr;
	const Fmatrix trans_turn = trans;
	if (actor != nullptr)
	{
		actor->Cameras().Rig().ApplyHud(trans);

		CWeapon* weapon = m_attached_items[0] != nullptr && m_attached_items[0]->m_parent_hud_item != nullptr ? m_attached_items[0]->m_parent_hud_item->cast_weapon() : nullptr;
		const u32 key = (actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal) & (ACTOR_DEFS::EMoveCommand::mcAnyMove | ACTOR_DEFS::EMoveCommand::mcSprint | ACTOR_DEFS::EMoveCommand::mcAccel | ACTOR_DEFS::EMoveCommand::mcCrouch)) | (weapon != nullptr && weapon->IsZoomed() ? 0x80000000 : 0);
		if (key != m_movement_key)
		{
			m_movement_key = key;
			UpdateMovementLayers();
		}
	}

	const float ik_share = CCameraRig::HudIkShare();
	m_layers_ik = ik_share > 0.f && actor != nullptr && m_attached_items[0] != nullptr && !m_attached_items[0]->m_model_combined && m_attached_items[1] == nullptr && m_animator_item == nullptr && m_right_ik_chain[2] != BI_NONE;

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
		m_transform.mul_43(trans, m_attach_offsetr);
	}

	{
		bool left_hand_active = m_attached_items[1];

		Fmatrix attach_offset;
		attach_offset.setHPB(VPUSH(Fvector(left_hand_active ? m_attached_items[1]->hands_attach_rot() : attach_rot()).mul(PI / 180.f)));
		attach_offset.c.set(left_hand_active ? m_attached_items[1]->hands_attach_pos() : attach_pos());
		m_transformL.mul_43(trans_2, m_attach_offsetl.set(attach_offset));
	}

	Fmatrix script_layers = Fidentity;
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

		if (anm->part == 0 || anm->part == 2)
		{
			m_transform.mulB_43(anm->XFORM());
			script_layers.mulB_43(anm->XFORM());
		}
		
		if (anm->part == 1 || anm->part == 2)
		{
			m_transformL.mulB_43(anm->XFORM());
		}

		if (need_stop)
		{
			anm->Stop(!anm->mix);
		}
	}

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

	if (m_layers_ik)
	{
		const hud_item_measures& measures = m_attached_items[0]->m_measures;
		const Fvector& base_pos = measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisPos][EHudOffsetType::eDefault];
		const Fvector& base_rot = measures.m_hands_positions.hands_offsets_tune[EHudOffsetAxis::eAxisRot][EHudOffsetType::eDefault];

		const Fvector rigid_rot = Fvector().lerp(attach_rot(), base_rot, ik_share).mul(PI / 180.f);
		Fmatrix rigid_attach;
		rigid_attach.setHPB(rigid_rot.x, rigid_rot.y, rigid_rot.z);
		rigid_attach.c.lerp(attach_pos(), base_pos, ik_share);

		const float rigid_share = 1.f - ik_share;
		Fmatrix rigid = trans_turn;
		rigid.c.mad(turn_offset, -ik_share);
		rigid.mulB_43(scale_layer_offset(Fmatrix().mul_43(Fmatrix().invert(trans_turn), trans_rig), rigid_share));

		attachable_hud_item* item = m_attached_items[0];
		const Fmatrix collision_inv = Fmatrix().invert(item->m_collision_xform);
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

		rigid.mulB_43(scale_layer_offset(item->m_collision_xform, rigid_share));
		rigid.mulB_43(rigid_attach);
		rigid.mulB_43(scale_layer_offset(script_layers, 1.f - ik_share));
		rigid.mulB_43(scale_layer_offset(movement_layers, 1.f - ik_share));

		m_layers_ik_offset.invert(rigid).mulB_43(m_transform);
		m_transform.set(rigid);

		m_transformL.set(rigid);
	}

	if ((m_model &&(m_attached_items[0] && !m_attached_items[0]->m_model_combined || m_attached_items[1])) || (m_animator_item && m_animator_item->IsPlaying))
	{
		m_model->UpdateTracks();
		m_model->dcast_PKinematics()->CalculateBones_Invalidate();
		m_model->dcast_PKinematics()->CalculateBones(true);
	}

	if(m_attached_items[0])
		m_attached_items[0]->update(true);

	if(m_attached_items[1])
		m_attached_items[1]->update(true);

	if (m_animator_item && m_animator_item->IsPlaying)
		m_animator_item->update(true);

	update_hands_ik(cam_trans, actor);

	{
		if (m_watches_bone != BI_NONE)
		{
			Fmatrix AttachBone = m_model->dcast_PKinematics()->LL_GetTransform(m_watches_bone);
			m_watches_transform.mul_43(m_transform, AttachBone);
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
	if (m_model == nullptr || m_left_ik_chain[0] == BI_NONE || m_left_ik_chain[1] == BI_NONE)
	{
		return bones;
	}

	IKinematics* kinematics = m_model->dcast_PKinematics();
	const u16 clavicle = kinematics->LL_GetData(m_left_ik_chain[0]).GetParentID();
	if (clavicle != BI_NONE && clavicle != kinematics->LL_GetBoneRoot())
	{
		bones.push_back(clavicle);
	}

	bones.push_back(m_left_ik_chain[0]);
	bones.push_back(m_left_ik_chain[1]);
	return bones;
}

u8 attachable_hud_item::left_grip_motion_mode(const shared_str& motion) const
{
	const auto matches = [&](const xr_vector<shared_str>& list)
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
	};

	return matches(m_left_grip_hand_motions) ? 2 : matches(m_left_grip_motions) ? 1 : 0;
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
			total += B->blendAmount;
			if (B->motionID == motion)
			{
				amount += B->blendAmount;
			}
		}
	}

	return total > EPS ? amount / total : 0.f;
}

static float hud_motion_time(IKinematicsAnimated* ka, MotionID motion)
{
	float time = -1.f;
	float amount = -1.f;
	for (u32 i = 0, count = ka->LL_PartBlendsCount(0); i < count; i++)
	{
		const CBlend* B = ka->LL_PartBlend(0, i);
		if (B != nullptr && B->channel == 0 && B->motionID == motion && B->blendAmount > amount)
		{
			amount = B->blendAmount;
			time = B->timeCurrent;
		}
	}

	return time;
}

bool player_hud::left_grip_reference(attachable_hud_item* item)
{
	IKinematicsAnimated* weapon = item->m_model->dcast_PKinematicsAnimated();
	player_hud_motion* anm = item->m_hand_motions.find_motion(item->m_left_grip_target);
	if (m_model == nullptr || weapon == nullptr || anm == nullptr || anm->m_animations.empty() || m_left_ik_chain[2] == BI_NONE)
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

	Fmatrix ancor, wrist, grip_bone;
	if (!m_model->LL_GetMotionBoneTransform(M.mid, hands_time, m_ancors[item->m_attach_place_idx], ancor)
		|| !m_model->LL_GetMotionBoneTransform(M.mid, hands_time, m_left_ik_chain[2], wrist)
		|| !weapon->LL_GetMotionBoneTransform(item_motion, item_time, item->m_left_ik_bone, grip_bone))
	{
		return false;
	}

	Fmatrix grip;
	grip.mul_43(ancor, item->m_attach_offset).mulB_43(grip_bone);
	item->m_left_grip_ref.mul_43(Fmatrix().invert(grip), wrist);
	item->m_left_grip_ancor_ref.set(ancor);

	item->m_left_grip_pose_ref.clear();
	for (u16 bone : left_arm_subtree())
	{
		Fmatrix bone_ms, parent_ms;
		if (!m_model->LL_GetMotionBoneTransform(M.mid, hands_time, bone, bone_ms) || !m_model->LL_GetMotionBoneTransform(M.mid, hands_time, m_model->dcast_PKinematics()->LL_GetData(bone).GetParentID(), parent_ms))
		{
			item->m_left_grip_pose_ref.clear();
			break;
		}

		item->m_left_grip_pose_ref.push_back(Fquaternion().set(Fmatrix().mul_43(Fmatrix().invert(parent_ms), bone_ms)));
	}

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

void player_hud::update_hands_ik(const Fmatrix& cam_trans, CActor* actor)
{
	if (m_left_grip_snap_frame != u32(-1) && Device.dwFrame > m_left_grip_snap_frame && m_model != nullptr)
	{
		for (u16 bone : left_arm_subtree())
		{
			m_model->LL_SetInertializationBone(bone, true);
		}

		m_left_grip_snap_frame = u32(-1);
	}

	attachable_hud_item* item = m_attached_items[0];
	IKinematics* model = m_model != nullptr ? m_model->dcast_PKinematics() : nullptr;
	if (item == nullptr || item->m_model_combined || model == nullptr || actor == nullptr || !CCameraRig::Enabled())
	{
		m_left_ik_weight = 0.f;
		return;
	}

	const u32 state = item->m_parent_hud_item->GetState();
	bool active = m_attached_items[1] == nullptr && item->m_left_ik_bone != BI_NONE;
	if (active)
	{
		active = state == CHUDState::eIdle || item->m_parent_hud_item->WpnCanShoot() && (state == CWeapon::eFire || state == CWeapon::eFire2);
	}

	m_left_ik_weight = std::clamp(m_left_ik_weight + (active ? Device.fTimeDelta : -Device.fTimeDelta) * 4.f, 0.f, 1.f);

	const bool has_right = m_right_ik_chain[0] != BI_NONE && m_right_ik_chain[1] != BI_NONE && m_right_ik_chain[2] != BI_NONE;
	const bool has_left = m_attached_items[1] == nullptr && m_left_ik_chain[0] != BI_NONE && m_left_ik_chain[1] != BI_NONE && m_left_ik_chain[2] != BI_NONE;
	const bool has_grip = has_left && item->m_left_ik_bone != BI_NONE;
	const u16 ancor = m_ancors[item->m_attach_place_idx];

	CWeapon* weapon = item->m_parent_hud_item->cast_weapon();
	const bool reload_state = weapon != nullptr && (state == CWeapon::eReload || state == CWeapon::eMisfire || state == CWeapon::eLightMis || state == CWeapon::eMagCheck
		|| state == CWeapon::eLoadChamber || state == CWeapon::eUnloadChamber || state == CWeapon::eChamberCheck);
	if (state == CHUDState::eIdle && m_aim_reload_weight <= 0.f && m_shot_weight <= 0.f)
	{
		m_aim_reload_ref.set(model->LL_GetTransform(ancor));
	}

	const bool aim_reload = reload_state && weapon->IsZoomed() && item->m_aim_reload_motion < 1.f;
	const float aim_reload_blend = CCameraRig::AimReloadBlend();
	m_aim_reload_weight = std::clamp(aim_reload_blend > 0.f ? m_aim_reload_weight + (aim_reload ? Device.fTimeDelta : -Device.fTimeDelta) / aim_reload_blend : (aim_reload ? 1.f : 0.f), 0.f, 1.f);

	const bool firing = weapon != nullptr && (state == CWeapon::eFire || state == CWeapon::eFire2) && (item->m_shot_variation > 0.f || item->m_shot_twist > 0.f || item->m_aim_shot_motion < 1.f);
	if (state == CHUDState::eIdle && m_shot_weight <= 0.f)
	{
		m_shot_ref.set(model->LL_GetTransform(ancor));
	}

	const float shot_blend = CCameraRig::ShotAnimBlend();
	m_shot_weight = std::clamp(shot_blend > 0.f ? m_shot_weight + (firing ? Device.fTimeDelta : -Device.fTimeDelta) / shot_blend : (firing ? 1.f : 0.f), 0.f, 1.f);

	const float lean = actor->Cameras().Rig().LeanCounter();
	const bool move_weapon = has_right && (m_layers_ik || !fis_zero(lean) || m_aim_reload_weight > 0.f || m_shot_weight > 0.f);
	if (!move_weapon && !(has_grip && (m_left_ik_weight > 0.f || item->m_left_grip_blend > 0.f)))
	{
		return;
	}

	Fmatrix delta = move_weapon && m_layers_ik ? m_layers_ik_offset : Fidentity;
	if (move_weapon && m_aim_reload_weight > 0.f)
	{
		const Fmatrix& anim = model->LL_GetTransform(ancor);

		if (reload_state)
		{
			const float progress = hud_cycle_progress(m_model);
			const float tail = CCameraRig::AimReloadTail();
			m_aim_reload_release = progress < 0.f || tail <= 0.f ? 1.f : std::clamp((1.f - progress) / tail, 0.f, 1.f);
		}

		const float release = m_aim_reload_release;
		const float blend = m_aim_reload_weight * (release * release * (3.f - 2.f * release));
		const float weight = blend * blend * (3.f - 2.f * blend);
		const Fmatrix motion = Fmatrix().mul_43(Fmatrix().invert(m_aim_reload_ref), anim);
		const Fmatrix damped = Fmatrix().mul_43(m_aim_reload_ref, scale_layer_offset(motion, lerp(1.f, item->m_aim_reload_motion, weight)));
		delta.mulB_43(Fmatrix().mul_43(damped, Fmatrix().invert(anim)));
	}

	if (move_weapon && m_shot_weight > 0.f)
	{
		const Fmatrix& anim = model->LL_GetTransform(ancor);

		if (firing)
		{
			const float progress = hud_cycle_progress(m_model);
			const float tail = CCameraRig::ShotAnimTail();
			m_shot_release = progress < 0.f || tail <= 0.f ? 1.f : std::clamp((1.f - progress) / tail, 0.f, 1.f);
		}

		const float blend = m_shot_weight * (m_shot_release * m_shot_release * (3.f - 2.f * m_shot_release));
		const float weight = blend * blend * (3.f - 2.f * blend);
		const Fmatrix motion = Fmatrix().mul_43(Fmatrix().invert(m_shot_ref), anim);

		const float aim = lerp(1.f, item->m_aim_shot_motion, weapon->GetAimFactor());

		Fvector rot;
		motion.getXYZ(rot);
		const float climb = rot.x;
		rot.x *= lerp(1.f, m_shot_rot_scale.x * aim, weight);
		rot.y = rot.y * lerp(1.f, m_shot_rot_scale.y * aim, weight) + climb * m_shot_twist.x * aim * weight;
		rot.z = rot.z * lerp(1.f, m_shot_rot_scale.z * aim, weight) + climb * m_shot_twist.y * aim * weight;

		Fvector pos = motion.c;
		pos.x *= lerp(1.f, m_shot_pos_scale.x * aim, weight);
		pos.y *= lerp(1.f, m_shot_pos_scale.y * aim, weight);
		pos.z *= lerp(1.f, m_shot_pos_scale.z * aim, weight);

		Fmatrix varied;
		varied.setXYZ(rot);
		varied.translate_over(pos);
		delta.mulB_43(Fmatrix().mul_43(Fmatrix().mul_43(m_shot_ref, varied), Fmatrix().invert(anim)));
	}

	if (move_weapon && !fis_zero(lean))
	{
		Fvector axis;
		Fmatrix().invert(m_transform).transform_dir(axis, cam_trans.k);
		axis.normalize_safe();

		const Fvector pivot = model->LL_GetTransform(ancor).c;
		Fmatrix lean_delta;
		lean_delta.rotation(axis, lean);
		Fvector rotated;
		lean_delta.transform_dir(rotated, pivot);
		lean_delta.c.sub(pivot, rotated);
		delta.mulA_43(lean_delta);
	}

	Fmatrix right_target = has_right ? Fmatrix().mul_43(delta, model->LL_GetTransform(m_right_ik_chain[2])) : Fidentity;
	Fmatrix left_target = has_left ? Fmatrix().mul_43(delta, model->LL_GetTransform(m_left_ik_chain[2])) : Fidentity;
	Fmatrix grip_target = left_target;
	float grip_weight = m_left_ik_weight * m_left_ik_weight * (3.f - 2.f * m_left_ik_weight);
	if (has_grip)
	{
		Fmatrix grip;
		grip.mul_43(Fmatrix().invert(m_transform), Fmatrix().mul_43(item->m_item_transform, item->m_model->LL_GetTransform(item->m_left_ik_bone)));
		grip.mulA_43(delta);
		const Fmatrix animated = Fmatrix().mul_43(Fmatrix().invert(grip), left_target);

		const auto follow_grip = [&](bool snap)
		{
			const float follow = CCameraRig::LeftHandIkFollow();
			if (snap || follow <= 0.f)
			{
				item->m_left_ik_offset.set(animated);
				return;
			}

			const float factor = 1.f - expf(-Device.fTimeDelta / follow);
			Fquaternion q_from, q_to, q_blend;
			q_from.set(item->m_left_ik_offset);
			q_to.set(animated);
			q_blend.slerp(q_from, q_to, factor);

			Fvector position;
			position.lerp(item->m_left_ik_offset.c, animated.c, factor);
			item->m_left_ik_offset.rotation(q_blend);
			item->m_left_ik_offset.c = position;
		};

		if (item->m_left_grip_blend > 0.f)
		{
			if (!item->m_left_grip_ref_tried)
			{
				item->m_left_grip_ref_tried = true;
				item->m_left_grip_ref_valid = left_grip_reference(item);
			}
			else if (item->m_left_grip_ref_valid && (m_left_grip_reaching || m_left_grip_weight > 0.f || m_left_grip_hold > 0.f))
			{
				left_grip_reference(item);
			}

			if (!item->m_left_grip_ref_valid)
			{
				m_left_grip_reaching = false;
			}

			const bool hold = m_left_grip_hold > 0.f && item->m_left_grip_ref_valid;
			m_left_grip_hold = std::max(m_left_grip_hold - Device.fTimeDelta, 0.f);

			const float release_time = m_left_grip_release_by_mark ? item->m_left_grip_blend : std::max(CCameraRig::HudInertialization(), 0.2f);
			m_left_grip_weight = m_left_grip_reaching || hold
				? std::min(m_left_grip_weight + Device.fTimeDelta / item->m_left_grip_blend, 1.f)
				: std::max(m_left_grip_weight - Device.fTimeDelta / release_time, 0.f);
			grip_weight = m_left_grip_weight * m_left_grip_weight * (3.f - 2.f * m_left_grip_weight);

			float arm_target = 1.f;
			if (m_left_grip_hand_only)
			{
				const Fmatrix& current = model->LL_GetTransform(ancor);
				Fquaternion q_current, q_idle;
				q_current.set(current);
				q_idle.set(item->m_left_grip_ancor_ref);
				const float dot = std::min(std::abs(q_current.x * q_idle.x + q_current.y * q_idle.y + q_current.z * q_idle.z + q_current.w * q_idle.w), 1.f);
				const float deviation = std::max(current.c.distance_to(item->m_left_grip_ancor_ref.c) / 0.1f, 2.f * acosf(dot) / deg2rad(30.f));
				arm_target = std::clamp((1.f - deviation) / 0.7f, 0.f, 1.f);
			}

			m_left_grip_arm_weight = m_left_grip_weight <= 0.f ? arm_target
				: m_left_grip_arm_weight + std::clamp(arm_target - m_left_grip_arm_weight, -Device.fTimeDelta / item->m_left_grip_blend, Device.fTimeDelta / item->m_left_grip_blend);
			grip_target.mul_43(grip, item->m_left_grip_ref);
		}
		else
		{
			if (active && !item->m_left_ik_fixed)
			{
				follow_grip(m_left_ik_weight < 1.f);
			}

			grip_target.mul_43(grip, item->m_left_ik_offset);
		}
	}

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

	const Fvector left_goal = Fvector().lerp(left_target.c, grip_target.c, has_grip ? grip_weight : 0.f);
	for (int i = 0; i < 4; i++)
	{
		if (has_right)
		{
			reach(m_right_ik_chain, right_target.c);
		}

		if (has_left)
		{
			reach(m_left_ik_chain, left_goal);
		}
	}

	if (shift.square_magnitude() > EPS_S)
	{
		Fvector world_shift;
		m_transform.transform_dir(world_shift, shift);
		m_transform.c.add(world_shift);
		right_target.c.sub(shift);
		left_target.c.sub(shift);
		grip_target.c.sub(shift);
		delta.mulA_43(Fmatrix().translate(Fvector().invert(shift)));
	}

	u16 bone = ancor;
	while (bone != BI_NONE && bone != m_right_ik_chain[2])
	{
		bone = model->LL_GetData(bone).GetParentID();
	}

	if (bone == BI_NONE || !has_right)
	{
		CCameraRig::TransformBone(model, ancor, delta);
	}

	if (has_right)
	{
		CCameraRig::SolveLimb(model, m_right_ik_chain, right_target, 1.f);
	}

	if (has_left)
	{
		const auto blend_local = [&](u16 bone, const Fquaternion& target, float weight)
		{
			const Fmatrix parent = model->LL_GetTransform(model->LL_GetData(bone).GetParentID());
			const Fmatrix bone_ms = model->LL_GetTransform(bone);
			const Fmatrix local = Fmatrix().mul_43(Fmatrix().invert(parent), bone_ms);

			Fquaternion anim, result;
			anim.set(local);
			result.slerp(anim, target, weight);

			Fmatrix new_local;
			new_local.rotation(result);
			new_local.c = local.c;
			const Fmatrix new_ms = Fmatrix().mul_43(parent, new_local);
			CCameraRig::TransformBone(model, bone, Fmatrix().mul_43(new_ms, Fmatrix().invert(bone_ms)));
		};

		const bool grip_pose = has_grip && item->m_left_grip_blend > 0.f && item->m_left_grip_ref_valid && grip_weight > 0.f;

		const xr_vector<u16> arm_bones = left_arm_subtree();
		if (grip_pose && item->m_left_grip_pose_ref.size() == arm_bones.size())
		{
			const xr_vector<u16> upper_bones = left_arm_blend_bones();
			const float arm = m_left_grip_arm_weight * m_left_grip_arm_weight * (3.f - 2.f * m_left_grip_arm_weight);
			for (size_t i = 0; i < arm_bones.size(); ++i)
			{
				const bool upper = std::find(upper_bones.begin(), upper_bones.end(), arm_bones[i]) != upper_bones.end();
				blend_local(arm_bones[i], item->m_left_grip_pose_ref[i], upper ? grip_weight * arm : grip_weight);
			}

			Fquaternion q_anim, q_grip, q_blend;
			q_anim.set(left_target);
			q_grip.set(grip_target);
			q_blend.slerp(q_anim, q_grip, grip_weight);

			Fmatrix target;
			target.rotation(q_blend);
			target.c.lerp(left_target.c, grip_target.c, grip_weight);
			CCameraRig::SolveLimb(model, m_left_ik_chain, target, 1.f);
		}
		else
		{
			CCameraRig::SolveLimb(model, m_left_ik_chain, left_target, 1.f);
			if (has_grip && grip_weight > 0.f)
			{
				CCameraRig::SolveLimb(model, m_left_ik_chain, grip_target, grip_weight);
			}
		}
	}

	item->update(true);
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

				hud_play_cycle(m_model, pid, M, part == 0 && pid == 0 && attached_item(1) ? true : bMixIn, speed, m_cycle_fade);
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
		origin.mad							(diff_dir, inertion.OriginOffset);

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
	if (m_left_grip_weight > 0.f && m_model != nullptr && strstr(motion.c_str(), "anm_idle") == motion.c_str())
	{
		for (u16 bone : left_arm_subtree())
		{
			m_model->LL_SetInertializationBone(bone, false);
		}

		m_left_grip_snap_frame = Device.dwFrame;

		const attachable_hud_item* item = m_attached_items[0];
		const float arm_return = item != nullptr ? (1.f - m_left_grip_arm_weight) * item->m_left_grip_blend : 0.f;
		m_left_grip_hold = CCameraRig::HudInertialization() > 0.f ? 0.f : std::max(std::max(item != nullptr ? item->m_idle_fade : 0.f, 0.3f), arm_return) + 0.05f;
	}
	else
	{
		m_left_grip_hold = 0.f;
	}

	attachable_hud_item* item = m_attached_items[0];
	const u8 mode = item != nullptr && item->m_left_grip_blend > 0.f && CCameraRig::Enabled() ? item->left_grip_motion_mode(motion) : 0;
	m_left_grip_reaching = mode != 0;
	m_left_grip_hand_only = mode == 2;
	m_left_grip_release_by_mark = false;

	if (item == nullptr || item->m_left_grip_blend <= 0.f || !CCameraRig::Enabled() || m_model == nullptr)
	{
		return;
	}

	bool shown = m_left_grip_weight >= 1.f;
	if (!shown)
	{
		const player_hud_motion* anm = item->m_hand_motions.find_motion(item->m_left_grip_target);
		shown = anm != nullptr && !anm->m_animations.empty() && hud_motion_amount(m_model, anm->m_animations[0].mid) >= 0.99f;
	}

	if (mode == 0 && shown)
	{
		if (const player_hud_motion* anm = item->m_hand_motions.find_motion(motion))
		{
			for (const motion_descr& descr : anm->m_animations)
			{
				const CMotionDef* def = m_model->LL_GetMotionDef(descr.mid);
				if (def != nullptr && std::any_of(def->marks.begin(), def->marks.end(), [&](const motion_marks& mark) { return xr_strcmp(mark.name, HudGripMarks::End) == 0 && !mark.is_empty(); }))
				{
					m_left_grip_reaching = true;
					m_left_grip_hold = 0.f;
					break;
				}
			}
		}
	}

	if (m_left_grip_reaching && shown)
	{
		m_left_grip_weight = 1.f;
	}
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

void player_hud::OnHudMotionMark(const attachable_hud_item* item, const shared_str& mark)
{
	if (item != nullptr && item == m_attached_items[0] && item->m_left_grip_blend > 0.f && CCameraRig::Enabled())
	{
		if (xr_strcmp(mark, HudGripMarks::Start) == 0)
		{
			m_left_grip_reaching = true;
			m_left_grip_release_by_mark = false;
		}
		else if (xr_strcmp(mark, HudGripMarks::End) == 0)
		{
			m_left_grip_reaching = false;
			m_left_grip_hold = 0.f;
			m_left_grip_release_by_mark = true;
		}
	}
}

void player_hud::OnShotAnimation(const attachable_hud_item* item)
{
	if (item == nullptr || !CCameraRig::Enabled())
	{
		return;
	}

	const float variation = item->m_shot_variation;
	m_shot_rot_scale.set(1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation));
	m_shot_pos_scale.set(1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation), 1.f + ::Random.randFs(variation));
	m_shot_twist.set(::Random.randFs(item->m_shot_twist), ::Random.randFs(item->m_shot_twist));
}

attachable_hud_item* player_hud::create_hud_item(const shared_str& sect)
{
	EnsureDefaultLoaded();
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
	EnsureDefaultLoaded();
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
		result.mul_43(m_transform, ancor_m);
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

static shared_str DefaultPlayerHudSection()
{
	static shared_str actorHudDefault = pSettings->read_if_exists<str_c>(
		"actor", "player_hud_default", "actor_hud");
	return actorHudDefault;
}

void player_hud::load_default()
{
	load(DefaultPlayerHudSection());
}

void player_hud::RequestDefault()
{
	PendingDefaultSect = DefaultPlayerHudSection();
	for (const char* Line : { "visual", "legs_visual", "visual_watches" })
	{
		if (pSettings->line_exist(PendingDefaultSect, Line))
		{
			::Render->model_Request(pSettings->r_string(PendingDefaultSect, Line));
		}
	}
}

void player_hud::FinishDefaultLoad(bool IsEarly)
{
	if (!PendingDefaultSect.size())
	{
		return;
	}

#ifdef DEBUG
	if (IsEarly)
	{
		Msg("! player_hud loaded before the end of level loading");
	}
#endif
	PROF_EVENT("player_hud::FinishDefaultLoad");
	const shared_str Section = PendingDefaultSect;
	PendingDefaultSect = nullptr;
	load(Section);
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

		m_item_attach[EHudOffsetAxis::eAxisPos] = pSettings->read_if_exists<Fvector3>(section.c_str(), "item_position", zero_vel);
		m_item_attach[EHudOffsetAxis::eAxisRot] = pSettings->read_if_exists<Fvector3>(section.c_str(), "item_orientation", zero_vel);
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
		fd.vLastFP.add(Device.vCameraPosition);

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
