#include "stdafx.h"
#include "player_hud.h"
#include "HudItem.h"
#include "ui_base.h"
#include "Actor.h"
#include "physic_item.h"
#include "actoreffector.h"
#include "../xrEngine/IGame_Persistent.h"
#include "InertionData.h"
#include "Inventory.h"
#include "WeaponBinoculars.h"

player_hud* g_player_hud = nullptr;
player_hud* g_player_hud2 = nullptr;
Fvector _ancor_pos;
Fvector _wpn_root_pos;

float CalcMotionSpeed(const shared_str& anim_name)
{

	if(!IsGameTypeSingle() && (anim_name=="anm_show" || anim_name=="anm_hide") )
		return 2.0f;
	else
		return 1.0f;
}

player_hud_motion* player_hud_motion_container::find_motion(const shared_str& name)
{
	xr_vector<player_hud_motion>::iterator it	= m_anims.begin();
	xr_vector<player_hud_motion>::iterator it_e = m_anims.end();
	for(;it!=it_e;++it)
	{
		const shared_str& s = (true)?(*it).m_alias_name:(*it).m_base_name;
		if( s == name)
			return &(*it);
	}
	return nullptr;
}

void player_hud_motion_container::load(IKinematicsAnimated* model, const shared_str& sect)
{
	CInifile::Sect& _sect		= pSettings->r_section(sect);
	CInifile::SectCIt _b		= _sect.Data.begin();
	CInifile::SectCIt _e		= _sect.Data.end();
	player_hud_motion* pm		= nullptr;
	
	string512					buff;
	MotionID					motion_ID;

	for(;_b!=_e;++_b)
	{
		if(strstr(_b->first.c_str(), "anm_")==_b->first.c_str())
		{
			const shared_str& anm	= _b->second;
			m_anims.resize			(m_anims.size()+1);
			pm						= &m_anims.back();
			//base and alias name
			pm->m_alias_name		= _b->first;
			
			if(_GetItemCount(anm.c_str())==1)
			{
				pm->m_base_name			= anm;
				pm->m_additional_name	= anm;
				pm->m_anim_speed = 1.f;
			}else
			{
				R_ASSERT2(_GetItemCount(anm.c_str()) <= 3, anm.c_str());
				string512				str_item;
				_GetItem(anm.c_str(),0,str_item);
				pm->m_base_name			= str_item;

				_GetItem(anm.c_str(),1,str_item);
				pm->m_additional_name = (strlen(str_item) > 0)
					? pm->m_additional_name = str_item
					: pm->m_base_name;

				_GetItem(anm.c_str(), 2, str_item);
				pm->m_anim_speed = strlen(str_item) > 0
					? atof(str_item)
					: 1.f;
			}

			//and load all motions for it

			for(u32 i=0; i<=8; ++i)
			{
				if(i==0)
					xr_strcpy				(buff,pm->m_base_name.c_str());		
				else
					xr_sprintf				(buff,"%s%d",pm->m_base_name.c_str(),i);		

				motion_ID				= model->ID_Cycle_Safe(buff);
				if(motion_ID.valid())
				{
					pm->m_animations.resize			(pm->m_animations.size()+1);
					pm->m_animations.back().mid		= motion_ID;
					pm->m_animations.back().name	= buff;
				}
			}
			R_ASSERT2(pm->m_animations.size(), make_string<const char*>("motion not found [%s]", pm->m_base_name.c_str()));
		}
	}

#ifndef MASTER_GOLD
	static bool NeedReg = true;

	if (!NeedReg)
		return;

	NeedReg = false;

	CImGuiManager::Instance().Subscribe("HudAdjust", CImGuiManager::ERenderPriority::eMedium, [this]
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
			float StartY = ImGui::GetCursorPosY();
			float StartX = ImGui::GetCursorPosX();
			if (ImGui::Button("Mode 1", { 60, 35 }))
				hud_adj_mode = 1;

			ImGui::SetCursorPos({ StartX + 75 , StartY });
			if (ImGui::Button("Mode 2", { 60, 35 }))
				hud_adj_mode = 2;

			ImGui::SetCursorPos({ StartX + 150 , StartY });
			if (ImGui::Button("Mode 3", { 60, 35 }))
				hud_adj_mode = 3;

			ImGui::SetCursorPos({ StartX + 225 , StartY });
			if (ImGui::Button("Mode 4", { 60, 35 }))
				hud_adj_mode = 4;

			ImGui::SetCursorPos({ StartX + 300 , StartY });
			if (ImGui::Button("Mode 5", { 60, 35 }))
				hud_adj_mode = 5;

			ImGui::SetCursorPos({ StartX , StartY + 45 });
			if (ImGui::Button("Mode 6", { 60, 35 }))
				hud_adj_mode = 6;

			ImGui::SetCursorPos({ StartX + 75 , StartY + 45 });
			if (ImGui::Button("Mode 7", { 60, 35 }))
				hud_adj_mode = 7;

			ImGui::SetCursorPos({ StartX + 150 , StartY + 45 });
			if (ImGui::Button("Mode 8", { 60, 35 }))
				hud_adj_mode = 8;

			ImGui::SetCursorPos({ StartX + 225 , StartY + 45 });
			if (ImGui::Button("Mode 9", { 60, 35 }))
				hud_adj_mode = 9;

			ImGui::SetCursorPos({ StartX + 300 , StartY + 45 });
			if (ImGui::Button("Crosshair", { 100, 35 }))
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
#endif
}

Fvector& attachable_hud_item::hands_attach_pos()
{
	return m_measures.m_hands_attach[0];
}

Fvector& attachable_hud_item::hands_attach_rot()
{
	return m_measures.m_hands_attach[1];
}

Fvector& attachable_hud_item::hands_offset_pos()
{
	u8 idx	= m_parent_hud_item->GetCurrentHudOffsetIdx();
	return m_measures.m_hands_offset[0][idx];
}

Fvector& attachable_hud_item::hands_offset_rot()
{
	u8 idx	= m_parent_hud_item->GetCurrentHudOffsetIdx();
	return m_measures.m_hands_offset[1][idx];
}

void attachable_hud_item::set_hands_offset_pos(Fvector3 offset)
{
	m_measures.m_hands_attach[0].set(offset);
}

void attachable_hud_item::set_hands_offset_rot(Fvector3 offset)
{
	m_measures.m_hands_attach[1].set(offset);
}

void attachable_hud_item::set_bone_visible(const shared_str& bone_name, BOOL bVisibility, BOOL bSilent)
{
	u16 bone_id = m_model->LL_BoneID(bone_name);

	if(bone_id==BI_NONE)
	{
		if(bSilent)	return;
		R_ASSERT2	(0, make_string<const char*>("model [%s] has no bone [%s]",pSettings->r_string(m_sect_name, "item_visual"), bone_name.c_str()));
	}
	m_model->LL_SetBoneVisible	(bone_id,bVisibility, FALSE);
}

void attachable_hud_item::update(bool bForce)
{
	if(!bForce && m_upd_firedeps_frame==Device.dwFrame)	return;
	bool is_16x9 = UI().is_widescreen();
	
	if(!!m_measures.m_prop_flags.test(hud_item_measures::e_16x9_mode_now)!=is_16x9)
		m_measures.load(m_sect_name, m_model);

	Fvector ypr						= m_measures.m_item_attach[1];
	ypr.mul							(PI/180.f);
	m_attach_offset.setHPB			(ypr.x,ypr.y,ypr.z);
	m_attach_offset.translate_over	(m_measures.m_item_attach[0]);

	m_parent->calc_transform		(m_attach_place_idx, m_attach_offset, m_item_transform);
	m_upd_firedeps_frame			= Device.dwFrame;

	IKinematicsAnimated* ka			=	m_model->dcast_PKinematicsAnimated();
	if(ka)
	{
		ka->UpdateTracks									();
		ka->dcast_PKinematics()->CalculateBones_Invalidate	();
		ka->dcast_PKinematics()->CalculateBones				(TRUE);
	}
}

void attachable_hud_item::update_hud_additional(Fmatrix& trans)
{
	if(m_parent_hud_item)
	{
		m_parent_hud_item->UpdateHudAdditonal(trans);
	}
}

void attachable_hud_item::setup_firedeps(firedeps& fd)
{
	update							(false);
	// fire point&direction
	if(m_measures.m_prop_flags.test(hud_item_measures::e_fire_point))
	{
		Fmatrix& fire_mat								= m_model->LL_GetTransform(m_measures.m_fire_bone);
		fire_mat.transform_tiny							(fd.vLastFP, m_measures.m_fire_point_offset);
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

	if(m_measures.m_prop_flags.test(hud_item_measures::e_fire_point2))
	{
		Fmatrix& fire_mat			= m_model->LL_GetTransform(m_measures.m_fire_bone2);
		fire_mat.transform_tiny		(fd.vLastFP2,m_measures.m_fire_point2_offset);
		m_item_transform.transform_tiny	(fd.vLastFP2);
		VERIFY(_valid(fd.vLastFP2));
		VERIFY(_valid(fd.vLastFP2));
	}

	if(m_measures.m_prop_flags.test(hud_item_measures::e_shell_point))
	{
		Fmatrix& fire_mat			= m_model->LL_GetTransform(m_measures.m_shell_bone);
		fire_mat.transform_tiny		(fd.vLastSP,m_measures.m_shell_point_offset);
		m_item_transform.transform_tiny	(fd.vLastSP);
		VERIFY(_valid(fd.vLastSP));
		VERIFY(_valid(fd.vLastSP));
	}
}

bool  attachable_hud_item::need_renderable()
{
	return m_parent_hud_item->need_renderable();
}

void attachable_hud_item::render()
{
	::Render->set_Transform		(&m_item_transform);
	::Render->add_Visual		(m_model->dcast_RenderVisual(), true);
	debug_draw_firedeps			();
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

void hud_item_measures::load(const shared_str& sect_name, IKinematics* K)
{
	Fvector def = { 0.f, 0.f, 0.f };
	bool is_16x9 = UI().is_widescreen();
	string64	_prefix;
	xr_sprintf	(_prefix,"%s",is_16x9?"_16x9":"");
	string128	val_name;

	xr_strconcat(val_name,"hands_position",_prefix);
	m_hands_attach[0]			= pSettings->r_fvector3(sect_name, val_name);
	xr_strconcat(val_name,"hands_orientation",_prefix);
	m_hands_attach[1]			= pSettings->r_fvector3(sect_name, val_name);

	m_item_attach[0]			= READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "item_position", def);
	m_item_attach[1]			= READ_IF_EXISTS(pSettings, r_fvector3, sect_name, "item_orientation", def);

	shared_str					 bone_name;
	m_prop_flags.set			 (e_fire_point,pSettings->line_exist(sect_name,"fire_bone"));
	if(m_prop_flags.test(e_fire_point))
	{
		bone_name				= pSettings->r_string(sect_name, "fire_bone");
		m_fire_bone				= K->LL_BoneID(bone_name);
		m_fire_point_offset		= pSettings->r_fvector3(sect_name, "fire_point");
	}else
		m_fire_point_offset.set(0,0,0);

	m_prop_flags.set			 (e_fire_point2,pSettings->line_exist(sect_name,"fire_bone2"));
	if(m_prop_flags.test(e_fire_point2))
	{
		bone_name				= pSettings->r_string(sect_name, "fire_bone2");
		m_fire_bone2			= K->LL_BoneID(bone_name);
		m_fire_point2_offset	= pSettings->r_fvector3(sect_name, "fire_point2");
	}else
		m_fire_point2_offset.set(0,0,0);

	m_prop_flags.set			 (e_shell_point,pSettings->line_exist(sect_name,"shell_bone"));
	if(m_prop_flags.test(e_shell_point))
	{
		bone_name				= pSettings->r_string(sect_name, "shell_bone");
		m_shell_bone			= K->LL_BoneID(bone_name);
		m_shell_point_offset	= pSettings->r_fvector3(sect_name, "shell_point");
	}else
		m_shell_point_offset.set(0,0,0);

	m_hands_offset[0][0].set	(0,0,0);
	m_hands_offset[1][0].set	(0,0,0);

	xr_strconcat(val_name,"aim_hud_offset_pos",_prefix);
	m_hands_offset[0][1]		= READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, def);
	xr_strconcat(val_name,"aim_hud_offset_rot",_prefix);
	m_hands_offset[1][1]		= READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, def);

	xr_strconcat(val_name,"gl_hud_offset_pos",_prefix);
	m_hands_offset[0][2]		= READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, def);
	xr_strconcat(val_name,"gl_hud_offset_rot",_prefix);
	m_hands_offset[1][2]		= READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, def);

	//--> �������� � �������
	xr_strconcat(val_name, "strafe_hud_offset_pos", _prefix);
	m_strafe_offset[0][0] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.015f, 0.f, 0.f));
	xr_strconcat(val_name, "strafe_hud_offset_rot", _prefix);
	m_strafe_offset[1][0] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.f, 0.f, 4.5f));

	//--> ������� � �������
	xr_strconcat(val_name, "strafe_aim_hud_offset_pos", _prefix);
	m_strafe_offset[0][1] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.005f, 0.f, 0.f));
	xr_strconcat(val_name, "strafe_aim_hud_offset_rot", _prefix);
	m_strafe_offset[1][1] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.f, 0.f, 2.5f));

	//--> ��������� �������
	bool bStrafeEnabled = READ_IF_EXISTS(pSettings, r_bool, sect_name, "strafe_enabled", true);
	bool bStrafeEnabled_aim = READ_IF_EXISTS(pSettings, r_bool, sect_name, "strafe_aim_enabled", false);
	float fFullStrafeTime = READ_IF_EXISTS(pSettings, r_float, sect_name, "strafe_transition_time", 0.5f);
	float fFullStrafeTime_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "strafe_aim_transition_time", 0.15f);
	float fStrafeCamLFactor = READ_IF_EXISTS(pSettings, r_float, sect_name, "strafe_cam_limit_factor", 0.5f);
	float fStrafeCamLFactor_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "strafe_cam_limit_aim_factor", 1.0f);
	float fStrafeMinAngle = READ_IF_EXISTS(pSettings, r_float, sect_name, "strafe_cam_min_angle", 0.0f);
	float fStrafeMinAngle_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "strafe_cam_aim_min_angle", 7.0f);

	//--> (Data 1)
	m_strafe_offset[2][0].set((bStrafeEnabled ? 1.0f : 0.0f), fFullStrafeTime, 0); // normal
	m_strafe_offset[2][1].set((bStrafeEnabled_aim ? 1.0f : 0.0f), fFullStrafeTime_aim, 0); // aim-GL

	//--> (Data 2)
	m_strafe_offset[3][0].set(fStrafeCamLFactor, fStrafeMinAngle, 0); // normal
	m_strafe_offset[3][1].set(fStrafeCamLFactor_aim, fStrafeMinAngle_aim, 0); // aim-GL

	m_inertion_params.m_tendto_speed = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_speed", 1.0f);
	m_inertion_params.m_tendto_speed_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_aim_speed", 1.0f);
	m_inertion_params.m_tendto_ret_speed = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_ret_speed", 5.0f);
	m_inertion_params.m_tendto_ret_speed_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_tendto_ret_aim_speed", 5.0f);

	m_inertion_params.m_min_angle = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_min_angle", 0.0f);
	m_inertion_params.m_min_angle_aim = READ_IF_EXISTS(pSettings, r_float, sect_name, "inertion_min_angle_aim", 3.5f);

	m_inertion_params.m_offset_LRUD = READ_IF_EXISTS(pSettings, r_fvector4, sect_name, "inertion_offset_LRUD", Fvector4().set(0.04f, 0.04f, 0.04f, 0.02f));
	m_inertion_params.m_offset_LRUD_aim = READ_IF_EXISTS(pSettings, r_fvector4, sect_name, "inertion_offset_LRUD_aim", Fvector4().set(0.015f, 0.015f, 0.01f, 0.005f));

	R_ASSERT2(pSettings->line_exist(sect_name,"fire_point")==pSettings->line_exist(sect_name,"fire_bone"),		sect_name.c_str());
	R_ASSERT2(pSettings->line_exist(sect_name,"fire_point2")==pSettings->line_exist(sect_name,"fire_bone2"),	sect_name.c_str());
	R_ASSERT2(pSettings->line_exist(sect_name,"shell_point")==pSettings->line_exist(sect_name,"shell_bone"),	sect_name.c_str());

	m_prop_flags.set(e_16x9_mode_now,is_16x9);
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

	// Visual
	const shared_str& visual_name = pSettings->r_string(sect_name, "item_visual");
	m_model						 = smart_cast<IKinematics*>(::Render->model_Create(visual_name.c_str()));

	m_attach_place_idx			= pSettings->r_u16(sect_name, "attach_place_idx");
	m_measures.load				(sect_name, m_model);
}

void attachable_hud_item::anim_play(const shared_str& item_anm_name, BOOL bMixIn, float speed)
{
	if(m_model->dcast_PKinematicsAnimated())
	{
		IKinematicsAnimated* ka			= m_model->dcast_PKinematicsAnimated();

		MotionID M2						= ka->ID_Cycle_Safe(item_anm_name);
		if(!M2.valid())
			M2							= ka->ID_Cycle_Safe("idle");
		else
			if(bDebug)
				Msg						("playing item animation [%s]",item_anm_name.c_str());
		
		R_ASSERT3(M2.valid(),"model has no motion [idle] ", pSettings->r_string(m_sect_name, "item_visual"));

		u16 root_id						= m_model->LL_GetBoneRoot();
		CBoneInstance& root_binst		= m_model->LL_GetBoneInstance(root_id);
		root_binst.set_callback_overwrite(TRUE);
		root_binst.mTransform.identity	();

		u16 pc							= ka->partitions().count();
		for(u16 pid=0; pid<pc; ++pid)
		{
			CBlend* B					= ka->PlayCycle(pid, M2, bMixIn);
			R_ASSERT					(B);
			B->speed					*= speed;
		}

		m_model->CalculateBones_Invalidate	();
	}
}

u32 attachable_hud_item::anim_play(const shared_str& anm_name_b, BOOL bMixIn, const CMotionDef*& md, u8& rnd_idx)
{
	R_ASSERT				(strstr(anm_name_b.c_str(),"anm_")==anm_name_b.c_str());
	string256				anim_name_r;
	bool is_16x9			= UI().is_widescreen();
	xr_sprintf				(anim_name_r,"%s%s",anm_name_b.c_str(),((m_attach_place_idx==1)&&is_16x9)?"_16x9":"");

	player_hud_motion* anm	= m_hand_motions.find_motion(anim_name_r);
	R_ASSERT2				(anm, make_string<const char*>("model [%s] has no motion alias defined [%s]", m_sect_name.c_str(), anim_name_r));
	R_ASSERT2				(anm->m_animations.size(), make_string<const char*>("model [%s] has no motion defined in motion_alias [%s]", pSettings->r_string(m_sect_name, "item_visual"), anim_name_r));
	
	rnd_idx					= (u8)Random.randI(anm->m_animations.size()) ;
	const motion_descr& M	= anm->m_animations[ rnd_idx ];
	float speed = anm->m_anim_speed;

	u32 ret					= g_player_hud->anim_play(m_attach_place_idx, M.mid, bMixIn, md, speed);
	
	if(m_model->dcast_PKinematicsAnimated())
	{
		shared_str item_anm_name;
		if(anm->m_base_name!=anm->m_additional_name)
			item_anm_name = anm->m_additional_name;
		else
			item_anm_name = M.name;

		anim_play(item_anm_name, bMixIn, speed);
	}

	R_ASSERT2		(m_parent_hud_item, "parent hud item is nullptr");
	CPhysicItem&	parent_object = m_parent_hud_item->object();
	//R_ASSERT2		(parent_object, "object has no parent actor");
	//CObject*		parent_object = static_cast_checked<CObject*>(&m_parent_hud_item->object());

	if (IsGameTypeSingle() && parent_object.H_Parent() == Level().CurrentControlEntity())
	{
		CActor* current_actor	= static_cast<CActor*>(Level().CurrentControlEntity());
		VERIFY					(current_actor);
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

	time_accumulator = 0;
	tocrouch_time_remains = 0;
	fromcrouch_time_remains = 0;
	
	toslowcrouch_time_remains = 0;
	fromslowcrouch_time_remains = 0;
	
	torlookout_time_remains = 0;
	fromrlookout_time_remains = 0;
	tollookout_time_remains = 0;
	fromllookout_time_remains = 0;
}

player_hud::~player_hud()
{
	IRenderVisual* v			= m_model->dcast_RenderVisual();
	::Render->model_Delete		(v);
	m_model						= nullptr;

	xr_vector<attachable_hud_item*>::iterator it	= m_pool.begin();
	xr_vector<attachable_hud_item*>::iterator it_e	= m_pool.end();
	for(;it!=it_e;++it)
	{
		attachable_hud_item* a	= *it;
		xr_delete				(a);
	}
	m_pool.clear				();
}

void player_hud::load(const shared_str& player_hud_sect)
{
	if(player_hud_sect == m_sect_name) {
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

	const shared_str& model_name = pSettings->r_string(player_hud_sect, "visual");
	m_model = smart_cast<IKinematicsAnimated*>(::Render->model_Create(model_name.c_str()));

	if(pSettings->line_exist(player_hud_sect, "legs_visual")) {
		auto model_name = pSettings->r_string(player_hud_sect, "legs_visual");
		m_legs_model = PKinematics(::Render->model_Create(model_name));
	}

	u16 l_arm = m_model->dcast_PKinematics()->LL_BoneID("l_clavicle");
	m_model->dcast_PKinematics()->LL_GetBoneInstance(l_arm).set_callback(bctCustom, [](CBoneInstance* B) {g_player_hud->LeftArmCallback(B); }, NULL);

	auto& _sect = pSettings->r_section(player_hud_sect);
	auto _b = _sect.Data.begin();
	auto _e = _sect.Data.end();

	for(; _b != _e; ++_b) 
	{
		if(strstr(_b->first.c_str(), "ancor_") == _b->first.c_str())
		{
			const shared_str& _bone = _b->second;
			m_ancors.push_back(m_model->dcast_PKinematics()->LL_BoneID(_bone));
		}
	}

	if(!b_reload) {
		m_model->PlayCycle("hand_idle_doun");
	}
	else {
		if(m_attached_items[1]) {
			m_attached_items[1]->m_parent_hud_item->on_a_hud_attach();
		}

		if(m_attached_items[0]) {
			m_attached_items[0]->m_parent_hud_item->on_a_hud_attach();
		}
	}

	m_model->dcast_PKinematics()->CalculateBones_Invalidate();
	m_model->dcast_PKinematics()->CalculateBones(TRUE);

	if(m_legs_model) {
		m_legs_model->CalculateBones_Invalidate();
		m_legs_model->CalculateBones(TRUE);
	}

	if(Actor()) {
		float m_fLegs_shift = READ_IF_EXISTS(pSettings, r_float, "actor_hud", "legs_shift_delta", -0.55f);
		Actor()->m_fLegs_shift = READ_IF_EXISTS(pSettings, r_float, player_hud_sect, "legs_shift_delta", m_fLegs_shift);
	}
}

bool player_hud::render_item_ui_query()
{
	bool res = false;
	if(m_attached_items[0])
		res |= m_attached_items[0]->render_item_ui_query();

	if(m_attached_items[1])
		res |= m_attached_items[1]->render_item_ui_query();

	return res;
}

void player_hud::render_item_ui()
{
	if(m_attached_items[0])
		m_attached_items[0]->render_item_ui();

	if(m_attached_items[1])
		m_attached_items[1]->render_item_ui();
}

void player_hud::render_hud()
{
	bool b_r0 = (m_attached_items[0] && m_attached_items[0]->need_renderable());
	bool b_r1 = (m_attached_items[1] && m_attached_items[1]->need_renderable());

	if(b_r0 || b_r1 || m_bhands_visible) {
		::Render->set_Transform(&m_transform);
		::Render->add_Visual(m_model->dcast_RenderVisual(), true);
	}

	if(b_r0) {
		m_attached_items[0]->render();
	}

	if(b_r1) {
		m_attached_items[1]->render();
	}

	if(Actor() && m_legs_model) {
		bool isClimb = Actor()->GetMovementState(ACTOR_DEFS::EMovementStates::eReal) & mcClimb;
		if(!isClimb) {
			auto bHud = ::Render->get_HUD();
			IKinematics* actor_model = Actor()->Visual()->dcast_PKinematics();

			actor_model->CalculateBones(TRUE);

			m_legs_model->CalculateBones_Invalidate();
			m_legs_model->CalculateBones(TRUE);

			if(m_legs_model->LL_BoneCount() == actor_model->LL_BoneCount()) {
				for(u16 i = 0; i < m_legs_model->LL_BoneCount(); ++i) {
					auto& BoneInstance = m_legs_model->LL_GetBoneInstance(i);
					BoneInstance.mTransform.set(actor_model->LL_GetBoneInstance(i).mTransform);
					BoneInstance.mRenderTransform.mul_43(BoneInstance.mTransform, m_legs_model->LL_GetData(i).m2b_transform);
				}
			}
			else {
				auto setBoneTransform = [actor_model, this](u16 ID, shared_str bonename) {
					auto BoneID = actor_model->LL_BoneID(bonename);
					if(BoneID != BI_NONE) {
						auto& BoneInstance = m_legs_model->LL_GetBoneInstance(ID);
						BoneInstance.mTransform.set(actor_model->LL_GetBoneInstance(BoneID).mTransform);
						BoneInstance.mRenderTransform.mul_43(BoneInstance.mTransform, m_legs_model->LL_GetData(ID).m2b_transform);
					}
				};

				setBoneTransform(0, "root_stalker");
				setBoneTransform(1, "bip01");

				shared_str bonename;
				for(u16 i = 0; i < m_legs_model->LL_BoneCount(); ++i) {
					bonename = m_legs_model->LL_BoneName_dbg(i);
					setBoneTransform(i, bonename);
				}
			}

			auto BoneID = m_legs_model->LL_BoneID("bip01_spine");
			auto& BoneInstance = m_legs_model->LL_GetData(BoneID);
			m_legs_model->Bone_Calculate(&BoneInstance, &m_legs_model->LL_GetTransform(BoneInstance.GetParentID()));

			::Render->set_HUD(FALSE);

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
	attachable_hud_item* pi			= create_hud_item(hud_name);
	player_hud_motion*	pm			= pi->m_hand_motions.find_motion(anim_name);
	if(!pm)
		return						100; // ms TEMPORARY
	R_ASSERT2						(pm, 
		make_string<const char*>("hudItem model [%s] has no motion with alias [%s]", hud_name.c_str(), anim_name.c_str() )
		);
	return motion_length			(pm->m_animations[0].mid, md, speed);
}

u32 player_hud::motion_length(const MotionID& M, const CMotionDef*& md, float speed)
{
	md					= m_model->LL_GetMotionDef(M);
	VERIFY				(md);
	if (md->flags & esmStopAtEnd) 
	{
		CMotion*			motion		= m_model->LL_GetRootMotion(M);
		return				iFloor( 0.5f + 1000.f*motion->GetLength() / (md->Dequantize(md->speed) * speed) );
	}
	return					0;
}

const Fvector& player_hud::attach_rot() const
{
	static Fvector m_last_rot = zero_vel;
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
	if(!m_attached_items[1])
		return;

	B->mTransform.mulA_44(m_attached_items[1]?m_transformL:m_transform);
	B->mTransform.mulA_44(Fmatrix(m_transform).invert());
}

void angle_inertion(Fvector& c_hpb, const Fvector& t_hpb, float speed)
{
	c_hpb.x = angle_inertion(c_hpb.x, t_hpb.x, speed, PI, Device.fTimeDelta);
	c_hpb.x = angle_inertion(c_hpb.x, t_hpb.x, speed, PI, Device.fTimeDelta);
	c_hpb.x = angle_inertion(c_hpb.x, t_hpb.x, speed, PI, Device.fTimeDelta);
}

#include "Missile.h"

void player_hud::update(const Fmatrix& cam_trans)
{
	if(!m_attached_items[0] && !m_attached_items[1])
	{
		m_transform.set(cam_trans);
		m_transformL.set(cam_trans);
		return;
	}

	Fmatrix	trans					= cam_trans;
	update_inertion					(trans);
	update_additional				(trans);


	{
		m_attach_offsetr.setHPB(VPUSH(Fvector(attach_rot()).mul(PI / 180.f)));//generate and set Euler angles
		m_attach_offsetr.c.set(attach_pos());
		m_transform.mul(trans, m_attach_offsetr);
	}

	{
		CMissile* pMiss = m_attached_items[0] && !EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode] ? smart_cast<CMissile*>(m_attached_items[0]->m_parent_hud_item) : NULL;
		bool throwing_missile = pMiss && (pMiss->GetState()>=CMissile::EMissileStates::eThrowStart&&pMiss->GetState()<=CMissile::EMissileStates::eThrow);
		bool left_hand_active = !throwing_missile && m_attached_items[1];

		Fmatrix attach_offset;
		attach_offset.setHPB(VPUSH(Fvector(left_hand_active ? m_attached_items[1]->hands_attach_rot() : attach_rot()).mul(PI / 180.f)));//generate and set Euler angles
		attach_offset.c.set(left_hand_active ? m_attached_items[1]->hands_attach_pos() : attach_pos());
		m_transformL.mul(trans, left_hand_active ? m_attach_offsetl.set(attach_offset) : m_attach_offsetl.inertion(attach_offset, 1-Device.fTimeDelta*10.f));
	}

	m_model->UpdateTracks();
	m_model->dcast_PKinematics()->CalculateBones_Invalidate();
	m_model->dcast_PKinematics()->CalculateBones(TRUE);

	if(m_attached_items[0])
		m_attached_items[0]->update(true);

	if(m_attached_items[1])
		m_attached_items[1]->update(true);
}

u32 player_hud::anim_play(u16 part, const MotionID& M, BOOL bMixIn, const CMotionDef*& md, float speed)
{
	///partitions info
	// 0==default (root_bone)
	// 1==left_hand (left hand bone hierarchy)
	// 2==right_hand (right hand bone hierarchy)
	// please append new bone parts for more realistic behavior of animations
	bool disable_root_part = false;
	u16 part_id							= u16(-1);
	if(attached_item(0) && attached_item(1))
	{
		disable_root_part = part==1;//if we run the animation for the left hand, we don't include the animation for the root bone (only if attached_item 1 active).
		part_id = m_model->partitions().part_id((part==0)?"right_hand":"left_hand");
	}

	CMissile* pMiss = m_attached_items[0] && !EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode] ? smart_cast<CMissile*>(m_attached_items[0]->m_parent_hud_item) : NULL;
	bool throwing_missile = pMiss && (pMiss->GetState()>=CMissile::EMissileStates::eThrowStart&&pMiss->GetState()<=CMissile::EMissileStates::eThrow) && attached_item(1);
	if (throwing_missile)//is the only when attached_item 1 is active and we have started throwing the item
	{
		if(part==0)
		{
			CBlend* B = NULL;
			B	= m_model->PlayCycle(0, M, bMixIn);
			B	= m_model->PlayCycle(1, M, bMixIn);
			B	= m_model->PlayCycle(2, M, bMixIn);
			B->speed *= speed;
		}
	}
	else
	{
		u16 pc					= m_model->partitions().count();
		for(u16 pid=0; pid<pc; ++pid)
		{
			if(pid==0 && disable_root_part)continue;

			if(pid==0 || pid==part_id || part_id==u16(-1))
			{
				if(m_blocked_part_idx==pid) continue;
				CBlend* B = m_model->PlayCycle(pid, M, part==0&&pid==0&&attached_item(1)?TRUE:bMixIn);
				B->speed *= speed;
			}
		}
	}
	m_model->dcast_PKinematics()->CalculateBones_Invalidate	();

	return				motion_length(M, md, speed);
}

void player_hud::update_additional	(Fmatrix& trans)
{
	if(m_attached_items[0] || m_attached_items[0]&&m_attached_items[1])
		m_attached_items[0]->update_hud_additional(trans);
	else
	{
		if(m_attached_items[1])
			m_attached_items[1]->update_hud_additional(trans);
	}
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

		// calc difference
		Fvector								diff_dir;
		diff_dir.sub						(xform.k, st_last_dir);

		// clamp by PI_DIV_2
		Fvector last;						last.normalize_safe(st_last_dir);
		float dot							= last.dotproduct(xform.k);
		if (dot<EPS){
			Fvector v0;
			v0.crossproduct					(st_last_dir,xform.k);
			st_last_dir.crossproduct		(xform.k,v0);
			diff_dir.sub					(xform.k, st_last_dir);
		}

		// tend to forward
		st_last_dir.mad						(diff_dir, inertion.TendtoSpeed*Device.fTimeDelta);
		origin.mad							(diff_dir, inertion.OriginOffset);

		// pitch compensation
		float pitch							= angle_normalize_signed(xform.k.getP());
		origin.mad							(xform.k,	-pitch * inertion.PitchOffsetD);
		origin.mad							(xform.i,	-pitch * inertion.PitchOffsetR);
		origin.mad							(xform.j,	-pitch * inertion.PitchOffsetN);
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
	res->m_hand_motions.load	(m_model, sect);
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
	if(m_attached_items[1])
		return m_attached_items[1]->m_parent_hud_item->CheckCompatibility(item);
	else
	{
		CEntity* pEntity = smart_cast<CEntity*>(Level().CurrentEntity());
		if (pEntity)
		{
			CActor* pActor = smart_cast<CActor*>(pEntity);
			if(pActor)
			{
				CHudItem* pDetector = smart_cast<CHudItem*>(pActor->inventory().ItemFromSlot(DETECTOR_SLOT));
				if(pDetector && pDetector->GetState()!=CHUDState::eHidden)
					return pDetector->CheckCompatibility(item);
			}
		}
	}

	return true;
}

void player_hud::attach_item(CHudItem* item)
{
	attachable_hud_item* pi			= create_hud_item(item->HudSection());
	int item_idx					= pi->m_attach_place_idx;
	
	if (m_attached_items[item_idx] != pi || pi->m_parent_hud_item != item) {
		if(m_attached_items[item_idx])
			m_attached_items[item_idx]->m_parent_hud_item->on_b_hud_detach();

		m_attached_items[item_idx]						= pi;
		pi->m_parent_hud_item							= item;

		if(item_idx==0 && m_attached_items[1])
			m_attached_items[1]->m_parent_hud_item->CheckCompatibility(item);

		item->on_a_hud_attach();
	}
	pi->m_parent_hud_item							= item;
}
void player_hud::RestoreHandBlends(LPCSTR ignored_part)
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
			CBlend* B			= m_model->PlayCycle(pid, M, TRUE);//this can destroy BR calling UpdateTracks !
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
	if( nullptr==attached_item(idx) )					return;

	m_attached_items[idx]->m_parent_hud_item->on_b_hud_detach();

	m_attached_items[idx]->m_parent_hud_item		= nullptr;
	m_attached_items[idx]							= nullptr;

	if(idx==1 && m_attached_items[0])
	{
		m_attached_items[0]->m_parent_hud_item->OnMovementChanged(mcAnyMove);
		RestoreHandBlends("right_hand");
	}
	else if(idx==0 && m_attached_items[1])
	{
		m_model->PlayCycle(2, m_model->ID_Cycle("hand_idle_doun"), FALSE);
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

void player_hud::calc_transform(u16 attach_slot_idx, const Fmatrix& offset, Fmatrix& result)
{
	Fmatrix ancor_m			= m_model->dcast_PKinematics()->LL_GetTransform(m_ancors[attach_slot_idx]);
	result.mul				(m_transform, ancor_m);
	result.mulB_43			(offset);
}

void player_hud::OnMovementChanged(ACTOR_DEFS::EMoveCommand cmd)
{
	CEntity* pEntity = smart_cast<CEntity*>(Level().CurrentEntity());
	if (pEntity)
	{
		CActor* pActor = smart_cast<CActor*>(pEntity);
		if(pActor)
		{
			if(pActor->HUDview())
			{
				if(cmd==0)
				{
					if(m_attached_items[0])
					{
						if(m_attached_items[0]->m_parent_hud_item->GetState()==CHUDState::eIdle)
							m_attached_items[0]->m_parent_hud_item->PlayAnimIdle();
					}
					if(m_attached_items[1])
					{
						if(m_attached_items[1]->m_parent_hud_item->GetState()==CHUDState::eIdle)
							m_attached_items[1]->m_parent_hud_item->PlayAnimIdle();
					}
				}else
				{
					if(m_attached_items[0])
						m_attached_items[0]->m_parent_hud_item->OnMovementChanged(cmd);

					if(m_attached_items[1])
						m_attached_items[1]->m_parent_hud_item->OnMovementChanged(cmd);
				}
			}
			else
			{
				if(cmd==0)
				{
					if(pActor->inventory().ActiveItem())
					{
						CHudItem* pWeap = smart_cast<CHudItem*>(pActor->inventory().ActiveItem());
						if(pWeap && pWeap->GetState()==CHUDState::eIdle)
							pWeap->PlayAnimIdle();
					}
					if(pActor->inventory().ItemFromSlot(DETECTOR_SLOT))
					{
						CHudItem* pDetector = smart_cast<CHudItem*>(pActor->inventory().ItemFromSlot(DETECTOR_SLOT));
						if(pDetector && pDetector->GetState()==CHUDState::eIdle)
							pDetector->PlayAnimIdle();
					}

				}
				else
				{
					if(pActor->inventory().GetActiveSlot() != NO_ACTIVE_SLOT)
					{
						CHudItem* pWeap = smart_cast<CHudItem*>(pActor->inventory().ActiveItem());
						if(pWeap && pWeap->GetState()!=CHUDState::eHidden)
							pWeap->OnMovementChanged(cmd);
					}
					if(pActor->inventory().ItemFromSlot(DETECTOR_SLOT))
					{
						CHudItem* pDetector = smart_cast<CHudItem*>(pActor->inventory().ItemFromSlot(DETECTOR_SLOT));
						if(pDetector && pDetector->GetState()!=CHUDState::eHidden)
							pDetector->OnMovementChanged(cmd);
					}
				}
			}
		}
	}
}

bool player_hud::check_anim(const shared_str& anim_name, u16 place_idx)
{
	if(!m_attached_items[place_idx]) return false;

	string256				anim_name_r;
	bool is_16x9			= UI().is_widescreen();
	xr_sprintf				(anim_name_r,"%s%s",anim_name.c_str(),((place_idx==1)&&is_16x9)?"_16x9":"");

	return !!m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name_r);

	MotionID motion;
	if(m_attached_items[place_idx] && place_idx>=0&&place_idx!=u16(-1))///���� �������� � ���������� ������ �� ������ anm_show
	{
		string256				anim_name_r;
		bool is_16x9			= UI().is_widescreen();
		xr_sprintf				(anim_name_r,"%s%s",anim_name.c_str(),((place_idx==1)&&is_16x9)?"_16x9":"");

		if(m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name_r))
			return true;
	}
	else//����� ����� ������ �� ������� �������� �� ������ abakan_draw ��� fn_2000_reload � ��
	{
		motion = m_model->ID_Cycle_Safe(anim_name);

		if(motion && motion.valid())
			return true;
	}
	return false;
}

//	HUD_HANDS_ANIMATOR
//	anim_name �������� ��������
//	place_idx ������ ������ 0 - ������, 1 - ��������, ����� ������ �������� �������� �������� ������ �������� � ���������� ��� �� ������ ���������
//	part_id ������ ��� ����� 0 - default 1 - left_hand, 2 - right_hand, -1 - �������� ��� ����� ���� ��� ���� ����������� � ��������, ����� ������ �������� �������� �������� ��� ���� ���������
//	bMixIn ����������� � ���������� ���������
//	speed ��������� �������� ��������
//	anm_idx ������ �������� �� ������� ���������
//	impact_on_item ������ �������� �� ������
//	similar_check ��� ���� ���� ����� ��������� ��������� �������� ���� ��� ��� ���� ��������
//	static void Callback ����������� ������� ������� ����� ������� �� ���������� ��������
//	void* CallbackParam �������� � ������� ����� ��������� ��� ������
//	UpdateCallbackType ��� �������� 0 - ��������� �� ��������� �������� 1 - ����� ����������� ���� �������� �� ����������
bool player_hud::animator_play(const shared_str& anim_name, u16 place_idx, u16 part_id, BOOL bMixIn, float speed, u8 anm_idx, bool impact_on_item, bool similar_check, PlayCallback Callback, LPVOID CallbackParam, BOOL UpdateCallbackType)
{
	MotionID motion;
	if(m_attached_items[place_idx] && place_idx>=0&&place_idx!=u16(-1))///���� �������� � ���������� ������ �� ������ anm_show
	{
		string256				anim_name_r;
		bool is_16x9			= UI().is_widescreen();
		xr_sprintf				(anim_name_r,"%s%s",anim_name.c_str(),((place_idx==1)&&is_16x9)?"_16x9":"");

		player_hud_motion* anm	= m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name_r);

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

				m_attached_items[place_idx]->anim_play(item_anm_name, bMixIn, speed);
			}
		}
		else
		{
			Msg("! Animation [%s] not found in %s motion container!", anim_name_r, m_attached_items[place_idx]->m_sect_name.c_str());
		}
	}
	else//����� ����� ������ �� ������� �������� �� ������ abakan_draw ��� fn_2000_reload � ��
	{
		motion = m_model->ID_Cycle_Safe(anim_name);

		if(!motion || !motion.valid())
		{
			Msg("! Animation [%s] not found in %s motion container!", anim_name.c_str(), section_name().c_str());
		}
	}

	//���� �������� �������� stop_at_end �� �� ����� ���������
	u16 pc = m_model->partitions().count();
	for(u16 pid=0; pid<pc; ++pid)
	{
		u32 blends_count = m_model->LL_PartBlendsCount(pid);
		for(u32 blend_id=0; blend_id<blends_count; ++blend_id)
		{
			CBlend* blend = m_model->LL_PartBlend(pid, blend_id);
			if(!blend) continue;
			MotionID M = blend->motionID;
			if(M != motion)	
			{
				if(blend->stop_at_end)
				{
					ResetBlockedPartID();
					return false;
				}
			}
		}
	}
	m_blocked_part_idx = part_id;//��������� ����� ����������� �������� �� ����� �������� ����������

	CBlend* B = NULL;
	switch (part_id)
	{
		case 0:
		case 1:
		case 2:
		{
			if(similar_check)//�������� �� ��������� ��� �����
			{
				u32 blends_count = m_model->LL_PartBlendsCount(part_id);
				for(u32 blend_id=0; blend_id<blends_count; ++blend_id)
				{
					CBlend* blend = m_model->LL_PartBlend(part_id, blend_id);
					if(!blend) continue;
					MotionID M = blend->motionID;
					if(M==motion)
					{
						ResetBlockedPartID();
						return false;
					}
				}
			}
			//�������� �� ��������� ��� �����
			B = m_model->PlayCycle(part_id, motion, bMixIn, Callback, CallbackParam);
		}break;
		case u16(-1):
		{
			if(similar_check)//�������� �� ��� ����� ������� ������� ���������� ��������
			{
				CMotionDef* m_def = m_model->LL_GetMotionDef(motion);
				u32 blends_count = m_model->LL_PartBlendsCount(m_def->bone_or_part);
				for(u32 blend_id=0; blend_id<blends_count; ++blend_id)
				{
					CBlend* blend = m_model->LL_PartBlend(m_def->bone_or_part, blend_id);
					if(!blend) continue;
					MotionID M = blend->motionID;
					if(M==motion)
					{
						ResetBlockedPartID();
						return false;
					}
				}
			}
			//�������� ��� ���� ��� ����� ������� ������ � ���������� ��������
			B = m_model->PlayCycle(motion, bMixIn, Callback, CallbackParam);
		}break;
		default:
		{
			if(similar_check)//�������� �� ���� ��� ������
			{
				u16 pc = m_model->partitions().count();
				for(u16 pid=0; pid<pc; ++pid)
				{
					u32 blends_count = m_model->LL_PartBlendsCount(pid);
					for(u32 blend_id=0; blend_id<blends_count; ++blend_id)
					{
						CBlend* blend = m_model->LL_PartBlend(pid, blend_id);
						if(!blend) continue;
						MotionID M = blend->motionID;
						if(M==motion)
						{
							ResetBlockedPartID();
							return false;
						}
					}
				}
			}
			//�������� �� ���� ��� ������
			B = m_model->PlayCycle(0, motion, bMixIn, Callback, CallbackParam);
			B = m_model->PlayCycle(1, motion, bMixIn, Callback, CallbackParam);
			B = m_model->PlayCycle(2, motion, bMixIn, Callback, CallbackParam);
		}break;
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
// ��������-������ ������� �� ����� ��������� ������� �������� �� �������� ������� ������������ ��������� � ������ ������������ �������
void player_hud::animator_fx_play(const shared_str& anim_name, u16 place_idx, u16 part_id, u8 anm_idx, float blendAccrue, float blendFalloff, float Speed, float Power)
{
	MotionID motion;
	if(m_attached_items[place_idx] && place_idx>=0)///���� �������� ������������ ������ �� ������ anm_show
	{
		string256				anim_name_r;
		bool is_16x9			= UI().is_widescreen();
		xr_sprintf				(anim_name_r,"%s%s",anim_name.c_str(),((place_idx==1)&&is_16x9)?"_16x9":"");

		player_hud_motion* anm	= m_attached_items[place_idx]->m_hand_motions.find_motion(anim_name_r);

		if(anm)
		{
			motion_descr M	= anm->m_animations[ anm_idx ];
			motion = M.mid;
			Speed *=anm->m_anim_speed;
		}
		else
		{
			Msg("! Animation [%s] not found in %s motion container!", anim_name_r, m_attached_items[place_idx]->m_sect_name.c_str());
		}
	}
	else//����� ����� ������ �� ������� �������� �� ������ abakan_draw ��� fn_2000_reload � ��
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
			case 2://�������� �� ��������� ��������
			{
				auto bones_vec = m_model->partitions().part(part_id).bones;
				for (u32 &it : bones_vec)
					m_model->LL_PlayFX(it, motion, m_def->Accrue()*blendAccrue, m_def->Falloff()*blendFalloff, m_def->Speed()*Speed, m_def->Power()*Power);
			}break;
			case -1://�������� ��� ���� ������� ������ � ���������� ��������
			{
				auto bones_vec = m_model->partitions().part(m_def->bone_or_part).bones;
				for (u32 &it : bones_vec)
					m_model->LL_PlayFX(it, motion, m_def->Accrue()*blendAccrue, m_def->Falloff()*blendFalloff, m_def->Speed()*Speed, m_def->Power()*Power);
			}break;
			default://�������� �� ���� ��� ������
			{
				for (auto &[first,second] : *m_model->dcast_PKinematics()->LL_Bones())
					m_model->LL_PlayFX(second, motion, m_def->Accrue()*blendAccrue, m_def->Falloff()*blendFalloff, m_def->Speed()*Speed, m_def->Power()*Power);
			}break;
		}
	}
}

player_hud::default_hud_coords_params player_hud::GetDefaultHudCoords(shared_str hud_sect)
{
	if (_last_default_hud_params.hud_sect != hud_sect || UI().is_widescreen() != _last_default_hud_params.is16x9)
	{
		Fvector3 zerovec = { 0,0,0 };
		_last_default_hud_params.hud_sect = hud_sect;
		_last_default_hud_params.is16x9 = UI().is_widescreen();

		if (_last_default_hud_params.is16x9)
		{
			_last_default_hud_params.hands_position = READ_IF_EXISTS(pSettings, r_fvector3, hud_sect, "hands_position_16x9", zerovec);
			_last_default_hud_params.hands_orientation = READ_IF_EXISTS(pSettings, r_fvector3, hud_sect, "hands_orientation_16x9", zerovec);
		}
		else
		{
			_last_default_hud_params.hands_position = READ_IF_EXISTS(pSettings, r_fvector3, hud_sect, "hands_position", zerovec);
			_last_default_hud_params.hands_orientation = READ_IF_EXISTS(pSettings, r_fvector3, hud_sect, "hands_orientation", zerovec);
		}
	}

	return _last_default_hud_params;
}

float player_hud::GetCachedCfgParamFloatDef(cached_cfg_param_float& cached, const shared_str section, const shared_str key, float def)
{
	if ((cached.last_section.size() == section.size()) && (cached.last_section == section))
	{
		if (cached.is_default)
			return def;
		else
			return cached.value;
	}
	else
	{
		cached.value = READ_IF_EXISTS(pSettings, r_float, section, key.c_str(), def);

		if (pSettings->line_exist(section, key.c_str()))
			cached.is_default = false;
		else
			cached.is_default = true;

		cached.last_section = section;
		return cached.value;
	}
}

void player_hud::AddOffsets(const xr_string base, shared_str section, Fvector3& pos, Fvector3& rot, float koef)
{
	Fvector3 tmp = {0,0,0};
	Fvector3 zerovec = {0,0,0};

	if (UI().is_widescreen())
	{
		tmp = READ_IF_EXISTS(pSettings, r_fvector3, section, (base + "_pos_16x9").c_str(), zerovec);
		tmp.mul(koef);
		pos.add(tmp);

		tmp = READ_IF_EXISTS(pSettings, r_fvector3, section, (base + "_rot_16x9").c_str(), zerovec);
		tmp.mul(koef);
		rot.add(tmp);
	}
	else
	{
		tmp = READ_IF_EXISTS(pSettings, r_fvector3, section, (base + "_pos").c_str(), zerovec);
		tmp.mul(koef);
		pos.add(tmp);

		tmp = READ_IF_EXISTS(pSettings, r_fvector3, section, (base + "_rot").c_str(), zerovec);
		tmp.mul(koef);
		rot.add(tmp);
	}
}

void player_hud::AddSuicideOffset(shared_str section, Fvector3& pos, Fvector3& rot)
{
	if (READ_IF_EXISTS(pSettings,  r_bool, section, "prohibit_suicide", false))
		return;

	if (READ_IF_EXISTS(pSettings, r_bool, section, "no_other_hud_moving_while_suicide", false))
	{
		rot.set(0, 0, 0);
		pos.set(0, 0, 0);
	}

	AddOffsets("hud_move_suicide_offset", section, pos, rot);

}

void player_hud::GetCurrentTargetOffset_aim(shared_str section, Fvector3& pos, Fvector3& rot, float& factor)
{
	pos.set(0, 0, 0);
	rot.set(0, 0, 0);
	factor = 1.0f;

	float koef = 1.0f;

	if (Actor()->GetMovementState(eReal) & mcCrouch && Actor()->GetMovementState(eReal) & mcAccel)
		koef = READ_IF_EXISTS(pSettings, r_float, section, "hud_aim_move_slow_crouch_factor", 1.0f);
	else if (Actor()->GetMovementState(eReal) & mcCrouch)
		koef = READ_IF_EXISTS(pSettings, r_float, section, "hud_aim_move_crouch_factor", 1.0f);
	else if (Actor()->GetMovementState(eReal) & mcAccel)
		koef = READ_IF_EXISTS(pSettings, r_float, section, "hud_aim_move_slow_factor", 1.0f);

	if (tocrouch_time_remains > 0)
		AddOffsets("hud_aim_move_to_crouch_offset", section, pos, rot, koef);

	if (fromcrouch_time_remains > 0)
		AddOffsets("hud_aim_move_from_crouch_offset", section, pos, rot, koef);

	if (toslowcrouch_time_remains > 0)
		AddOffsets("hud_aim_move_to_slow_crouch_offset", section, pos, rot, koef);

	if (fromslowcrouch_time_remains > 0)
		AddOffsets("hud_aim_move_from_slow_crouch_offset", section, pos, rot, koef);

	if (torlookout_time_remains > 0)
		AddOffsets("hud_aim_move_to_rlookout_offset", section, pos, rot, koef);

	if (fromrlookout_time_remains > 0)
		AddOffsets("hud_aim_move_from_rlookout_offset", section, pos, rot, koef);

	if (tollookout_time_remains > 0)
		AddOffsets("hud_aim_move_to_llookout_offset", section, pos, rot, koef);

	if (fromllookout_time_remains > 0)
		AddOffsets("hud_aim_move_from_llookout_offset", section, pos, rot, koef);
}

void player_hud::GetCurrentTargetOffset(shared_str section, Fvector3& pos, Fvector3& rot, float& factor)
{
	factor = GetCachedCfgParamFloatDef(cached_hud_move_stabilize_factor, section, "hud_move_stabilize_factor", 2.0f);

	pos.set(0, 0, 0);
	rot.set(0, 0, 0);

	float koef = 1.0f;

	if (Actor()->GetMovementState(eReal) & mcCrouch && Actor()->GetMovementState(eReal) & mcAccel)
		koef = GetCachedCfgParamFloatDef(cached_hud_move_slow_crouch_factor, section, "hud_move_slow_crouch_factor", 1.0f);
	else if (Actor()->GetMovementState(eReal) & mcCrouch)
		koef = GetCachedCfgParamFloatDef(cached_hud_move_crouch_factor, section, "hud_move_crouch_factor", 1.0f);
	else if (Actor()->GetMovementState(eReal) & mcAccel)
		koef = GetCachedCfgParamFloatDef(cached_hud_move_slow_factor, section, "hud_move_slow_factor", 1.0f);

	if (tocrouch_time_remains > 0)
	{
		AddOffsets("hud_move_to_crouch_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromcrouch_time_remains > 0)
	{
		AddOffsets("hud_move_from_crouch_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (toslowcrouch_time_remains > 0)
	{
		AddOffsets("hud_move_to_slow_crouch_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromslowcrouch_time_remains > 0)
	{
		AddOffsets("hud_move_from_slow_crouch_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (torlookout_time_remains > 0)
	{
		AddOffsets("hud_move_to_rlookout_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromrlookout_time_remains > 0)
	{
		AddOffsets("hud_move_from_rlookout_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (tollookout_time_remains > 0)
	{
		AddOffsets("hud_move_to_llookout_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if (fromllookout_time_remains > 0)
	{
		AddOffsets("hud_move_from_llookout_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal) & mcRLookout) && !(Actor()->GetMovementState(eReal) & mcLLookout))
	{
		AddOffsets("hud_move_rlookout_offset", section, pos, rot, koef);
		factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_rlookout_offset_speed_factor", 1.0f);
	}

	if ((Actor()->GetMovementState(eReal) & mcLLookout) && !(Actor()->GetMovementState(eReal) & mcRLookout))
	{
		AddOffsets("hud_move_llookout_offset", section, pos, rot, koef);
		factor = READ_IF_EXISTS(pSettings, r_float, section, "hud_move_llookout_offset_speed_factor", 1.0f);
	}

	if ((Actor()->GetMovementState(eReal)) & mcLStrafe && !(Actor()->GetMovementState(eReal) & mcRStrafe))
	{
		AddOffsets("hud_move_left_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcRStrafe && !(Actor()->GetMovementState(eReal) & mcLStrafe))
	{
		AddOffsets("hud_move_right_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcFwd && !(Actor()->GetMovementState(eReal) & mcBack))
	{
		AddOffsets("hud_move_forward_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcBack && !(Actor()->GetMovementState(eReal) & mcFwd))
	{
		AddOffsets("hud_move_back_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcJump && !(Actor()->GetMovementState(eReal) & mcFall) && !(Actor()->GetMovementState(eReal) & mcLanding) && !(Actor()->GetMovementState(eReal) & mcLanding2))
	{
		AddOffsets("hud_move_jump_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcFall && !(Actor()->GetMovementState(eReal) & mcJump) && !(Actor()->GetMovementState(eReal) & mcLanding) && !(Actor()->GetMovementState(eReal) & mcLanding2))
	{
		AddOffsets("hud_move_fall_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcLanding && !(Actor()->GetMovementState(eReal) & mcJump) && !(Actor()->GetMovementState(eReal) & mcFall) && !(Actor()->GetMovementState(eReal) & mcLanding2))
	{
		AddOffsets("hud_move_landing_offset", section, pos, rot, koef);
		factor = 1.0f;
	}

	if ((Actor()->GetMovementState(eReal)) & mcLanding2 && !(Actor()->GetMovementState(eReal) & mcJump) && !(Actor()->GetMovementState(eReal) & mcFall) && !(Actor()->GetMovementState(eReal) & mcLanding))
	{
		AddOffsets("hud_move_landing2_offset", section, pos, rot, koef);
		factor = 1.0f;
	}
}

void player_hud::UpdateWeaponOffset(u32 delta)
{
	CHudItemObject* itm = smart_cast<CHudItemObject*>(Actor()->inventory().ActiveItem());
	CCustomDetector* det = nullptr;

	if (itm == nullptr)
		itm = Actor()->GetDetector();
	else
		det = Actor()->GetDetector();

	if (itm == nullptr)
		return;

	attachable_hud_item* HID = itm->HudItemData();

	if (HID == nullptr)
		return;

	time_accumulator += delta;

	shared_str section = itm->HudSection();

	if ((Actor()->GetMovementState(eWishful) & mcCrouch) && !(Actor()->GetMovementState(eReal) & mcCrouch))
	{
		// Начали присяд
		tocrouch_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_crouch_time", 0.f) * 1000.f);
		fromcrouch_time_remains = 0;
	}
	else if (!(Actor()->GetMovementState(eWishful) & mcCrouch) && (Actor()->GetMovementState(eReal) & mcCrouch))
	{
		// Закончили присяд
		fromcrouch_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_crouch_time", 0.f) * 1000.f);
		tocrouch_time_remains = 0;
	}

	if ((Actor()->GetMovementState(eWishful) & mcCrouch) && (Actor()->GetMovementState(eWishful) & mcAccel) && !(Actor()->GetMovementState(eReal) & mcAccel))
	{
		// Начали присяд
		toslowcrouch_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_slow_crouch_time", 0.f) * 1000.f);
		fromslowcrouch_time_remains = 0;
	}
	else if ((Actor()->GetMovementState(eWishful) & mcCrouch) && !(Actor()->GetMovementState(eWishful) & mcAccel) && (Actor()->GetMovementState(eReal) & mcAccel))
	{
		// Закончили присяд
		fromslowcrouch_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_slow_crouch_time", 0.f) * 1000.f);
		toslowcrouch_time_remains = 0;
	}

	if (!(Actor()->GetMovementState(eReal) & mcRLookout) && !(Actor()->GetMovementState(eReal) & mcLLookout))
	{
		// Если одновременно выглядываем влево и вправо - что-то тут не так...
		if ((Actor()->GetMovementState(eWishful) & mcRLookout) && !(Actor()->GetMovementState(eReal) & mcRLookout))
		{
			// Начали выглядывать вправо
			torlookout_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_rlookout_time", 0.f) * 1000.f);
			fromrlookout_time_remains = 0;
		}
		else if (!(Actor()->GetMovementState(eWishful) & mcRLookout) && (Actor()->GetMovementState(eReal) & mcRLookout))
		{
			// Закончили выглядывать вправо
			fromrlookout_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_rlookout_time", 0.f) * 1000.f);
			torlookout_time_remains = 0;
		}

		if ((Actor()->GetMovementState(eWishful) & mcLLookout) && !(Actor()->GetMovementState(eReal) & mcLLookout))
		{
			// Начали выглядывать влево
			tollookout_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "to_llookout_time", 0.f) * 1000.f);
			fromllookout_time_remains = 0;
		}
		else if (!(Actor()->GetMovementState(eWishful) & mcLLookout) && (Actor()->GetMovementState(eReal) & mcLLookout))
		{
			// Закончили выглядывать влево
			fromllookout_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, section, "from_llookout_time", 0.f) * 1000.f);
			tollookout_time_remains = 0;
		}
	}

	// Прочитаем конфиговые умолчания
	default_hud_coords_params def_hud_params = GetDefaultHudCoords(section);
	Fvector3 pos = def_hud_params.hands_position;
	Fvector3 rot = def_hud_params.hands_orientation;

	Fvector3 targetpos = {0,0,0};
	Fvector3 targetrot = {0,0,0};

	float factor = 1.0f;

	// Вычислим целевое смещение от равновесия
	if (itm->GetState() == CHUDState::eHiding || (det != nullptr && det->GetState() == CHUDState::eHiding))
		factor = GetCachedCfgParamFloatDef(cached_hud_move_weaponhide_factor, section, "hud_move_weaponhide_factor", 1.0f);
	else if ((itm->WpnCanShoot() || smart_cast<CWeaponBinoculars*>(itm) != nullptr) && (static_cast<CWeapon*>(itm)->IsZoomed() || static_cast<CWeapon*>(itm)->IsAimStarted))
	{
		GetCurrentTargetOffset_aim(section, targetpos, targetrot, factor);
		factor = GetCachedCfgParamFloatDef(cached_hud_move_unzoom_factor, section, "hud_move_unzoom_factor", 1.0f);
	}
	else
	{
		GetCurrentTargetOffset(section, targetpos, targetrot, factor);
		if (Actor()->IsActorSuicideNow() && Actor()->CheckActorVisibilityForController())
			AddSuicideOffset(section, targetpos, targetrot);
		else if (HID != nullptr &&
			!(Actor()->GetMovementState(eReal) & mcFwd) &&
			!(Actor()->GetMovementState(eReal) & mcBack) &&
			!(Actor()->GetMovementState(eReal) & mcLStrafe) &&
			!(Actor()->GetMovementState(eReal) & mcRStrafe) &&
			!(Actor()->GetMovementState(eReal) & mcSprint) &&
			!(Actor()->GetMovementState(eReal) & mcJump) &&
			!(Actor()->GetMovementState(eReal) & mcFall) &&
			!(Actor()->GetMovementState(eReal) & mcLanding) &&
			!(Actor()->GetMovementState(eReal) & mcLanding2))
		{
			// TODO: Смещение в идле
		}
	}

	// Находим целевую позицию
	targetpos.add(pos);
	targetrot.add(rot);

	float speed_pos = 0.f;
	float speed_rot = 0.f;

	// Смотрим на скорость сдвига
	if (!Actor()->IsActorSuicideNow() && !itm->IsSuicideAnimPlaying())
	{
		speed_rot = GetCachedCfgParamFloatDef(cached_hud_move_speed_rot, section, "hud_move_speed_rot", 0.4f) * factor / 100.f;
		speed_pos = GetCachedCfgParamFloatDef(cached_hud_move_speed_pos, section, "hud_move_speed_pos", 0.1f) * factor / 100.f;
	}
	else
	{
		speed_rot = GetCachedCfgParamFloatDef(cached_suicide_speed_rot, section, "suicide_speed_rot", 0.002f);
		speed_pos = GetCachedCfgParamFloatDef(cached_suicide_speed_pos, section, "suicide_speed_pos", 0.2f);
	}

	CHudItem::jitter_params jitter;

	// 120 коррекций в секунду к вычисленному положению
	while (time_accumulator > 8)
	{
		// Смотрим, куда будем двигать худ
		pos = targetpos;
		rot = targetrot;

		Fvector3 cur_pos = HID->hands_attach_pos();
		Fvector3 cur_rot = HID->hands_attach_rot();

		pos.sub(cur_pos);
		rot.sub(cur_rot);

		// Пересчитываем вектор сдвига с учетом скорости
		if (Actor()->IsActorSuicideNow())
		{
			// Идем линейно
			if (pos.magnitude() > speed_pos)
				pos.set_length(speed_pos);

			if (rot.magnitude() > speed_rot)
				rot.set_length(speed_rot);
		}
		else
		{
			if (pos.magnitude() > 0.0001f)
				pos.mul(speed_pos);

			if (rot.magnitude() > 0.0001f)
				rot.mul(speed_rot);
		}

		// Добавляем пересчитанный сдвиг к текущему положению и записываем его
		cur_pos.add(pos);
		cur_rot.add(rot);

		if (Actor()->IsHandJitter(itm))
		{
			jitter = itm->GetCurJitterParams(section);

			pos.x = ::Random.randF(0.f, 1000.f) - 500.f;
			pos.y = ::Random.randF(0.f, 500.f) - 250.f;
			pos.z = ::Random.randF(0.f, 1000.f) - 500.f;
			pos.set_length(jitter.pos_amplitude * Actor()->GetHandJitterScale(itm));
			cur_pos.add(pos);

			rot.x = ::Random.randF(0.f, 1000.f) - 500.f;
			rot.y = ::Random.randF(0.f, 1000.f) - 500.f;
			rot.z = ::Random.randF(0.f, 1000.f) - 500.f;
			rot.set_length(jitter.rot_amplitude * Actor()->GetHandJitterScale(itm));
			cur_rot.add(rot);
		}

		HID->set_hands_offset_pos(cur_pos);
		HID->set_hands_offset_rot(cur_rot);

		if (det != nullptr)
		{
			attached_item(1)->set_hands_offset_pos(cur_pos);
			attached_item(1)->set_hands_offset_rot(cur_rot);
		}

		time_accumulator -= 8;
	}

	if (Actor()->IsActorSuicideNow() && Actor()->CheckActorVisibilityForController() && !(READ_IF_EXISTS(pSettings, r_bool, section, "prohibit_suicide", false) || READ_IF_EXISTS(pSettings, r_bool, section, "suicide_by_animation", false)))
	{
		jitter = itm->GetCurJitterParams(section);
		pos = HID->hands_attach_pos();
		rot = HID->hands_attach_rot();

		pos.sub(targetpos);
		rot.sub(targetrot);

		if (pos.magnitude() < jitter.pos_amplitude * 2 && rot.magnitude() < jitter.rot_amplitude * 2)
			Actor()->DoSuicideShot();
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

void player_hud::ResetItmHudOffset(CHudItem* itm)
{
	attachable_hud_item* hid = itm->HudItemData();
	if (hid == nullptr)
		return;

	default_hud_coords_params def_hud_params = GetDefaultHudCoords(itm->HudSection());

	hid->set_hands_offset_pos(def_hud_params.hands_position);
	hid->set_hands_offset_rot(def_hud_params.hands_orientation);
}