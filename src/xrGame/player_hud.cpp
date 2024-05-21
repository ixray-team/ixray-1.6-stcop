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

player_hud* g_player_hud = nullptr;
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

void attachable_hud_item::set_bone_visible(const shared_str& bone_name, BOOL bVisibility, BOOL bSilent)
{
	u16  bone_id;
	BOOL bVisibleNow;
	bone_id			= m_model->LL_BoneID			(bone_name);
	if(bone_id==BI_NONE)
	{
		if(bSilent)	return;
		R_ASSERT2	(0, make_string<const char*>("model [%s] has no bone [%s]",pSettings->r_string(m_sect_name, "item_visual"), bone_name.c_str()));
	}
	bVisibleNow		= m_model->LL_GetBoneVisible	(bone_id);
	if(bVisibleNow!=bVisibility)
		m_model->LL_SetBoneVisible	(bone_id,bVisibility, TRUE);
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

	//--> Смещение в стрейфе
	xr_strconcat(val_name, "strafe_hud_offset_pos", _prefix);
	m_strafe_offset[0][0] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.015f, 0.f, 0.f));
	xr_strconcat(val_name, "strafe_hud_offset_rot", _prefix);
	m_strafe_offset[1][0] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.f, 0.f, 4.5f));

	//--> Поворот в стрейфе
	xr_strconcat(val_name, "strafe_aim_hud_offset_pos", _prefix);
	m_strafe_offset[0][1] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.005f, 0.f, 0.f));
	xr_strconcat(val_name, "strafe_aim_hud_offset_rot", _prefix);
	m_strafe_offset[1][1] = READ_IF_EXISTS(pSettings, r_fvector3, sect_name, val_name, Fvector().set(0.f, 0.f, 2.5f));

	//--> Параметры стрейфа
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
		IKinematicsAnimated* ka			= m_model->dcast_PKinematicsAnimated();

		shared_str item_anm_name;
		if(anm->m_base_name!=anm->m_additional_name)
			item_anm_name = anm->m_additional_name;
		else
			item_anm_name = M.name;

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


player_hud::player_hud()
{
	m_model					= nullptr;
	m_attached_items[0]		= nullptr;
	m_attached_items[1]		= nullptr;
	m_transform.identity	();
	m_transformL.identity	();
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
	if(player_hud_sect ==m_sect_name)	return;
	bool b_reload = (m_model!=nullptr);
	if(m_model)
	{
		IRenderVisual* v			= m_model->dcast_RenderVisual();
		::Render->model_Delete		(v);
	}

	m_sect_name					= player_hud_sect;
	const shared_str& model_name= pSettings->r_string(player_hud_sect, "visual");
	m_model						= smart_cast<IKinematicsAnimated*>(::Render->model_Create(model_name.c_str()));

	u16 l_arm = m_model->dcast_PKinematics()->LL_BoneID("l_clavicle");
	m_model->dcast_PKinematics()->LL_GetBoneInstance(l_arm).set_callback(bctCustom, LeftArmCallback, this);

	CInifile::Sect& _sect		= pSettings->r_section(player_hud_sect);
	CInifile::SectCIt _b		= _sect.Data.begin();
	CInifile::SectCIt _e		= _sect.Data.end();
	for(;_b!=_e;++_b)
	{
		if(strstr(_b->first.c_str(), "ancor_")==_b->first.c_str())
		{
			const shared_str& _bone	= _b->second;
			m_ancors.push_back		(m_model->dcast_PKinematics()->LL_BoneID(_bone));
		}
	}
	
//	Msg("hands visual changed to[%s] [%s] [%s]", model_name.c_str(), b_reload?"R":"", m_attached_items[0]?"Y":"");

	if(!b_reload)
	{
		m_model->PlayCycle("hand_idle_doun");
	}else
	{
		if(m_attached_items[1])
			m_attached_items[1]->m_parent_hud_item->on_a_hud_attach();

		if(m_attached_items[0])
			m_attached_items[0]->m_parent_hud_item->on_a_hud_attach();
	}
	m_model->dcast_PKinematics()->CalculateBones_Invalidate	();
	m_model->dcast_PKinematics()->CalculateBones(TRUE);
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
	if(!m_attached_items[0] && !m_attached_items[1])	return;

	bool b_r0 = (m_attached_items[0] && m_attached_items[0]->need_renderable());
	bool b_r1 = (m_attached_items[1] && m_attached_items[1]->need_renderable());

	if(!b_r0 && !b_r1)									return;

	::Render->set_Transform		(&m_transform);
	::Render->add_Visual		(m_model->dcast_RenderVisual(), true);
	
	if(m_attached_items[0])
		m_attached_items[0]->render();
	
	if(m_attached_items[1])
		m_attached_items[1]->render();
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

const Fvector& player_hud::attach_rot() const {
	if (m_attached_items[0]) {
		return m_attached_items[0]->hands_attach_rot();
	} else {
		if (m_attached_items[1]) {
			return m_attached_items[1]->hands_attach_rot();
		} else {
			static Fvector default_attach_rot {};
			default_attach_rot.set(0, 0, 0);
			return default_attach_rot;
		}
	}
}

const Fvector& player_hud::attach_pos() const {
	if (m_attached_items[0]) {
		return m_attached_items[0]->hands_attach_pos();
	} else {
		if (m_attached_items[1]) {
			return m_attached_items[1]->hands_attach_pos();
		} else {
			static Fvector default_attach_pos {};
			default_attach_pos.set(0, 0, 0);
			return default_attach_pos;
		}
	}
}

void player_hud::LeftArmCallback(CBoneInstance* B) {
	player_hud* PlayerHud = static_cast<player_hud*>(B->callback_param());

	Fmatrix inv_main_trans;
	inv_main_trans.invert(PlayerHud->m_transform_fake);

	B->mTransform.mulA_44(PlayerHud->m_transformL_fake);
	B->mTransform.mulA_44(inv_main_trans);
}

void player_hud::update(const Fmatrix& cam_trans)
{
	Fmatrix	trans					= cam_trans;
	update_inertion					(trans);
	update_additional				(trans);

	Fvector ypr						= attach_rot();
	ypr.mul							(PI/180.f);
	m_attach_offset.setHPB			(ypr.x,ypr.y,ypr.z);
	m_attach_offset.translate_over	(attach_pos());
	m_transform.mul					(trans, m_attach_offset);

	// insert inertion here
	if (m_attached_items[1]) {
		ypr = m_attached_items[1]->hands_attach_rot();
		ypr.mul(PI / 180.f);
		m_attach_offset.setHPB(ypr.x, ypr.y, ypr.z);
		m_attach_offset.translate_over(m_attached_items[1]->hands_attach_pos());
		m_transformL.mul(trans, m_attach_offset);
	}

	if (!m_attached_items[1]) {
		m_transformL.set(m_transform);
	}

	m_transform_fake = m_transform;
	m_transformL_fake = m_transformL;
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

	u16 part_id							= u16(-1);
	if(attached_item(0) && attached_item(1))
		part_id = m_model->partitions().part_id((part==0)?"right_hand":"left_hand");

	u16 pc					= m_model->partitions().count();
	for(u16 pid=0; pid<pc; ++pid)
	{
		if(pid==0 || pid==part_id || part_id==u16(-1))
		{
			CBlend* B	= m_model->PlayCycle(pid, M, bMixIn);
			R_ASSERT	(B);
			B->speed	*= speed;
		}
	}
	m_model->dcast_PKinematics()->CalculateBones_Invalidate	();

	return				motion_length(M, md, speed);
}

void player_hud::update_additional	(Fmatrix& trans)
{
	CEntity* pEntity = smart_cast<CEntity*>(Level().CurrentEntity());
	if (pEntity)
	{
		CActor* pActor = smart_cast<CActor*>(pEntity);
		if(pActor)
		{
			if(pActor->HUDview())
			{
				if(m_attached_items[0])
					m_attached_items[0]->update_hud_additional(trans);

				if(m_attached_items[1])
					m_attached_items[1]->update_hud_additional(trans);
			}
			else
			{
				u16 I = pActor->inventory().FirstSlot();
				u16 E = pActor->inventory().LastSlot();

				for (; I <= E; ++I)
				{
					PIItem item_in_slot = pActor->inventory().ItemFromSlot(I);
					CHudItem* itm =	smart_cast<CHudItem*>(item_in_slot);
					if(itm)
						itm->UpdateHudAdditonal(trans);
				}
			}
		}
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

void player_hud::detach_item_idx(u16 idx)
{
	if( nullptr==attached_item(idx) )					return;

	m_attached_items[idx]->m_parent_hud_item->on_b_hud_detach();

	m_attached_items[idx]->m_parent_hud_item		= nullptr;
	m_attached_items[idx]							= nullptr;

	if(idx==1 && attached_item(0))
	{
		u16 part_idR			= m_model->partitions().part_id("right_hand");
		u32 bc					= m_model->LL_PartBlendsCount(part_idR);
		for(u32 bidx=0; bidx<bc; ++bidx)
		{
			CBlend* BR			= m_model->LL_PartBlend(part_idR, bidx);
			if(!BR)
				continue;

			MotionID M			= BR->motionID;

			u16 pc					= m_model->partitions().count();
			for(u16 pid=0; pid<pc; ++pid)
			{
				if(pid!=part_idR)
				{
					CBlend* B			= m_model->PlayCycle(pid, M, TRUE);//this can destroy BR calling UpdateTracks !
					if( BR->blend_state() != CBlend::eFREE_SLOT )
					{
						u16 bop				= B->bone_or_part;
						*B					= *BR;
						B->bone_or_part		= bop;
					}
				}
			}
		}
	}else
	if(idx==0 && attached_item(1))
	{
		OnMovementChanged(mcAnyMove);
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
