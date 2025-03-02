#include "stdafx.h"
#include "controller_psy_hit.h"
#include "../basemonster/base_monster.h"
#include "controller.h"
#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../../../Level.h"
#include "../../../Actor.h"
#include "../../../ActorEffector.h"
#include "../../../../xrEngine/CameraBase.h"
#include "../../../CharacterPhysicsSupport.h"
#include "../../../level_debug.h"
#include "../../../ActorCondition.h"
#include "../../../HudManager.h"
#include "Inventory.h"
#include "UIGameCustom.h"
#include "game_cl_single.h"
#include "Weapon.h"
#include "WeaponRPG7.h"
#include "WeaponRG6.h"
#include "WeaponKnife.h"
#include "../../xrScripts/script_engine.h"

void CControllerPsyHit::load(LPCSTR section)
{
	m_min_tube_dist = pSettings->r_float(section,"tube_condition_min_distance");
}

void CControllerPsyHit::reinit()
{
	inherited::reinit();

	IKinematicsAnimated	*skel = smart_cast<IKinematicsAnimated *>(m_object->Visual());
	m_stage[0] = skel->ID_Cycle_Safe("psy_attack_0"); VERIFY(m_stage[0]);
	m_stage[1] = skel->ID_Cycle_Safe("psy_attack_1"); VERIFY(m_stage[1]);
	m_stage[2] = skel->ID_Cycle_Safe("psy_attack_2"); VERIFY(m_stage[2]);
	m_stage[3] = skel->ID_Cycle_Safe("psy_attack_3"); VERIFY(m_stage[3]);
	m_current_index		= 0;

	m_time_last_tube	= 0;
	m_sound_state		= eNone;
}


bool CControllerPsyHit::tube_ready () const
{
	u32 tube_condition_min_delay	=	5000;
	if ( CController* controller = smart_cast<CController*>(m_object) )
		tube_condition_min_delay	=	controller->m_tube_condition_min_delay;

	return m_time_last_tube + tube_condition_min_delay < time();
}

bool CControllerPsyHit::IsNeedPsiHitOverride()
{
	CActor* pActor = smart_cast<CActor*>(Level().CurrentControlEntity());
	return pActor != nullptr && pActor->inventory().ActiveItem() != nullptr;
}

bool CControllerPsyHit::check_start_conditions()
{
	CActor* pActor = Actor();
	if (is_active())
		return false;

	if (m_man->is_captured_pure())
		return false;

	if (IsGameTypeSingle())
	{
		if (pActor->Cameras().GetCamEffector(eCEControllerPsyHit))
			return						false;

		if (!see_enemy(pActor))
			return						false;

		if (!tube_ready())
			return						false;

		if (m_object->Position().distance_to(pActor->Position()) < m_min_tube_dist)
		{
			if (!IsNeedPsiHitOverride())
				return false;
		}
	}
	else
	{
		CActor* pA = const_cast<CActor*>(smart_cast<const CActor*>(m_object->EnemyMan.get_enemy()));
		if (pA)
		{
			m_curent_actor_id = u16(-1);

			if (pA->Cameras().GetCamEffector(eCEControllerPsyHit))
				return						false;

			if (!see_enemy(pA))
				return						false;

			if (!tube_ready())
				return						false;

			if (m_object->Position().distance_to(pActor->Position()) < m_min_tube_dist)
			{
				if (!IsNeedPsiHitOverride())
					return false;
			}

			m_curent_actor_id = pA->ID();
		}
		else
			return false;
	}

	return true;
}

void CControllerPsyHit::OnPsyHitActivate(CController* monster_controller)
{
	if (Actor()->_controlled_time_remains == 0)
	{
		Actor()->UpdatePsiBlockFailedState(monster_controller);
		Actor()->ChangeInputRotateAngle();
	}

	Actor()->_controller_preparing_starttime = Device.dwTimeGlobal;

	/*luabind::functor<void> funct;
	if (ai().script_engine().functor("gunsl_controller.on_psi_attack_prepare", funct))
		funct("", monster_controller->ID());*/

	if ((!Actor()->IsPsiBlocked() || Actor()->IsPsiBlockFailed()) && Actor()->_controlled_time_remains > 0)
		Actor()->_controlled_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_prepare_time", 3.f) * 1000.f);
}

void CControllerPsyHit::activate()
{
	CActor* pActor = Actor();
	if (!IsGameTypeSingle())
	{
		pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));
		if (!pActor)
		{
			return;
		}
	}

	m_man->capture_pure				(this);
	m_man->subscribe				(this, ControlCom::eventAnimationEnd);

	m_man->path_stop				(this);
	m_man->move_stop				(this);

	//////////////////////////////////////////////////////////////////////////
	// set direction
	SControlDirectionData			*ctrl_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir); 
	VERIFY							(ctrl_dir);
	if (ctrl_dir == nullptr)
	{
		return;
	}

	ctrl_dir->heading.target_speed	= 3.f;
	ctrl_dir->heading.target_angle	= m_man->direction().angle_to_target(pActor->Position());

	//////////////////////////////////////////////////////////////////////////
	m_current_index					= 0;
	play_anim						();

	m_blocked						= false;

	if (!IsGameTypeSingle()) {
		NET_Packet	tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
		tmp_packet.w_u16(pActor->ID());
		tmp_packet.w_u8(0);
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
	}

	bool isGuns = EngineExternal().isModificationGunslinger();

	if (isGuns)
		OnPsyHitActivate(static_cast<CController*>(m_object));

	set_sound_state					(ePrepare);
}

void CControllerPsyHit::deactivate()
{
	CActor* pActor = Actor();
	m_man->release_pure				(this);
	m_man->unsubscribe				(this, ControlCom::eventAnimationEnd);

	if (m_blocked) {
		NET_Packet			P;

		if (IsGameTypeSingle()) {
			pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
			P.w_u16(INV_STATE_BLOCK_ALL);
			P.w_u8(u8(false));
			pActor->u_EventSend(P);
		}
		else
		{
			CActor* pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));
			if (pActor)
			{
				pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
				P.w_u16(INV_STATE_BLOCK_ALL);
				P.w_u8(u8(false));
				pActor->u_EventSend(P);
			}
		}
	}


	set_sound_state(eNone);
}

void CControllerPsyHit::on_event(ControlCom::EEventType type, ControlCom::IEventData *data)
{
	if (type == ControlCom::eventAnimationEnd) {
		if (m_current_index < 3) {
			m_current_index++;
			play_anim			();
			
			switch (m_current_index) {
				case 1: death_glide_start();	break;
				case 2: hit();					break;
				case 3: death_glide_end();		break;
			}
		} else {
			m_man->deactivate	(this);
			return;
		}
	}
}

void CControllerPsyHit::play_anim()
{
	SControlAnimationData		*ctrl_anim = (SControlAnimationData*)m_man->data(this, ControlCom::eControlAnimation); 
	VERIFY						(ctrl_anim);
	if (ctrl_anim == nullptr)
	{
		return;
	}
	ctrl_anim->global.set_motion ( m_stage[m_current_index] );
	ctrl_anim->global.actual	= false;
}

namespace detail
{

bool check_actor_visibility (const Fvector trace_from, 
							 const Fvector trace_to,
							 CObject* object)
{
	CActor* pActor = Actor();
	const float dist = trace_from.distance_to(trace_to);
	Fvector trace_dir;
	trace_dir.sub(trace_to, trace_from);


	collide::rq_result l_rq;
	l_rq.O = nullptr;
	Level().ObjectSpace.RayPick(trace_from,
								trace_dir, 
								dist, 
								collide::rqtBoth, 
								l_rq, 
								object);

	return l_rq.O == pActor || (l_rq.range >= dist - 0.1f);
}

} // namespace detail

extern CActor* g_actor;


/*
bool CControllerPsyHit::see_enemy ()
{
	CActor* pActor = smart_cast<CActor*>(Level().CurrentControlEntity());
	return	m_object->EnemyMan.see_enemy_now(pActor);
}

*/

struct controller_feel_params
{
	float min_dist = 0.0f;
	float max_dist = 0.0f;
};

static controller_feel_params GetControllerFeelParams()
{
	controller_feel_params result;

	result.min_dist = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_min_feel_dist", 10.0f);
	result.max_dist = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_max_feel_dist", 30.0f);

	return result;
}

bool CControllerPsyHit::PsiEffects(CController* monster_controller)
{
	if (Actor() == nullptr)
		return true;

	bool psi_blocked = Actor()->IsPsiBlocked() && !Actor()->IsPsiBlockFailed();
	bool not_seen = !Actor()->IsControllerSeeActor(monster_controller);
	float dist = Actor()->DistToSelectedContr(monster_controller);
	controller_feel_params contr_feel = GetControllerFeelParams();

	bool dist_forcer = false;
	if (dist < contr_feel.min_dist)
		dist_forcer = true;
	else if (dist > contr_feel.max_dist)
		dist_forcer = false;
	else
		dist_forcer = ((dist - contr_feel.min_dist) / (contr_feel.max_dist - contr_feel.min_dist)) > ::Random.randF(0.f, 1.f);

	// Если активен бустер псиблокады, то суицид не делаем
	// Также не делаем, если актор свалил
	if ((psi_blocked || (!dist_forcer && not_seen)) && !Actor()->IsActorSuicideNow() && !Actor()->IsSuicideInreversible())
	{
		Actor()->_planning_suicide = false;
		Actor()->_suicide_now = false;
		Actor()->SetHandsJitterTime(floor(READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_psyblocked_time", 5.0f) * 1000.0f));
		return not_seen;
	}

	Actor()->_controlled_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_time", 3.f) * 1000.f);

	CHudItemObject* wpn = smart_cast<CHudItemObject*>(Actor()->inventory().ActiveItem());

	/*if (IsPDAWindowVisible() || (wpn != nullptr && GetSection(wpn) == GetPDAShowAnimator()))
	{
		_planning_suicide = false;
		_suicide_now = false;
		SetHandsJitterTime(GetControllerTime());
		return false;  // бьем стандартным пси-хитом
	}*/

	if (Actor()->GetDetector() != nullptr || wpn != nullptr && !Actor()->CanUseItemForSuicide(wpn))
	{
		Actor()->_planning_suicide = Actor()->CanUseItemForSuicide(wpn);
		Actor()->_suicide_now = false;
		return wpn == nullptr || (!(wpn->GetState() == CHUDState::eHidden || wpn->GetState() == CHUDState::eHiding));
	}

	if (wpn == nullptr)
	{
		if (Actor()->CanUseItemForSuicide(smart_cast<CHudItemObject*>(Actor()->inventory().ItemFromSlot(1))))
		{
			Actor()->_planning_suicide = true;
			return true;
		}
		else
		{
			Actor()->_planning_suicide = false;
			return false;
		}
	}

	if (smart_cast<CWeaponKnife*>(wpn) != nullptr)
	{
		Actor()->_controlled_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, wpn->HudSection(), "controller_time", Actor()->_controlled_time_remains / 1000.0f) * 1000.f);
		Actor()->_planning_suicide = true;
		// если до сих пор анимация суицида не стартовала - форсируем событие
		if ((!Actor()->_suicide_now || !wpn->IsSuicideAnimPlaying()) && (wpn->GetState() != CWeapon::eFire || wpn->GetState() != CWeapon::eFire2))
			wpn->SwitchState(CWeapon::eFire);

		return true;
	}

	Fvector3& c_pos = m_object->Position();
	Fvector3 c_pos_cp = c_pos;
	Fvector3& a_pos = Actor()->Position();
	c_pos_cp.sub(a_pos);

	if (smart_cast<CWeapon*>(wpn) != nullptr && static_cast<CWeapon*>(wpn)->IsGrenadeMode())
	{
		bool can_switch_gl = READ_IF_EXISTS(pSettings, r_bool, wpn->HudSection(), "controller_can_switch_gl", false);
		bool can_shoot_gl = READ_IF_EXISTS(pSettings, r_bool, wpn->HudSection(), "controller_can_shoot_gl", false);
		if (can_shoot_gl && (static_cast<CWeapon*>(wpn)->GetAmmoInGLCount() > 0) && (READ_IF_EXISTS(pSettings, r_float, wpn->HudSection(), "controller_shoot_gl_min_dist", 10.f) < c_pos_cp.magnitude()))
		{
			// Дистанция до контры большая, можно стрелять из подствола
			// Ничего особенного делать тут не надо (пока?), просто идем дальше по if'ам
		}
		else if (can_switch_gl && (static_cast<CWeapon*>(wpn)->GetAmmoInMagCount() > 0) && !static_cast<CWeapon*>(wpn)->IsMisfire())
		{
			wpn->SwitchState(CWeapon::eSwitch);
			Actor()->_planning_suicide = true;
			Actor()->_suicide_now = false;
			return true;
		}
		else
		{
			Actor()->g_PerformDrop();
			return true;
		}
	}
	else if (smart_cast<CWeaponRG6*>(wpn) != nullptr || smart_cast<CWeaponRPG7*>(wpn) != nullptr)
	{
		if (READ_IF_EXISTS(pSettings, r_float, wpn->HudSection(), "controller_shoot_expl_min_dist", 10.f) > c_pos_cp.magnitude())
		{
			Actor()->g_PerformDrop();
			return true;
		}
	}

	Actor()->_planning_suicide = false;
	Actor()->_suicide_now = false;

	if (READ_IF_EXISTS(pSettings, r_bool, wpn->HudSection(), "suicide_by_animation", false))
	{
		Actor()->_suicide_now = wpn->IsSuicideAnimPlaying() || (Actor()->_lastshot_done_time > 0);
		if (!Actor()->_suicide_now)
		{
			wpn->SwitchState(CHUDState::eSuicide);
			Actor()->_suicide_now = true;
		}

		if (Actor()->_suicide_now)
			Actor()->_controlled_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, wpn->HudSection(), "controller_time", Actor()->_controlled_time_remains / 1000.0f) * 1000.f);

		Actor()->_planning_suicide = true;
	}
	else
	{
		if (wpn->CanStartAction())
		{
			Actor()->_suicide_now = true;
			Actor()->_controlled_time_remains = floor(READ_IF_EXISTS(pSettings, r_float, wpn->HudSection(), "controller_time", Actor()->_controlled_time_remains / 1000.0f) * 1000.f);
		}
		Actor()->_planning_suicide = true;
	}

	if (wpn->GetState() == CWeapon::eFire)
	{
		if (READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_queue_stop_prob", 0.95f) >= ::Random.randF(0.f, 1.f))
			static_cast<CWeapon*>(wpn)->SetWorkingState(false);
	}

	return true;
}

bool CControllerPsyHit::PsiStart(CController* monster_controller)
{
	bool result = PsiEffects(monster_controller);

	if (Actor() && !Actor()->inventory_disabled())
	{
		CurrentGameUI()->HideShownDialogs();
		Actor()->set_inventory_disabled(true);
		Actor()->_inventory_disabled_set = true;
	}

	if (result && Actor())
	{
		//wpn = GetActorActiveItem();
		//if (wpn != nullptr && !game_ini_r_bool_def(GetHUDSection(wpn), "suicide_by_animation", false) && (IsKnife(wpn) || WpnCanShoot(wpn)))
		//{
		//	scream = "sndScream" + std::to_string(rand() % 3 + 1);
		//	CHudItem_Play_Snd(wpn, scream.c_str());
		//}

		bool found = false;
		for (int k = 0; k < Actor()->_active_controllers.size(); ++k)
		{
			if (Actor()->_active_controllers[k] == monster_controller)
			{
				found = true;
				break;
			}
		}

		if (!found)
			Actor()->AddActiveController(monster_controller);

		if (Actor()->_planning_suicide || Actor()->_suicide_now)
		{
			/*luabind::functor<void> funct;
			if (ai().script_engine().functor("gunsl_controller.on_suicide_attack", funct))
				funct("", monster_controller->ID());*/
		}
	}
	else
	{
		/*luabind::functor<void> funct;
		if (ai().script_engine().functor("gunsl_controller.on_std_attack", funct))
			funct("", monster_controller->ID());*/
	}

	/*if (g_SingleGameDifficulty >= egdStalker)
	{
		phantoms_params p = GetControllerPhantomsParams();
		u32 i = rand() % (p.max_cnt - p.min_cnt) + p.min_cnt;
		Fvector3 va = Device.vCameraPosition;

		for (u32 j = 1; j <= i; ++j)
		{
			Fvector3 v;
			v.x = ::Random.randF(0.f, 1000.f) - 500.f;
			v.y = ::Random.randF(0.f, 1000.f);
			v.z = ::Random.randF(0.f, 1000.f) - 500;
			float radius = ((p.max_radius - p.min_radius) * rand() / 1000.0f) + p.min_radius;
			v.set_length(radius);
			v.add(va);
			spawn_phantom(&v);
		}
	}*/

	return result;
}

bool CControllerPsyHit::see_enemy(CActor* pA)
{
	return	m_object->EnemyMan.see_enemy_now(pA);
}

bool CControllerPsyHit::check_conditions_final()
{
	CActor* pActor = Actor();
	if (!m_object->g_Alive())
		return false;
	// 	if (m_object->EnemyMan.get_enemy() != pActor	
	// 		return false;

	if (IsGameTypeSingle())
	{
		if (!g_actor)
			return false;

		if (!m_object->EnemyMan.is_enemy(pActor))
			return false;

		if (!pActor->g_Alive())
			return false;

		if (m_object->Position().distance_to_xz(pActor->Position()) < m_min_tube_dist - 2)
			return false;

		if (IsNeedPsiHitOverride())
			return false;
	}
	else
	{
		pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));

		if (!pActor)
			return false;

		if (!m_object->EnemyMan.is_enemy(pActor))
			return false;

		if (!pActor->g_Alive())
			return false;

		if (m_object->Position().distance_to_xz(pActor->Position()) < m_min_tube_dist - 2)
			return false;

		if (IsNeedPsiHitOverride())
			return false;
	}

	if (IsGameTypeSingle())
		return see_enemy(pActor);
	else
		return see_enemy(pActor);
}

void CControllerPsyHit::death_glide_start()
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	
	if (isGuns)
	{
		if (!check_conditions_final() && PsiStart(static_cast<CController*>(m_object)))
		{
			m_man->deactivate(this);
			return;
		}
	}
	else
	{
		if (!check_conditions_final())
		{
			m_man->deactivate(this);
			return;
		}
	}
	
	if (isGuns)
		return;

	CActor* pActor = Actor();

	if (!IsGameTypeSingle())
	{
		pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));
		if (!pActor)
		{
			return;
		}
	}

	if (IsGameTypeSingle())
	{
		HUD().SetRenderable(false);

		if (CController* controller = smart_cast<CController*>(m_object))
		{
			controller->CControlledActor::install();
			controller->CControlledActor::dont_need_turn();
		}

		// Start effector
		CEffectorCam* ce = pActor->Cameras().GetCamEffector(eCEControllerPsyHit);
		VERIFY(!ce);

		Fvector src_pos = pActor->cam_Active()->vPosition;
		Fvector target_pos = m_object->Position();
		target_pos.y += 1.2f;

		Fvector dir;
		dir.sub(target_pos,src_pos);

		float dist = dir.magnitude();
		dir.normalize();

		float const actor_psy_immunity = pActor->conditions().GetHitImmunity(ALife::eHitTypeTelepatic);

		target_pos.mad(src_pos,dir, 0.01f + actor_psy_immunity*(dist-4.8f));

		float const base_fov = g_fov;
		float const dest_fov = g_fov - (g_fov-10.f)*actor_psy_immunity;

		pActor->Cameras().AddCamEffector(new CControllerPsyHitCamEffector(eCEControllerPsyHit, src_pos,target_pos, m_man->animation().motion_time(m_stage[1], m_object->Visual()), base_fov, dest_fov));

		smart_cast<CController*>(m_object)->draw_fire_particles();

		dir.sub(src_pos,target_pos);
		dir.normalize();
		float h,p;
		dir.getHP(h,p);
		dir.setHP(h,p+PI_DIV_3);
		pActor->character_physics_support()->movement()->ApplyImpulse(dir, pActor->GetMass() * 530.f);

		set_sound_state(eStart);
	}
	else
	{
		NET_Packet	tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
		tmp_packet.w_u16(pActor->ID());
		tmp_packet.w_u8(1);
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
	}

	if (IsGameTypeSingle())
	{
		NET_Packet P;
		pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		pActor->u_EventSend(P);
	}
	else
	{
		NET_Packet P;
		pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		pActor->u_EventSend(P);
	}
	m_blocked = true;

	//////////////////////////////////////////////////////////////////////////
	// set direction
	SControlDirectionData* ctrl_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir); 
	VERIFY(ctrl_dir);
	if (ctrl_dir == nullptr)
	{
		return;
	}
	ctrl_dir->heading.target_speed = 3.f;
	ctrl_dir->heading.target_angle = m_man->direction().angle_to_target(pActor->Position());

	//////////////////////////////////////////////////////////////////////////
}

void CControllerPsyHit::death_glide_end()
{
	CActor* pActor = Actor();
	if (IsGameTypeSingle()) {
	CController *monster = smart_cast<CController *>(m_object);
	monster->draw_fire_particles();

	monster->m_sound_tube_hit_left.play_at_pos(pActor, Fvector().set(-1.f, 0.f, 1.f), sm_2D);
	monster->m_sound_tube_hit_right.play_at_pos(pActor, Fvector().set(1.f, 0.f, 1.f), sm_2D);

	m_object->Hit_Psy		(pActor, monster->m_tube_damage);
	}	
	else
	{
		CController* monster = smart_cast<CController*>(m_object);
		CActor* pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));
		if (pActor)
		{
			m_object->Hit_Psy(pActor, monster->m_tube_damage);
		}
	}

	m_time_last_tube	=	Device.dwTimeGlobal;
	stop					();
}

void CControllerPsyHit::update_frame()
{
}

void CControllerPsyHit::set_sound_state(ESoundState state)
{
	CActor* pActor = Actor();
	CController *monster = smart_cast<CController *>(m_object);
	if (state == ePrepare) {
		monster->m_sound_tube_prepare.play_at_pos(pActor, Fvector().set(0.f, 0.f, 0.f), sm_2D);
	} else 
	if (state == eStart) {
		if (monster->m_sound_tube_prepare._feedback())	monster->m_sound_tube_prepare.stop();

		monster->m_sound_tube_start.play_at_pos(pActor, Fvector().set(0.f, 0.f, 0.f), sm_2D);
		monster->m_sound_tube_pull.play_at_pos(pActor, Fvector().set(0.f, 0.f, 0.f), sm_2D);
	} else 
	if (state == eHit) {
		if (monster->m_sound_tube_start._feedback())	monster->m_sound_tube_start.stop();
		if (monster->m_sound_tube_pull._feedback())		monster->m_sound_tube_pull.stop();
	} else 
	if (state == eNone) {
		if (monster->m_sound_tube_start._feedback())	monster->m_sound_tube_start.stop();
		if (monster->m_sound_tube_pull._feedback())		monster->m_sound_tube_pull.stop();
		if (monster->m_sound_tube_prepare._feedback())	monster->m_sound_tube_prepare.stop();
	}

	m_sound_state = state;
}

void CControllerPsyHit::hit()
{	
	if (!IsGameTypeSingle())
	{
		CActor* pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));
		if (pActor)
		{
			NET_Packet	tmp_packet;
			CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
			tmp_packet.w_u16(pActor->ID());
			tmp_packet.w_u8(3);
			Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
		}
	}

	set_sound_state			(eHit);
}

void CControllerPsyHit::stop ()
{
	CActor* pActor = Actor();
	if (IsGameTypeSingle())
	{
	HUD().SetRenderable(true);

	if ( CController* controller = smart_cast<CController*>(m_object) )
		if ( controller->CControlledActor::is_controlling() )
			controller->CControlledActor::release();

	// Stop camera effector
	CEffectorCam* ce = pActor->Cameras().GetCamEffector(eCEControllerPsyHit);
	if (ce)
		pActor->Cameras().RemoveCamEffector(eCEControllerPsyHit);
	}
	else
	{
		CActor* pActor = smart_cast<CActor*>(Level().Objects.net_Find(m_curent_actor_id));
		if (pActor)
		{
			NET_Packet	tmp_packet;
			CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
			tmp_packet.w_u16(pActor->ID());
			tmp_packet.w_u8(2);
			Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
		}
	}

}

void CControllerPsyHit::on_death()
{
	if (!is_active()) 
		return;

	stop ();

	m_man->deactivate		(this);
}
