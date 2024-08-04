#include "stdafx.h"
#include "controller_psy_hit.h"
#include "../BaseMonster/base_monster.h"
#include "controller.h"
#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../../../level.h"
#include "../../../actor.h"
#include "../../../ActorEffector.h"
#include "../../../../xrEngine/CameraBase.h"
#include "../../../CharacterPhysicsSupport.h"
#include "../../../level_debug.h"
#include "../../../ActorCondition.h"
#include "../../../HudManager.h"



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

bool CControllerPsyHit::check_start_conditions()
{
	CActor* pActor = Actor();
	if (is_active())				
		return false;	

	if (IsGameTypeSingle()) {
		if (pActor->Cameras().GetCamEffector(eCEControllerPsyHit))
			return						false;

	if ( !see_enemy(pActor) )
			return						false;

	if ( !tube_ready() )
			return						false;

		if (m_object->Position().distance_to(pActor->Position()) < m_min_tube_dist)
			return						false;
	}
	else
	{
		CActor* pA = const_cast<CActor*>(smart_cast<const CActor*>(m_object->EnemyMan.get_enemy()));
		if (pA) {
			m_curent_actor_id = u16(-1);

			if (pA->Cameras().GetCamEffector(eCEControllerPsyHit))
				return						false;

			if (!see_enemy(pA))
				return						false;

			if (!tube_ready())
				return						false;

			if (m_object->Position().distance_to(pA->Position()) < m_min_tube_dist)
				return						false;

			m_curent_actor_id = pA->ID();
		}
		else
			return false;
	}
	return							true;
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

bool CControllerPsyHit::see_enemy(CActor* pA)
{
	return	m_object->EnemyMan.see_enemy_now(pA);
}

bool CControllerPsyHit::check_conditions_final() {
	CActor* pActor = Actor();
	if (!m_object->g_Alive())
		return false;
	// 	if (m_object->EnemyMan.get_enemy() != pActor	
	// 		return false;

	if (IsGameTypeSingle()) {
		if (!g_actor)
			return false;

		if (!m_object->EnemyMan.is_enemy(pActor))
			return false;

		if (!pActor->g_Alive())
			return false;

		if (m_object->Position().distance_to_xz(pActor->Position()) < m_min_tube_dist - 2)
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
	}

	if (IsGameTypeSingle())
		return see_enemy(pActor);
	else
		return see_enemy(pActor);
}

void CControllerPsyHit::death_glide_start()
{
	CActor* pActor = Actor();
	if (!check_conditions_final()) {
		m_man->deactivate	(this);
		return;
	}
	
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

	if ( CController* controller = smart_cast<CController*>(m_object) )
		{
			controller->CControlledActor::install	();
			controller->CControlledActor::dont_need_turn();
		}

		// Start effector
		CEffectorCam* ce = pActor->Cameras().GetCamEffector(eCEControllerPsyHit);
		VERIFY(!ce);

	Fvector src_pos		= pActor->cam_Active()->vPosition;
	Fvector target_pos	= m_object->Position();
	target_pos.y		+= 1.2f;

		Fvector				dir;
	dir.sub				(target_pos,src_pos);

	float dist			= dir.magnitude();
	dir.normalize		();


	float const actor_psy_immunity	= pActor->conditions().GetHitImmunity(ALife::eHitTypeTelepatic);

	target_pos.mad		(src_pos,dir, 0.01f + actor_psy_immunity*(dist-4.8f) );


	float const base_fov	=	g_fov;
	float const dest_fov	=	g_fov - (g_fov-10.f)*actor_psy_immunity;

	pActor->Cameras().AddCamEffector(new CControllerPsyHitCamEffector(eCEControllerPsyHit, src_pos,target_pos,
			m_man->animation().motion_time(m_stage[1], m_object->Visual()),
			base_fov, dest_fov));

	smart_cast<CController *>(m_object)->draw_fire_particles();

	dir.sub(src_pos,target_pos);
		dir.normalize();
	float h,p;
	dir.getHP(h,p);
	dir.setHP(h,p+PI_DIV_3);
		pActor->character_physics_support()->movement()->ApplyImpulse(dir, pActor->GetMass() * 530.f);

	set_sound_state					(eStart);

	}
	else {
		NET_Packet	tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
		tmp_packet.w_u16(pActor->ID());
		tmp_packet.w_u8(1);
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
	}

	if (IsGameTypeSingle())
	{
		NET_Packet			P;
	pActor->u_EventGen	(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
	P.w_u16				(INV_STATE_BLOCK_ALL);
	P.w_u8				(u8(true));
		pActor->u_EventSend(P);
	}
	else {
		NET_Packet			P;
		pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		pActor->u_EventSend(P);
	}
	m_blocked			= true;

	//////////////////////////////////////////////////////////////////////////
	// set direction
	SControlDirectionData			*ctrl_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir); 
	VERIFY							(ctrl_dir);
	ctrl_dir->heading.target_speed	= 3.f;
	ctrl_dir->heading.target_angle	= m_man->direction().angle_to_target(pActor->Position());

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
