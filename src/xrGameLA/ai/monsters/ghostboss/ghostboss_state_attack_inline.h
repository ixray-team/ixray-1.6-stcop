#pragma once

#include "ghostboss_state_attack_tele.h"
#include "ghostboss_state_attack_teleport.h"
#include "ghostboss_state_attack_gravi.h"
#include "ghostboss_state_attack_melee.h"
#include "ghostboss_state_attack_shield.h"
#include "../states/state_look_point.h"
#include "../states/state_move_to_restrictor.h"
#include "ghostboss_state_attack_run_around.h"

#define GRAVI_PERCENT		80
#define TELE_PERCENT		50
#define RUN_AROUND_PERCENT	20

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateGhostBossAttackAbstract CStateGhostBossAttack<_Object>

TEMPLATE_SPECIALIZATION
CStateGhostBossAttackAbstract::CStateGhostBossAttack(_Object *obj) : inherited(obj)
{
	this->add_state(eStateGhostBossAttack_Tele, new CStateGhostBossAttackTele<_Object>(obj));
	this->add_state(eStateGhostBossAttack_Gravi, new CStateGhostBossAttackGravi<_Object>(obj));
	this->add_state(eStateGhostBossAttack_Melee, new CStateGhostBossAttackMelee<_Object>(obj));
	
	this->add_state(eStateGhostBossAttack_FaceEnemy, new CStateMonsterLookToPoint<_Object>(obj));
	this->add_state(eStateGhostBossAttack_RunAround, new CStateGhostBossAttackRunAround<_Object>(obj));

	this->add_state(eStateCustomMoveToRestrictor, new CStateMonsterMoveToRestrictor<_Object>(obj));
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossAttackAbstract::initialize()
{
	inherited::initialize	();
	m_force_gravi			= false;
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossAttackAbstract::reselect_state()
{
	if (this->get_state(eStateGhostBossAttack_Melee)->check_start_conditions()) {
		this->select_state(eStateGhostBossAttack_Melee);
		return;
	}

	if (m_force_gravi) {
		m_force_gravi = false;

		if (this->get_state(eStateGhostBossAttack_Gravi)->check_start_conditions()) {
			this->select_state		(eStateGhostBossAttack_Gravi);
			return;
		}
	}

	if (this->get_state(eStateCustomMoveToRestrictor)->check_start_conditions()) {
		this->select_state(eStateCustomMoveToRestrictor);
		return;
	}

	bool enable_gravi	= false;//get_state(eStateGhostBossAttack_Gravi)->check_start_conditions	();
	bool enable_tele	= this->get_state(eStateGhostBossAttack_Tele)->check_start_conditions		();

	if (!enable_gravi && !enable_tele) {
		if (this->prev_substate == eStateGhostBossAttack_RunAround)
			this->select_state(eStateGhostBossAttack_FaceEnemy);
		else 	
			this->select_state(eStateGhostBossAttack_RunAround);
		return;
	}

	if (enable_gravi && enable_tele) {

		u32 rnd_val = ::Random.randI(GRAVI_PERCENT + TELE_PERCENT + RUN_AROUND_PERCENT);
		u32 cur_val = GRAVI_PERCENT;

		if (rnd_val < cur_val) {
			this->select_state(eStateGhostBossAttack_Gravi);
			return;
		}

		cur_val += TELE_PERCENT;
		if (rnd_val < cur_val) {
			this->select_state(eStateGhostBossAttack_Tele);
			return;
		}

		this->select_state(eStateGhostBossAttack_RunAround);
		return;
	}

	if ((this->prev_substate == eStateGhostBossAttack_RunAround) || (this->prev_substate == eStateGhostBossAttack_FaceEnemy)) {
		if (enable_gravi) this->select_state(eStateGhostBossAttack_Gravi);
		else this->select_state(eStateGhostBossAttack_Tele);
	} else {
		this->select_state(eStateGhostBossAttack_RunAround);
	}
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossAttackAbstract::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateGhostBossAttack_FaceEnemy) {
		SStateDataLookToPoint data;
		
		data.point				= this->object->EnemyMan.get_enemy()->Position();
		data.action.action		= ACT_STAND_IDLE;
		data.action.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = this->object->db().m_dwAttackSndDelay;

		state->fill_data_with	(&data, sizeof(SStateDataLookToPoint));
		return;
	}

}

TEMPLATE_SPECIALIZATION
void CStateGhostBossAttackAbstract::check_force_state()
{
	// check if we can start execute
	if ((this->current_substate == eStateCustomMoveToRestrictor) || (this->prev_substate == eStateGhostBossAttack_RunAround)) {
		if (this->get_state(eStateGhostBossAttack_Gravi)->check_start_conditions()) {
			this->current_substate	= u32(-1);
			m_force_gravi		= true;
		}
	}
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateGhostBossAttackAbstract
