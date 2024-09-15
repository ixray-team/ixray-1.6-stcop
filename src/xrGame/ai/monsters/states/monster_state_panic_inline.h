#pragma once

#include "state_data.h"
#include "state_move_to_point.h"
#include "state_custom_action.h"
#include "state_look_unprotected_area.h"
#include "monster_state_panic_run.h"
#include "monster_state_home_point_attack.h"


CStateMonsterPanic::CStateMonsterPanic(CBaseMonster *obj) : inherited(obj)
{
	this->add_state(eStatePanic_Run,					xr_new<CStateMonsterPanicRun>(obj));
	this->add_state(eStatePanic_FaceUnprotectedArea,	xr_new<CStateMonsterLookToUnprotectedArea>(obj));
	this->add_state(eStatePanic_MoveToHomePoint,		xr_new<CStateMonsterAttackMoveToHomePoint>(obj));
}


CStateMonsterPanic::~CStateMonsterPanic()
{
}


void CStateMonsterPanic::initialize()
{
	inherited::initialize();
}


void CStateMonsterPanic::reselect_state()
{
	if (this->get_state(eStatePanic_MoveToHomePoint)->check_start_conditions()) {
		this->select_state(eStatePanic_MoveToHomePoint);
		return;
	}

	if (this->prev_substate == eStatePanic_Run) this->select_state(eStatePanic_FaceUnprotectedArea);
	else this->select_state(eStatePanic_Run);
}


void CStateMonsterPanic::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStatePanic_FaceUnprotectedArea) {
		SStateDataAction data;
		
		data.action			= ACT_STAND_IDLE;
		data.spec_params	= ASP_STAND_SCARED;
		data.time_out		= 3000;		
		data.sound_type		= MonsterSound::eMonsterSoundPanic;
		data.sound_delay	= this->object->db().m_dwAttackSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}

}


void CStateMonsterPanic::check_force_state()
{
	if ((this->current_substate == eStatePanic_FaceUnprotectedArea)){
		// если видит врага
		if (this->object->EnemyMan.get_enemy_time_last_seen() == Device.dwTimeGlobal) {
			this->select_state(eStatePanic_Run);
			return;
		}
		// если получил hit
		if (this->object->HitMemory.get_last_hit_time() + 5000 > Device.dwTimeGlobal) {
			this->select_state(eStatePanic_Run);
			return;
		}
	}
}
