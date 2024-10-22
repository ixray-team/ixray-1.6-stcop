#include "StdAfx.h"
#include "monster_state_panic.h"


#include "state_data.h"
#include "state_move_to_point.h"
#include "state_custom_action.h"
#include "state_look_unprotected_area.h"
#include "monster_state_panic_run.h"
#include "monster_state_home_point_attack.h"


CStateMonsterPanic::CStateMonsterPanic(CBaseMonster* obj) : inherited(obj)
{
    add_state(eStatePanic_Run, new CStateMonsterPanicRun(obj));
    add_state(eStatePanic_FaceUnprotectedArea, new CStateMonsterLookToUnprotectedArea(obj));
    add_state(eStatePanic_MoveToHomePoint, new CStateMonsterAttackMoveToHomePoint(obj));
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
	if (get_state(eStatePanic_MoveToHomePoint)->check_start_conditions()) {
		select_state(eStatePanic_MoveToHomePoint);
		return;
	}

	if (prev_substate == eStatePanic_Run) select_state(eStatePanic_FaceUnprotectedArea);
	else select_state(eStatePanic_Run);
}


void CStateMonsterPanic::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStatePanic_FaceUnprotectedArea) {
		SStateDataAction data;

		data.action = ACT_STAND_IDLE;
		data.spec_params = ASP_STAND_SCARED;
		data.time_out = 3000;
		data.sound_type = MonsterSound::eMonsterSoundPanic;
		data.sound_delay = object->db().m_dwAttackSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}

}


void CStateMonsterPanic::check_force_state()
{
	if ((current_substate == eStatePanic_FaceUnprotectedArea)) {
		// если видит врага
		if (object->EnemyMan.get_enemy_time_last_seen() == Device.dwTimeGlobal) {
			select_state(eStatePanic_Run);
			return;
		}
		// если получил hit
		if (object->HitMemory.get_last_hit_time() + 5000 > Device.dwTimeGlobal) {
			select_state(eStatePanic_Run);
			return;
		}
	}
}
