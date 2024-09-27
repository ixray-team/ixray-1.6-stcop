#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "../states/state_data.h"
#include "../states/state_move_to_point.h"
#include "../states/state_look_unprotected_area.h"
#include "group_state_panic_run.h"
#include "../states/monster_state_home_point_attack.h"

#include "group_state_panic.h"

CStateGroupPanic::CStateGroupPanic(CBaseMonster* object) : inherited(object)
{
	add_state(eStatePanic_Run, new CStateGroupPanicRun (object));
	add_state(eStatePanic_FaceUnprotectedArea, new CStateMonsterLookToUnprotectedArea(object));
	add_state(eStatePanic_MoveToHomePoint, new CStateMonsterAttackMoveToHomePoint(object));
}

CStateGroupPanic::~CStateGroupPanic()
{
}

void CStateGroupPanic::initialize()
{
	inherited::initialize();
}

void CStateGroupPanic::reselect_state()
{
	if (get_state(eStatePanic_MoveToHomePoint)->check_start_conditions()) {
		select_state(eStatePanic_MoveToHomePoint);
		return;
	}

	if (prev_substate == eStatePanic_Run) select_state(eStatePanic_FaceUnprotectedArea);
	else select_state(eStatePanic_Run);
}

void CStateGroupPanic::setup_substates()
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

void CStateGroupPanic::check_force_state()
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
