#include "stdafx.h"
#include "bloodsucker.h"
#include "bloodsucker_vampire_hide.h"

#include "../states/state_hide_from_point.h"
#include "bloodsucker_predator.h"

CustomBloodsuckerStateVampireHide::CustomBloodsuckerStateVampireHide(CustomBloodsucker* object) : inherited(object)
{
	add_state(eStateVampire_RunAway, new CStateMonsterHideFromPoint (object));
	add_state(eStatePredator, new CustomBloodsuckerStatePredator(object));
}

CustomBloodsuckerStateVampireHide::~CustomBloodsuckerStateVampireHide()
{

}

void CustomBloodsuckerStateVampireHide::reselect_state()
{
	if (this->prev_substate == eStateVampire_RunAway) {
		if (this->get_state(eStatePredator)->check_start_conditions()) {
			this->select_state(eStatePredator);
			return;
		}
	}

	this->select_state(eStateVampire_RunAway);
}

void CustomBloodsuckerStateVampireHide::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateVampire_RunAway) {
		SStateHideFromPoint		data;
		data.point = this->object->EnemyMan.get_enemy_position();
		data.accelerated = true;
		data.braking = false;
		data.accel_type = eAT_Aggressive;
		data.distance = 50.f;
		data.action.action = ACT_RUN;
		data.action.sound_type = MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = this->object->db().m_dwAttackSndDelay;
		data.action.time_out = 15000;

		state->fill_data_with(&data, sizeof(SStateHideFromPoint));

		return;
	}
}

bool CustomBloodsuckerStateVampireHide::check_completion()
{
	if ((this->current_substate == eStatePredator) &&
		this->get_state_current()->check_completion())	return true;

	return false;
}
