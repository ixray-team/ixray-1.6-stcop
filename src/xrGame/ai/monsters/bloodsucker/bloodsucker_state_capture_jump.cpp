#include "stdafx.h"
#include "bloodsucker.h"
#include "bloodsucker_state_capture_jump.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

//#include "../states/monster_state_rest_idle.h"
#include "../states/state_custom_action.h"

CustomBloodsuckerStateJump::CustomBloodsuckerStateJump(CustomBloodsucker* object) : inherited(object)
{
	this->add_state(eStateCustom, new CStateMonsterCustomAction(object));
}

CustomBloodsuckerStateJump::~CustomBloodsuckerStateJump()
{

}

void CustomBloodsuckerStateJump::execute()
{
	this->select_state(eStateCustom);

	this->get_state_current()->execute();
	this->prev_substate = this->current_substate;
}

void CustomBloodsuckerStateJump::setup_substates()
{
	state_ptr state = this->get_state_current();
	if (this->current_substate == eStateCustom) {
		SStateDataAction data;

		data.action = ACT_STAND_IDLE;
		data.time_out = 0;			// do not use time out
		/*data.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.sound_delay = object->db().m_dwIdleSndDelay;
		*/
		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}
