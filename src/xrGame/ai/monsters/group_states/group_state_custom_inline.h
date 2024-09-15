#pragma once

#include "../states/state_custom_action.h"



CStateCustomGroup::CStateCustomGroup(CBaseMonster *obj) : inherited(obj)
{
	this->add_state(eStateCustom,				xr_new<CStateMonsterCustomAction<CBaseMonster> >		(obj));
}


CStateCustomGroup::~CStateCustomGroup	()
{
}


void CStateCustomGroup::execute()
{
	// check alife control
	this->select_state	(eStateCustom);
	this->object->start_animation();

	this->get_state_current()->execute();
	this->prev_substate = this->current_substate;
}

void CStateCustomGroup::setup_substates()
{
	state_ptr state = this->get_state_current();
	if (this->current_substate == eStateCustom) {
		SStateDataAction data;

		data.action		= ACT_STAND_IDLE;
		data.time_out	= 0;			// do not use time out
		switch(this->object->get_number_animation())
		{
		case u32(5):
			data.sound_type	= MonsterSound::eMonsterSoundSteal;
			break;
		case u32(6):
			data.sound_type	= MonsterSound::eMonsterSoundThreaten;
			break;
		default:
			data.sound_type	= MonsterSound::eMonsterSoundIdle;
			break;
		}
		data.sound_delay = this->object->db().m_dwEatSndDelay;
		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}
