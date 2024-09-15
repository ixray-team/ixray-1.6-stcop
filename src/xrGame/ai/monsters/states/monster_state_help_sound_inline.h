#pragma once

#include "state_move_to_point.h"
#include "state_custom_action_look.h"


CStateMonsterHearHelpSound::CStateMonsterHearHelpSound(CBaseMonster*obj) : inherited(obj)
{
	this->add_state	(eStateHearHelpSound_MoveToDest,	xr_new<CStateMonsterMoveToPointEx>(obj));
	this->add_state	(eStateHearHelpSound_LookAround,	xr_new<CStateMonsterCustomActionLook>(obj));
}

bool CStateMonsterHearHelpSound::check_start_conditions()
{
	if (!this->object->SoundMemory.hear_help_sound()) return false;
	if (this->object->Home->has_home())				return this->object->Home->at_home(ai().level_graph().vertex_position(this->object->SoundMemory.hear_help_sound_node()));

	return true;
}

bool CStateMonsterHearHelpSound::check_completion()
{
	if (this->current_substate == u32(-1)) return true;
	return false;
}

void CStateMonsterHearHelpSound::reselect_state()
{
	if (this->prev_substate == u32(-1))
		this->select_state(eStateHearHelpSound_MoveToDest);
	else if (this->prev_substate == eStateHearHelpSound_MoveToDest)
		this->select_state(eStateHearHelpSound_LookAround);
}

void CStateMonsterHearHelpSound::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateHearHelpSound_MoveToDest) {
		SStateDataMoveToPointEx data;

		data.vertex				= this->object->SoundMemory.hear_help_sound_node();
		data.point				= ai().level_graph().vertex_position(data.vertex);
		data.action.action		= ACT_RUN;
		data.action.time_out	= 0;		// do not use time out
		data.completion_dist	= 0.f;		// get exactly to the point
		data.time_to_rebuild	= 0;		// do not rebuild
		data.accelerated		= true;
		data.braking			= true;
		data.accel_type 		= eAT_Aggressive;
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));

		return;
	}

	if (this->current_substate == eStateHearHelpSound_LookAround) {
		SStateDataAction	data;
		data.action			= ACT_LOOK_AROUND;
		data.time_out		= 3000;
		data.sound_type		= MonsterSound::eMonsterSoundIdle;
		data.sound_delay	= this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}
