#pragma once

#include "../../../level.h"
#include "state_move_to_point.h"
#include "state_custom_action.h"


CStateMonsterTestState::CStateMonsterTestState(CBaseMonster*obj) : inherited(obj)
{
	add_state(eStateCustom,xr_new<CStateMonsterMoveToPointEx<CBaseMonster> >(obj));
}


void CStateMonsterTestState::reselect_state()
{
	this->select_state(eStateCustom);
}


void CStateMonsterTestState::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateCustom) {
		SStateDataMoveToPointEx data;

		Fvector dest_pos = Level().CurrentEntity()->Position();
		dest_pos = random_position(dest_pos, 20.f);

		if (!this->object->control().path_builder().restrictions().accessible(dest_pos)) {
			data.vertex		= this->object->control().path_builder().restrictions().accessible_nearest(dest_pos, data.point);
		} else {
			data.point		= dest_pos;
			data.vertex		= u32(-1);
		}

		data.action.action		= ACT_RUN;
		data.action.time_out	= 20000;
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type 		= eAT_Calm;
		data.completion_dist	= 3.f;
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
		data.time_to_rebuild	= 0;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));

		return;
	}
}


CStateMonsterTestCover::CStateMonsterTestCover(_Object *obj) : inherited(obj) 
{
	this->add_state(eStateAttack_HideInCover,xr_new<CStateMonsterMoveToPointEx<_Object> >(obj));
	this->add_state(eStateAttack_CampInCover,xr_new<CStateMonsterCustomAction<_Object> >(obj));
}

void CStateMonsterTestCover::initialize()
{
	inherited::initialize();

	m_last_node	= this->object->m_target_node;
}



void CStateMonsterTestCover::check_force_state()
{
	if (m_last_node	!= this->object->m_target_node) {
		m_last_node			= this->object->m_target_node;
		this->current_substate	= u32(-1);
		return;
	}
	
	if (this->current_substate == eStateAttack_CampInCover)
		if (this->object->ai_location().level_vertex_id() != m_last_node)
			this->current_substate = u32(-1);
}

void CStateMonsterTestCover::reselect_state()
{
	if (this->object->ai_location().level_vertex_id() != m_last_node)
		this->select_state(eStateAttack_HideInCover);
	else 
		this->select_state(eStateAttack_CampInCover);
}


void CStateMonsterTestCover::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateAttack_HideInCover) {
		SStateDataMoveToPointEx data;
		data.vertex				= m_last_node;
		data.point				= ai().level_graph().vertex_position(data.vertex);
		data.action.action		= ACT_RUN;
		data.action.time_out	= 200000;
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type 		= eAT_Aggressive;
		data.completion_dist	= 0.f;
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
		data.time_to_rebuild	= 0;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));
		return;
	}

	if (this->current_substate == eStateAttack_CampInCover) {
		SStateDataAction	data;
		data.action			= ACT_STAND_IDLE;
		data.sound_type		= MonsterSound::eMonsterSoundIdle;
		data.sound_delay	= this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));
		return;
	}

}
