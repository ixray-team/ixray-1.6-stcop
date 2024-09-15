#pragma once

#include "../monster_home.h"


void CStateMonsterRestMoveToHomePoint::initialize()
{
	inherited::initialize	();
	m_target_node			= this->object->Home->get_place_in_mid_home();
}


void CStateMonsterRestMoveToHomePoint::execute()
{
	this->object->path().set_target_point		(ai().level_graph().vertex_position(m_target_node), m_target_node);
	this->object->anim().accel_activate		(EAccelType(this->object->Home->is_aggressive() ? eAT_Aggressive : eAT_Calm));
	this->object->anim().accel_set_braking	(true);
	this->object->path().set_rebuild_time		(0);
	this->object->path().set_distance_to_end	(0.f);
	this->object->path().set_use_covers		(false);
	
	this->object->set_action					(this->object->Home->is_aggressive() ? ACT_RUN : ACT_WALK_FWD);
	this->object->set_state_sound				(this->object->Home->is_aggressive() ? MonsterSound::eMonsterSoundAggressive : MonsterSound::eMonsterSoundIdle);
}


bool CStateMonsterRestMoveToHomePoint::check_start_conditions()
{
	return (!this->object->Home->at_mid_home(this->object->Position()));
}


bool CStateMonsterRestMoveToHomePoint::check_completion()
{
	return ((this->object->ai_location().level_vertex_id() == m_target_node) && !this->object->control().path_builder().is_moving_on_path());
}
