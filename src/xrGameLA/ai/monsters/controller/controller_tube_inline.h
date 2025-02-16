#pragma once

#include "controller_psy_hit.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateControllerTubeAbstract CStateControllerTube<_Object>

#define TUBE_PROBABILITY 20

TEMPLATE_SPECIALIZATION
void CStateControllerTubeAbstract::execute()
{
//	object->control().activate	(ControlCom::eComCustom1);
//	object->set_action			(ACT_STAND_IDLE);
	this->object->m_time_last_tube	= time();

	// missed
	//if (!object->tube_at_once() && (Random.randI(100) > TUBE_PROBABILITY)) return;

	this->object->control().activate	(ControlCom::eComCustom1);
}

#define SEE_ENEMY_DURATION 1000

TEMPLATE_SPECIALIZATION
bool CStateControllerTubeAbstract::check_start_conditions()
{
	/*
	if (object->EnemyMan.see_enemy_duration() < SEE_ENEMY_DURATION) return false;
	if (!object->m_psy_hit->check_start_conditions()) return false;

	return true;
	*/
	return this->object->can_tube_fire();
}

TEMPLATE_SPECIALIZATION
bool CStateControllerTubeAbstract::check_completion()
{
	return (!this->object->m_psy_hit->is_active());
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateControllerTubeAbstract
