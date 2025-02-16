#pragma once

#include "zombie_choke_execute.h"
#include "../../../clsid_game.h"
#include "../../../entity_alive.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateZombieChokeAbstract CStateZombieChoke<_Object>

#define MAX_DISTANCE_TO_ENEMY		1.6f

TEMPLATE_SPECIALIZATION


CStateZombieChokeAbstract::CStateZombieChoke(_Object *obj) : inherited(obj)
{
	this->add_state	(eStateChoke_Execute, new CStateZombieChokeExecute<_Object>(obj));
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeAbstract::reinit()
{
	inherited::reinit	();
	
	m_time_last_choke	= 0;
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeAbstract::initialize()
{
	inherited::initialize						();

	m_enemy	= this->object->EnemyMan.get_enemy		();
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeAbstract::reselect_state()
{
	if (this->get_state(eStateChoke_Execute)->check_start_conditions())
		this->select_state(eStateChoke_Execute);
	
	//ctd. here !!!!
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeAbstract::check_force_state()
{
	// check if we can start execute
	if (this->prev_substate == eStateChoke_Execute)
	{
		if (this->get_state(eStateChoke_Execute)->check_start_conditions())
			this->current_substate = u32(-1);
	}
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeAbstract::finalize()
{
	inherited::finalize();

	m_time_last_choke				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeAbstract::critical_finalize()
{
	inherited::critical_finalize	();

	m_time_last_choke				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
bool CStateZombieChokeAbstract::check_start_conditions()
{
	if (!this->object->WantChoke()) return false;

	const CEntityAlive *m_enemy = this->object->EnemyMan.get_enemy();
	if (!this->object->EnemyMan.see_enemy_now())								return false;

	if (!this->get_state(eStateChoke_Execute)->check_start_conditions())					return false;

	return true;
  }

TEMPLATE_SPECIALIZATION
bool CStateZombieChokeAbstract::check_completion()
{
	// если враг изменился
	if (m_enemy != this->object->EnemyMan.get_enemy())		return true;
	
	// если актера уже душит второй зомби
	if ((this->current_substate != eStateChoke_Execute) &&
		this->object->CControlledActor::is_controlling())	return true;

	if (this->current_substate == eStateChoke_Execute && this->get_state(eStateChoke_Execute)->check_completion())	return true;

	return false;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateZombieChokeAbstract

