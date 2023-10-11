#pragma once

#include "chimera_state_threaten_steal.h"
#include "chimera_state_threaten_walk.h"
#include "chimera_state_threaten_roar.h"


#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateChimeraThreatenAbstract CStateChimeraThreaten<_Object>

TEMPLATE_SPECIALIZATION
CStateChimeraThreatenAbstract::CStateChimeraThreaten(_Object *obj) : inherited(obj)
{
	add_state(eStateWalk,		xr_new<CStateChimeraThreatenWalk<_Object> >	(obj));
	add_state(eStateThreaten,	xr_new<CStateChimeraThreatenRoar<_Object> >	(obj));
	add_state(eStateSteal,		xr_new<CStateChimeraThreatenSteal<_Object> >(obj));
}

TEMPLATE_SPECIALIZATION
CStateChimeraThreatenAbstract::~CStateChimeraThreaten()
{
}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenAbstract::reinit()
{
	inherited::reinit	();

	m_last_time_threaten = 0;
}


#define MIN_DIST_TO_ENEMY	3.f
#define MORALE_THRESHOLD	0.8f
#define THREATEN_DELAY		10000

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenAbstract::check_start_conditions()
{
	if (this->object->tfGetRelationType(this->object->EnemyMan.get_enemy()) == ALife::eRelationTypeWorstEnemy) return false;
	if (this->object->Position().distance_to(this->object->EnemyMan.get_enemy_position()) < MIN_DIST_TO_ENEMY) return false;
	if (this->object->HitMemory.is_hit())						return false;
	if (this->object->hear_dangerous_sound)					return false;
	if (this->m_last_time_threaten + THREATEN_DELAY > Device.dwTimeGlobal) return false;

	return true;
}

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenAbstract::check_completion()
{
	if (this->object->Position().distance_to(this->object->EnemyMan.get_enemy_position()) < MIN_DIST_TO_ENEMY) return true;
	if (this->object->HitMemory.is_hit()) return true;
	if (this->object->tfGetRelationType(this->object->EnemyMan.get_enemy()) == ALife::eRelationTypeWorstEnemy) return true;

	return false;
}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenAbstract::initialize()
{
	inherited::initialize	();
}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenAbstract::reselect_state()
{
	if (this->prev_substate == u32(-1)) {
		this->select_state(eStateThreaten);
		return;
	}

	if (this->prev_substate == eStateSteal) {
		this->select_state(eStateThreaten);
		return;
	}

	if (this->prev_substate == eStateThreaten) {
		if (this->get_state(eStateSteal)->check_start_conditions()) {
			this->select_state(eStateSteal);
			return;
		} else if (this->get_state(eStateWalk)->check_start_conditions()) {
			this->select_state(eStateWalk);
			return;
		}
	}

	select_state(eStateThreaten);
}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenAbstract::finalize()
{
	inherited::finalize		();
	m_last_time_threaten	 = Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenAbstract::critical_finalize()
{
	inherited::critical_finalize();
	m_last_time_threaten	 = Device.dwTimeGlobal;
}


#undef TEMPLATE_SPECIALIZATION
#undef CStateChimeraThreatenAbstract
