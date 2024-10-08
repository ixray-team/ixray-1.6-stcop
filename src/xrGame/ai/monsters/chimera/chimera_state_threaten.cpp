#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "chimera.h"
#include "chimera_state_threaten.h"

#include "../states/state_move_to_point.h"

#include "chimera_state_threaten_steal.h"
#include "chimera_state_threaten_walk.h"
#include "chimera_state_threaten_roar.h"


CStateChimeraThreaten::CStateChimeraThreaten(CBaseMonster* obj) : inherited(obj)
{
	add_state(eStateWalk, new CStateChimeraThreatenWalk(obj));
	add_state(eStateThreaten, new CStateChimeraThreatenRoar(obj));
	add_state(eStateSteal, new CStateChimeraThreatenSteal(obj));
}


CStateChimeraThreaten::~CStateChimeraThreaten()
{
}


void CStateChimeraThreaten::reinit()
{
	inherited::reinit();

	m_last_time_threaten = 0;
}


#define MIN_DIST_TO_ENEMY	3.f
#define MORALE_THRESHOLD	0.8f
#define THREATEN_DELAY		10000


bool CStateChimeraThreaten::check_start_conditions()
{
	if (this->object->tfGetRelationType(this->object->EnemyMan.get_enemy()) == ALife::eRelationTypeWorstEnemy) return false;
	if (this->object->Position().distance_to(this->object->EnemyMan.get_enemy_position()) < MIN_DIST_TO_ENEMY) return false;
	if (this->object->HitMemory.is_hit())						return false;
	if (this->object->hear_dangerous_sound)					return false;
	if (this->m_last_time_threaten + THREATEN_DELAY > Device.dwTimeGlobal) return false;

	return true;
}


bool CStateChimeraThreaten::check_completion()
{
	if (this->object->Position().distance_to(this->object->EnemyMan.get_enemy_position()) < MIN_DIST_TO_ENEMY) return true;
	if (this->object->HitMemory.is_hit()) return true;
	if (this->object->tfGetRelationType(this->object->EnemyMan.get_enemy()) == ALife::eRelationTypeWorstEnemy) return true;

	return false;
}


void CStateChimeraThreaten::initialize()
{
	inherited::initialize();
}


void CStateChimeraThreaten::reselect_state()
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
		}
		else if (this->get_state(eStateWalk)->check_start_conditions()) {
			this->select_state(eStateWalk);
			return;
		}
	}

	select_state(eStateThreaten);
}


void CStateChimeraThreaten::finalize()
{
	inherited::finalize();
	m_last_time_threaten = Device.dwTimeGlobal;
}


void CStateChimeraThreaten::critical_finalize()
{
	inherited::critical_finalize();
	m_last_time_threaten = Device.dwTimeGlobal;
}
