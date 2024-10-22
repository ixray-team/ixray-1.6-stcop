#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "chimera.h"
#include "chimera_state_threaten.h"

#include "../states/state_move_to_point.h"

#include "chimera_state_threaten_steal.h"
#include "chimera_state_threaten_walk.h"
#include "chimera_state_threaten_roar.h"

CStateChimeraThreaten::CStateChimeraThreaten(CBaseMonster* object) : inherited(object)
{
	m_last_time_threaten = {};

	add_state(eStateWalk, new CStateChimeraThreatenWalk(object));
	add_state(eStateThreaten, new CStateChimeraThreatenRoar(object));
	add_state(eStateSteal, new CStateChimeraThreatenSteal(object));
}

CStateChimeraThreaten::~CStateChimeraThreaten()
{

}

void CStateChimeraThreaten::reinit()
{
	inherited::reinit();

	m_last_time_threaten = 0;
}

bool CStateChimeraThreaten::check_start_conditions()
{
	if (object->tfGetRelationType(object->EnemyMan.get_enemy()) == ALife::eRelationTypeWorstEnemy) return false;
	if (object->Position().distance_to(object->EnemyMan.get_enemy_position()) < EntityDefinitions::CChimeraBase::MIN_DIST_TO_ENEMY) return false;
	if (object->HitMemory.is_hit())						return false;
	if (object->hear_dangerous_sound)					return false;
	if (m_last_time_threaten + EntityDefinitions::CChimeraBase::THREATEN_DELAY > Device.dwTimeGlobal) return false;

	return true;
}

bool CStateChimeraThreaten::check_completion()
{
	if (object->Position().distance_to(object->EnemyMan.get_enemy_position()) < EntityDefinitions::CChimeraBase::MIN_DIST_TO_ENEMY) return true;
	if (object->HitMemory.is_hit()) return true;
	if (object->tfGetRelationType(object->EnemyMan.get_enemy()) == ALife::eRelationTypeWorstEnemy) return true;

	return false;
}

void CStateChimeraThreaten::initialize()
{
	inherited::initialize();
}

void CStateChimeraThreaten::reselect_state()
{
	if (prev_substate == u32(-1)) {
		select_state(eStateThreaten);
		return;
	}

	if (prev_substate == eStateSteal) {
		select_state(eStateThreaten);
		return;
	}

	if (prev_substate == eStateThreaten) {
		if (get_state(eStateSteal)->check_start_conditions()) {
			select_state(eStateSteal);
			return;
		}
		else if (get_state(eStateWalk)->check_start_conditions()) {
			select_state(eStateWalk);
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
