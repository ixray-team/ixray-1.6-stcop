#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "chimera.h"

#include "../states/state_move_to_point.h"

#include "chimera_state_threaten_steal.h"

CStateChimeraThreatenSteal::CStateChimeraThreatenSteal(CBaseMonster* object) : inherited(object)
{

}

CStateChimeraThreatenSteal::~CStateChimeraThreatenSteal()
{

}

void CStateChimeraThreatenSteal::initialize()
{
	inherited::initialize();

	data.action.action = ACT_STEAL;

	data.accelerated = true;
	data.braking = false;
	data.accel_type = eAT_Calm;

	data.completion_dist = 2.f;
	data.action.sound_type = MonsterSound::eMonsterSoundIdle;
	data.action.sound_delay = object->db().m_dwIdleSndDelay;
}

void CStateChimeraThreatenSteal::finalize()
{
	inherited::finalize();
}

void CStateChimeraThreatenSteal::execute()
{
	data.point = object->EnemyMan.get_enemy_position();
	data.vertex = object->EnemyMan.get_enemy_vertex();
	data.time_to_rebuild = object->get_attack_rebuild_time();

	inherited::execute();
}

bool CStateChimeraThreatenSteal::check_completion()
{
	if (inherited::check_completion()) return true;

	float dist_to_enemy = object->EnemyMan.get_enemy_position().distance_to(object->Position());
	if (dist_to_enemy < EntityDefinitions::CChimeraBase::MIN_DISTANCE_TO_ENEMY) return true;

	return false;
}

bool CStateChimeraThreatenSteal::check_start_conditions()
{
	float dist_to_enemy = object->EnemyMan.get_enemy_position().distance_to(object->Position());
	if (dist_to_enemy > EntityDefinitions::CChimeraBase::MIN_DISTANCE_TO_ENEMY) return true;
	return false;
}
