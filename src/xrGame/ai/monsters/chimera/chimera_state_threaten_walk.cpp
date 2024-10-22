#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "chimera.h"

#include "../states/state_move_to_point.h"

#include "chimera_state_threaten_walk.h"

CStateChimeraThreatenWalk::CStateChimeraThreatenWalk(CBaseMonster* object) : inherited(object)
{

}

CStateChimeraThreatenWalk::~CStateChimeraThreatenWalk()
{

}

void CStateChimeraThreatenWalk::initialize()
{
	inherited::initialize();

	data.point = object->EnemyMan.get_enemy_position();
	data.vertex = object->EnemyMan.get_enemy_vertex();

	data.action.action = ACT_WALK_FWD;

	data.accelerated = true;
	data.braking = false;
	data.accel_type = eAT_Calm;

	data.completion_dist = 2.f;
	data.action.sound_type = MonsterSound::eMonsterSoundIdle;
	data.action.sound_delay = object->db().m_dwIdleSndDelay;
	data.time_to_rebuild = 1500;
}

void CStateChimeraThreatenWalk::execute()
{
	data.point = object->EnemyMan.get_enemy_position();
	data.vertex = object->EnemyMan.get_enemy_vertex();

	inherited::execute();
}

bool CStateChimeraThreatenWalk::check_completion()
{
	if (inherited::check_completion()) return true;

	float dist_to_enemy = object->EnemyMan.get_enemy_position().distance_to(object->Position());
	if (dist_to_enemy < EntityDefinitions::CChimeraBase::DISTANCE_TO_ENEMY) return true;

	return false;
}

bool CStateChimeraThreatenWalk::check_start_conditions()
{
	float dist_to_enemy = object->EnemyMan.get_enemy_position().distance_to(object->Position());
	if (dist_to_enemy < EntityDefinitions::CChimeraBase::MAX_DISTANCE_TO_ENEMY) return true;
	return false;
}
