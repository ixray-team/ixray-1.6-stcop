#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>
#define CStateChimeraThreatenWalkAbstract CStateChimeraThreatenWalk<_Object>

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenWalkAbstract::initialize()
{
	inherited::initialize();

	this->data.point				= this->object->EnemyMan.get_enemy_position	();
	this->data.vertex				= this->object->EnemyMan.get_enemy_vertex		();

	this->data.action.action		= ACT_WALK_FWD;

	this->data.accelerated		= true;
	this->data.braking			= false;
	this->data.accel_type 		= eAT_Calm;

	this->data.completion_dist	= 2.f;
	this->data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
	this->data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
	this->data.time_to_rebuild	= 1500;
}


TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenWalkAbstract::execute()
{
	this->data.point				= this->object->EnemyMan.get_enemy_position	();
	this->data.vertex				= this->object->EnemyMan.get_enemy_vertex		();

	inherited::execute();
}

#define DISTANCE_TO_ENEMY		5.f

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenWalkAbstract::check_completion()
{	
	if (inherited::check_completion()) return true;

	float dist_to_enemy = this->object->EnemyMan.get_enemy_position().distance_to(this->object->Position());
	if (dist_to_enemy < DISTANCE_TO_ENEMY) return true;

	return false;
}

#define MAX_DISTANCE_TO_ENEMY	8.f

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenWalkAbstract::check_start_conditions()
{
	float dist_to_enemy = this->object->EnemyMan.get_enemy_position().distance_to(this->object->Position());
	if (dist_to_enemy < MAX_DISTANCE_TO_ENEMY) return true;
	return false;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateChimeraThreatenWalkAbstract

