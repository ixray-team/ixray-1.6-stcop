#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>
#define CStateChimeraThreatenStealAbstract CStateChimeraThreatenSteal<_Object>

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenStealAbstract::initialize()
{
	inherited::initialize();
	
	this->data.action.action		= ACT_STEAL;
	
	this->data.accelerated		= true;
	this->data.braking			= false;
	this->data.accel_type 		= eAT_Calm;
	
	this->data.completion_dist	= 2.f;
	this->data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
	this->data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenStealAbstract::finalize()
{
	inherited::finalize();
}


TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenStealAbstract::execute()
{
	this->data.point				= this->object->EnemyMan.get_enemy_position	();
	this->data.vertex				= this->object->EnemyMan.get_enemy_vertex		();
	this->data.time_to_rebuild		= this->object->get_attack_rebuild_time		();

	inherited::execute();
}

#define MIN_DISTANCE_TO_ENEMY	8.f

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenStealAbstract::check_completion()
{	
	if (inherited::check_completion()) return true;
	
	float dist_to_enemy = this->object->EnemyMan.get_enemy_position().distance_to(this->object->Position());
	if (dist_to_enemy < MIN_DISTANCE_TO_ENEMY) return true;

	return false;
}

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenStealAbstract::check_start_conditions()
{
	float dist_to_enemy = this->object->EnemyMan.get_enemy_position().distance_to(this->object->Position());
	if (dist_to_enemy > MIN_DISTANCE_TO_ENEMY) return true;
	return false;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateChimeraThreatenStealAbstract
#undef MIN_DISTANCE_TO_ENEMY
