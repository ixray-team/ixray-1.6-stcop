#pragma once

#define MIN_UNSEEN_TIME			15000
#define MIN_DIST_TO_ENEMY		15.f


void CStateMonsterPanicRun::initialize()
{
	inherited::initialize();
	
	this->object->path().prepare_builder		();	
}


void CStateMonsterPanicRun::execute()
{
	this->object->set_action							(ACT_RUN);
	this->object->set_state_sound						(MonsterSound::eMonsterSoundPanic);
	this->object->anim().accel_activate			(eAT_Aggressive);
	this->object->anim().accel_set_braking			(false);
	this->object->path().set_retreat_from_point	(this->object->EnemyMan.get_enemy_position());
	this->object->path().set_generic_parameters	();
}

bool CStateMonsterPanicRun::check_completion()
{
	float dist_to_enemy = this->object->Position().distance_to(this->object->EnemyMan.get_enemy_position());
	u32 time_delta	= Device.dwTimeGlobal - this->object->EnemyMan.get_enemy_time_last_seen();

	if (dist_to_enemy < MIN_DIST_TO_ENEMY)  return false;
	if (time_delta	  < MIN_UNSEEN_TIME)	return false;

	return true;
}

#undef DIST_TO_PATH_END
#undef MIN_DIST_TO_ENEMY
