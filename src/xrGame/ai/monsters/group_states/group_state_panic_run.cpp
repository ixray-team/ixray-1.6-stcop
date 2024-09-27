#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "../monster_home.h"

#include "../basemonster/base_monster.h"

#include "group_state_panic_run.h"

#define MIN_UNSEEN_TIME			15000
#define MIN_DIST_TO_ENEMY		15.f

CStateGroupPanicRun::CStateGroupPanicRun(CBaseMonster* object) : inherited(object)
{

}

CStateGroupPanicRun::~CStateGroupPanicRun()
{

}

void CStateGroupPanicRun::initialize()
{
	inherited::initialize();

	object->path().prepare_builder();
}


void CStateGroupPanicRun::execute()
{
	object->set_action(ACT_RUN);
	object->set_state_sound(MonsterSound::eMonsterSoundPanic);
	object->anim().accel_activate(eAT_Aggressive);
	object->anim().accel_set_braking(false);

	Fvector enemy2home = object->Home->get_home_point();
	enemy2home.sub(object->EnemyMan.get_enemy_position());
	enemy2home.normalize_safe();

	object->path().set_target_point(object->Home->get_place_in_max_home_to_direction(enemy2home));

	object->path().set_generic_parameters();
}

bool CStateGroupPanicRun::check_completion()
{
	float dist_to_enemy = object->Position().distance_to(object->EnemyMan.get_enemy_position());
	u32 time_delta = Device.dwTimeGlobal - object->EnemyMan.get_enemy_time_last_seen();

	if (dist_to_enemy < MIN_DIST_TO_ENEMY)  return false;
	if (time_delta < MIN_UNSEEN_TIME)	return false;

	return true;
}

#undef DIST_TO_PATH_END
#undef MIN_DIST_TO_ENEMY

