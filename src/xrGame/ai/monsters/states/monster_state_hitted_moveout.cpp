#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "monster_state_hitted_moveout.h"

#define DIST_TO_PATH_END	1.5f
#define DIST_TO_HIT_POINT	3.f


void CStateMonsterHittedMoveOut::initialize()
{
	inherited::initialize();
	select_target();
	object->path().prepare_builder();
}


void CStateMonsterHittedMoveOut::execute()
{
	// проверить на завершение пути
	if (object->control().path_builder().detail().time_path_built() > time_state_started) {
		if (object->control().path_builder().is_path_end(DIST_TO_PATH_END))
			select_target();
	}

	if (target.node != u32(-1))
		object->path().set_target_point(target.position, target.node);
	else
		object->path().set_target_point(object->HitMemory.get_last_hit_position());

	float dist = object->HitMemory.get_last_hit_position().distance_to(object->Position());

	if (dist > 10.f) object->set_action(ACT_WALK_FWD);
	else object->set_action(ACT_STEAL);

	object->anim().accel_deactivate();
	object->anim().accel_set_braking(false);
	object->set_state_sound(MonsterSound::eMonsterSoundIdle);
}


bool CStateMonsterHittedMoveOut::check_completion()
{
	if (object->HitMemory.get_last_hit_time() > time_state_started) return true;

	float dist = object->HitMemory.get_last_hit_position().distance_to(object->Position());
	if (dist < DIST_TO_HIT_POINT) return true;

	return false;
}


void CStateMonsterHittedMoveOut::select_target()
{
	if (!object->GetCoverCloseToPoint(object->HitMemory.get_last_hit_position(), 10.f, 20.f, 0.f, 15.f, target.position, target.node)) {
		target.node = u32(-1);
	}
}
