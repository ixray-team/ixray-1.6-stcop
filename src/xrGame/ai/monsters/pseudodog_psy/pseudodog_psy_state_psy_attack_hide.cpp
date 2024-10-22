#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "sound_player.h"

#include "../../../ai_space.h"
#include "../monster_cover_manager.h"
#include "../../../cover_point.h"

#include "pseudodog_psy_state_psy_attack_hide.h"

CStatePsyDogHide::CStatePsyDogHide(CBaseMonster* object) : inherited(object) 
{

}

CStatePsyDogHide::~CStatePsyDogHide()
{

}

void CStatePsyDogHide::initialize()
{
	inherited::initialize();

	select_target_point();
	object->path().prepare_builder();
}

void CStatePsyDogHide::execute()
{
	object->set_action(ACT_RUN);
	object->path().set_target_point(target.position, target.node);
	object->path().set_rebuild_time(0);
	object->path().set_distance_to_end(0);
	object->path().set_use_covers(false);

	object->anim().accel_activate(eAT_Aggressive);
	object->anim().accel_set_braking(false);

	object->sound().play(MonsterSound::eMonsterSoundAggressive, 0, 0, object->db().m_dwAttackSndDelay);
}

bool CStatePsyDogHide::check_start_conditions()
{
	return true;
}

bool CStatePsyDogHide::check_completion()
{
	return ((object->ai_location().level_vertex_id() == target.node) && !object->control().path_builder().is_moving_on_path());
}

void CStatePsyDogHide::select_target_point()
{
	const CCoverPoint* point = object->CoverMan->find_cover(object->EnemyMan.get_enemy_position(), 10.f, 30.f);
	if (point && (object->Position().distance_to(point->position()) > 2.f)) {
		target.node = point->level_vertex_id();
		target.position = point->position();
	}
	else {
		const CCoverPoint* point = object->CoverMan->find_cover(object->Position(), 10.f, 30.f);
		if (point && (object->Position().distance_to(point->position()) > 2.f)) {
			target.node = point->level_vertex_id();
			target.position = point->position();
		}
		else {
			target.node = 0;
			target.position = ai().level_graph().vertex_position(target.node);
		}
	}
}