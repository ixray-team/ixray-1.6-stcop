#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "monster_state_home_point_rest.h"


#include "../monster_home.h"


void CStateMonsterRestMoveToHomePoint::initialize()
{
	inherited::initialize();
	m_target_node = object->Home->get_place_in_mid_home();
}


void CStateMonsterRestMoveToHomePoint::execute()
{
	object->path().set_target_point(ai().level_graph().vertex_position(m_target_node), m_target_node);
	object->anim().accel_activate(EAccelType(object->Home->is_aggressive() ? eAT_Aggressive : eAT_Calm));
	object->anim().accel_set_braking(true);
	object->path().set_rebuild_time(0);
	object->path().set_distance_to_end(0.f);
	object->path().set_use_covers(false);

	object->set_action(object->Home->is_aggressive() ? ACT_RUN : ACT_WALK_FWD);
	object->set_state_sound(object->Home->is_aggressive() ? MonsterSound::eMonsterSoundAggressive : MonsterSound::eMonsterSoundIdle);
}


bool CStateMonsterRestMoveToHomePoint::check_start_conditions()
{
	return (!object->Home->at_mid_home(object->Position()));
}


bool CStateMonsterRestMoveToHomePoint::check_completion()
{
	return ((object->ai_location().level_vertex_id() == m_target_node) && !object->control().path_builder().is_moving_on_path());
}
