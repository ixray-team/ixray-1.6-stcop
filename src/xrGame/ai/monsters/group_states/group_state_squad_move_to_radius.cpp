#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "../monster_home.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "group_state_squad_move_to_radius.h"

CStateGroupSquadMoveToRadius::CStateGroupSquadMoveToRadius(CBaseMonster* object) : inherited(object, &data)
{

}

CStateGroupSquadMoveToRadius::~CStateGroupSquadMoveToRadius()
{

}

void CStateGroupSquadMoveToRadius::initialize()
{
	inherited::initialize();
	object->path().prepare_builder();
}

void CStateGroupSquadMoveToRadius::execute()
{
	Fvector m_enemy_position = object->EnemyMan.get_enemy()->Position();

	Fvector to_direction = object->Position();
	to_direction.sub(m_enemy_position);
	to_direction.normalize_safe();

	data.point.x = m_enemy_position.x + data.completion_dist * to_direction.x;
	data.point.y = m_enemy_position.y + data.completion_dist * to_direction.y;
	data.point.z = m_enemy_position.z + data.completion_dist * to_direction.z;
	if (!ai().level_graph().valid_vertex_position(data.point))
	{
		data.point = object->EnemyMan.get_enemy()->Position();
	}

	object->set_action(data.action.action);
	object->anim().SetSpecParams(data.action.spec_params);

	object->path().set_target_point(data.point, data.vertex);
	object->path().set_rebuild_time(data.time_to_rebuild);
	object->path().set_distance_to_end(1.f);
	object->path().set_use_covers();
	object->path().set_cover_params(5.f, 30.f, 1.f, 30.f);

	if (data.accelerated) {
		object->anim().accel_activate(EAccelType(data.accel_type));
		object->anim().accel_set_braking(data.braking);
	}

	if (data.action.sound_type != u32(-1)) {
		object->set_state_sound(data.action.sound_type, data.action.sound_delay == u32(-1));
	}
}

bool CStateGroupSquadMoveToRadius::check_completion()
{
	if (data.action.time_out != 0) {
		if (time_state_started + data.action.time_out < Device.dwTimeGlobal) return true;
	}

	if (data.point.distance_to_xz(object->Position()) <= 1.f) return true;

	return false;
}