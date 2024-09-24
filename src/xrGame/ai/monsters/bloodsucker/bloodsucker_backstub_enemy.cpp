#include "stdafx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "bloodsucker.h"
#include "bloodsucker_attack_state.h"
#include "bloodsucker_vampire_execute.h"
#include "bloodsucker_backstub_enemy.h"

#include "../states/state_move_to_point.h"

CustomBloodsuckerBackstubEnemy::CustomBloodsuckerBackstubEnemy(CustomBloodsucker* object) : inherited(object, &data)
{

};

CustomBloodsuckerBackstubEnemy::~CustomBloodsuckerBackstubEnemy()
{

}

void   CustomBloodsuckerBackstubEnemy::initialize()
{
	inherited::initialize();
	this->object->path().prepare_builder();
	m_last_health = this->object->conditions().GetHealth();
	m_encircle = data.start_with_encircle;
	m_encircle_end_tick = Device.dwTimeGlobal + SBloodsuckerStateBackstubEnemyProperies::encircle_time;
	m_next_change_behaviour_tick = 0;
}

void   CustomBloodsuckerBackstubEnemy::execute()
{
	// on hit, change behaviour
	if (this->object->conditions().GetHealth() < m_last_health - SBloodsuckerStateBackstubEnemyProperies::loose_health_diff &&
		Device.dwTimeGlobal > m_next_change_behaviour_tick)
	{
		m_next_change_behaviour_tick = Device.dwTimeGlobal + SBloodsuckerStateBackstubEnemyProperies::change_behaviour_time;
		m_last_health = this->object->conditions().GetHealth();
		m_encircle = !m_encircle;
		if (m_encircle)
		{
			m_encircle_end_tick = Device.dwTimeGlobal + SBloodsuckerStateBackstubEnemyProperies::encircle_time;
		}
	}

	if (Device.dwTimeGlobal > m_encircle_end_tick)
	{
		if (this->object->EnemyMan.enemy_see_me_now())
		{
			m_encircle = false;
		}
	}

	this->object->set_action(data.action.action);
	this->object->anim().SetSpecParams(data.action.spec_params);

	data.point = this->object->EnemyMan.get_enemy_position();
	data.vertex = 0;

	data.target_direction = Fvector().set(0.f, 0.f, 0.f);
	const CEntityAlive* enemy = this->object->EnemyMan.get_enemy();
	VERIFY(enemy);

	const SRotation rot = enemy->Orientation();
	data.target_direction.setHP(rot.yaw, rot.pitch);

	this->object->path().set_target_point(data.point, data.vertex);
	this->object->path().set_rebuild_time(data.time_to_rebuild);
	this->object->path().set_distance_to_end(data.completion_dist);
	this->object->path().set_use_covers();
	this->object->path().set_cover_params(5.f, 30.f, 1.f, 30.f);

	if (m_encircle)
	{
		this->object->path().set_use_dest_orient(true);
		this->object->path().set_dest_direction(data.target_direction);
		this->object->path().set_try_min_time(false);
	}
	else
	{
		this->object->path().set_try_min_time(true);
		this->object->path().set_use_dest_orient(false);
	}

	if (data.accelerated)
	{
		this->object->anim().accel_activate(EAccelType(data.accel_type));
		this->object->anim().accel_set_braking(data.braking);
	}

	if (data.action.sound_type != u32(-1))
	{
		this->object->set_state_sound(data.action.sound_type, data.action.sound_delay == u32(-1));
	}
}

bool   CustomBloodsuckerBackstubEnemy::check_start_conditions()
{
	if (!this->object->Home->at_home(this->object->EnemyMan.get_enemy_position()))
	{
		return false;
	}

	float dist = this->object->MeleeChecker.distance_to_enemy(this->object->EnemyMan.get_enemy());

	return dist > this->object->MeleeChecker.get_min_distance();
}

bool   CustomBloodsuckerBackstubEnemy::check_completion()
{
	if (!this->object->Home->at_home(this->object->EnemyMan.get_enemy_position()))
	{
		return true;
	}

	const bool real_path_end = fis_zero(data.completion_dist) ?
		(data.point.distance_to_xz(this->object->Position()) < ai().level_graph().header().cell_size()) : true;

	if (this->object->control().path_builder().is_path_end(data.completion_dist) && real_path_end)
	{
		// in straight-mode we're done
		if (!m_encircle)
		{
			return true;
		}

		if (this->object->EnemyMan.see_enemy_now() && !this->object->EnemyMan.enemy_see_me_now())
		{
			return true;
		}
	}

	return false;
}
