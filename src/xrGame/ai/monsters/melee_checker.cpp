#include "StdAfx.h"
#include "melee_checker.h"
#include "basemonster/base_monster.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../xrEngine/xr_collide_form.h"

#define MAX_TRACE_ENEMY_RANGE	6.f

float CMeleeChecker::distance_to_enemy(const CEntityAlive *enemy)
{
	// The actor is inside the car, so the monster must stop at the car body,
	// not at the actor's hidden position.
	if (m_object->IsEnemyInCar())
		return m_object->GetEnemyAttackPosition().distance_to(m_object->Position());

	float dist = enemy->Position().distance_to	(m_object->Position());
	if (dist > MAX_TRACE_ENEMY_RANGE)			return dist;

	Fvector					enemy_center;
	enemy->Center			(enemy_center);

	Fvector					my_head_pos = get_head_position(m_object);
	
	Fvector					dir; 
	dir.sub					(enemy_center, my_head_pos);
	dir.normalize_safe		();

	collide::ray_defs		r_query	(my_head_pos, dir, MAX_TRACE_ENEMY_RANGE, CDB::OPT_CULL | CDB::OPT_ONLYNEAREST, collide::rqtObject);
	r_res.r_clear			();

	if (m_object->CFORM()->_RayQuery(r_query, r_res)) {
		if (!r_res.r_any().IsStatic() && r_res.r_any().GetDynamic() == enemy)
		{
			dist			= r_res.r_any().range;
		}
	}

	return					(dist);
}

void CMeleeChecker::on_hit_attempt(bool hit_success) 
{
	// добавить новый элемент в стек
	for (u32 i=HIT_STACK_SIZE-1; i > 0; i--) m_hit_stack[i] = m_hit_stack[i-1];
	m_hit_stack[0] = hit_success;

	// проверить однородность стека
	bool stack_similar = true;	
	for (u32 i=1; i < HIT_STACK_SIZE; i++) if (m_hit_stack[i] != hit_success) {stack_similar = false; break;}

	if (!stack_similar) return;

	// обновить m_current_min_distance
	if (hit_success) {
		if (m_current_min_distance + m_as_step < m_min_attack_distance)	m_current_min_distance += m_as_step;
		else m_current_min_distance = m_min_attack_distance;
	} else {
		if (m_current_min_distance > m_as_min_dist + m_as_step) m_current_min_distance -= m_as_step;
		else m_current_min_distance = m_as_min_dist;
	}
}

bool	CMeleeChecker::can_start_melee (const CEntityAlive *enemy)
{
	const bool EnemyInCar = m_object->IsEnemyInCar();

	// A car body hides the actor from the visual check. The car-surface
	// distance is authoritative while the enemy is inside a vehicle.
	if ( !EnemyInCar && !m_object->EnemyMan.see_enemy_now(enemy) )
	{
		return false;
	}

	// The level graph rarely has a vertex right next to the car, so the
	// monster can stop slightly outside the normal melee range. Use the full
	// melee radius against the car surface instead of the tighter melee-start
	// distance; otherwise it circles the car and only attacks by accident.
	const float StartDistance = EnemyInCar ? get_max_distance() : get_min_distance();
	return distance_to_enemy(enemy) < StartDistance;
}

bool	CMeleeChecker::should_stop_melee (const CEntityAlive *enemy)
{
	const float StopDistance = m_object->IsEnemyInCar() ? get_max_distance() + 0.3f : get_max_distance();
	return distance_to_enemy(enemy) > StopDistance;
}

