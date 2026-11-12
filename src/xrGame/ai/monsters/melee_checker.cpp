#include "StdAfx.h"
#include "melee_checker.h"
#include "../../monster_scale.h"
#include "basemonster/base_monster.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../xrEngine/xr_collide_form.h"

#define MAX_TRACE_ENEMY_RANGE	6.f

float CMeleeChecker::get_min_distance()
{
	return m_current_min_distance * m_object->GetVisualScale();
}

float CMeleeChecker::get_max_distance()
{
	return (m_max_attack_distance - (m_min_attack_distance - m_current_min_distance)) * m_object->GetVisualScale();
}

float CMeleeChecker::TargetDistance(const CEntityAlive* Enemy) const
{
	Fvector Direction;
	Direction.sub(Enemy->Position(), m_object->Position());
	const float Distance = Direction.magnitude();
	const float OwnerScale = m_object->GetVisualScale();
	const float TargetScale = Enemy->GetVisualScale();
	if (OwnerScale == TargetScale)
	{
		return Distance;
	}
	Direction.y = 0.f;
	Direction.normalize_safe();
	Fvector HalfSize;
	Enemy->BoundingBox().getradius(HalfSize);
	const Fmatrix& TargetTransform = Enemy->XFORM();
	const float TargetRadius = std::abs(Direction.dotproduct(TargetTransform.i)) * HalfSize.x +
		std::abs(Direction.dotproduct(TargetTransform.k)) * HalfSize.z;
	return MonsterScale::MeleeDistance(Distance, OwnerScale, TargetScale, TargetRadius);
}

float CMeleeChecker::distance_to_enemy(const CEntityAlive *enemy)
{
	if (m_object->GetVisualScale() != 1.f || enemy->GetVisualScale() != 1.f)
	{
		return TargetDistance(enemy);
	}
	float dist = enemy->Position().distance_to	(m_object->Position());
	const float TraceRange = MAX_TRACE_ENEMY_RANGE * m_object->GetVisualScale();
	if (dist > TraceRange)			return dist;

	Fvector					enemy_center;
	enemy->Center			(enemy_center);

	Fvector					my_head_pos = get_head_position(m_object);
	
	Fvector					dir; 
	dir.sub					(enemy_center, my_head_pos);
	dir.normalize_safe		();

	collide::ray_defs		r_query	(my_head_pos, dir, TraceRange, CDB::OPT_CULL | CDB::OPT_ONLYNEAREST, collide::rqtObject);
	r_res.r_clear			();

	if (m_object->CFORM()->_RayQuery(r_query, r_res)) {
		if (r_res.r_begin()->O == enemy)
			dist			= r_res.r_begin()->range;
	}

	return					(dist);
}

void CMeleeChecker::on_hit_attempt(bool hit_success) 
{
	m_object->NativeHitAttemptAt = Device.dwTimeGlobal;
	m_object->NativeHitAttemptSuccess = hit_success;
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
	m_object->NativeMeleeCheckedAt = Device.dwTimeGlobal;
	if (!m_object->EnemyMan.see_enemy_now(enemy))
	{
		m_object->NativeMeleeReason = "no_native_vision";
		return false;
	}
	const bool Allowed = distance_to_enemy(enemy) < get_min_distance();
	m_object->NativeMeleeReason = Allowed ? "start_allowed" : "outside_start_distance";
	return Allowed;
}

bool	CMeleeChecker::should_stop_melee (const CEntityAlive *enemy)
{
	return distance_to_enemy(enemy) > get_max_distance();
}

