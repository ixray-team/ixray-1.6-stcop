#include "StdAfx.h"
#include "melee_checker.h"
#include "ai_monster_utils.h"
#include <algorithm>
#include <cmath>
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
	if (m_object->UsesExtendedCombat)
	{
		Fvector Planar = Direction;
		Planar.y = 0.f;
		const float PlanarDistance = Planar.magnitude();
		Planar.normalize_safe();
		Fvector Head;
		m_object->Center(Head);
		if (PKinematics(m_object->Visual())->LL_BoneID(m_object->get_head_bone_name()) != BI_NONE)
		{
			Head = get_head_position(m_object);
		}
		const Fvector HeadOffset = Fvector().sub(Head, m_object->Position());
		const float HeadLead = std::clamp(HeadOffset.dotproduct(Planar), 0.f, m_object->Radius());
		Fvector HalfSize;
		Enemy->BoundingBox().getradius(HalfSize);
		const float BodyRadius = std::abs(Planar.dotproduct(Enemy->XFORM().i)) * HalfSize.x + std::abs(Planar.dotproduct(Enemy->XFORM().k)) * HalfSize.z;
		const float Gap = std::max(0.f, PlanarDistance - HeadLead - std::min(BodyRadius, m_object->ExtendedMeleeHitTolerance));
		const float OwnerBottom = m_object->Position().y + m_object->BoundingBox().min.y;
		const float OwnerTop = m_object->Position().y + m_object->BoundingBox().max.y;
		const float TargetBottom = Enemy->Position().y + Enemy->BoundingBox().min.y;
		const float TargetTop = Enemy->Position().y + Enemy->BoundingBox().max.y;
		const float HeightGap = std::max({0.f, TargetBottom - OwnerTop, OwnerBottom - TargetTop});
		return std::sqrt(Gap * Gap + HeightGap * HeightGap);
	}
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

// Only a currently perceived opponent supplies live motion for melee correction.
Fvector CMeleeChecker::TargetVelocity(const CEntityAlive* Enemy) const
{
	Fvector Velocity = {0.f, 0.f, 0.f};
	if (Enemy && m_object->UsesExtendedCombat && m_object->EnemyMan.see_enemy_now(Enemy))
	{
		const_cast<CEntityAlive*>(Enemy)->PHGetLinearVell(Velocity);
		Velocity.y = 0.f;
		if (!_valid(Velocity.x) || !_valid(Velocity.z) || Velocity.square_magnitude() > 400.f)
		{
			Velocity.set(0.f, 0.f, 0.f);
		}
	}
	return Velocity;
}

Fvector CMeleeChecker::PredictTargetPosition(const CEntityAlive* Enemy, float Horizon) const
{
	Fvector Lead = TargetVelocity(Enemy);
	Lead.mul(clampr(Horizon, 0.f, m_object->ExtendedMeleePredictionTime));
	const float Length = Lead.magnitude();
	if (Length > m_object->ExtendedMeleePredictionDistance && Length > EPS_L)
	{
		Lead.mul(m_object->ExtendedMeleePredictionDistance / Length);
	}
	return Fvector().add(Enemy->Position(), Lead);
}

float CMeleeChecker::distance_to_enemy(const CEntityAlive *enemy)
{
	if (m_object->UsesExtendedCombat || m_object->GetVisualScale() != 1.f || enemy->GetVisualScale() != 1.f)
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
	const float Tolerance = m_object->UsesExtendedCombat ? m_object->ExtendedMeleeHitTolerance : 0.f;
	const float Distance = distance_to_enemy(enemy);
	const float StartRange = m_object->UsesExtendedCombat && m_object->NativeHitAttemptAt && !m_object->NativeHitAttemptSuccess && m_object->NativeHitRange > EPS_L ?
		std::min(get_min_distance() + Tolerance, m_object->NativeHitRange + Tolerance) : get_min_distance() + Tolerance;
	const bool Allowed = Distance < StartRange;
	m_object->NativeMeleeReason = Allowed ? "start_allowed" : "outside_start_distance";
	return Allowed;
}

bool	CMeleeChecker::should_stop_melee (const CEntityAlive *enemy)
{
	const float Tolerance = m_object->UsesExtendedCombat ? m_object->ExtendedMeleeHitTolerance : 0.f;
	const float ContinueRange = m_object->UsesExtendedCombat && m_object->NativeHitAttemptAt && !m_object->NativeHitAttemptSuccess && m_object->NativeHitRange > EPS_L ?
		std::min(get_max_distance() + Tolerance, m_object->NativeHitRange + Tolerance) : get_max_distance() + Tolerance;
	return distance_to_enemy(enemy) > ContinueRange;
}

