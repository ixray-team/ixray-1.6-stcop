#include "stdafx.h"
#include <cmath>
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "ai/ExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "Level.h"

namespace ExtendedEntityesSight
{
struct SQuery
{
	CBaseMonster* Observer = nullptr;
	CObject* Target = nullptr;
	bool IsVisible = true;
};

// Учитывает прозрачность материала очередного попадания луча зрения.
bool Inspect(collide::rq_result& Hit, LPVOID Context)
{
	SQuery& Query = *static_cast<SQuery*>(Context);
	if (!Hit.O && Query.Observer->feel_vision_mtl_transp(nullptr, Hit.element) >= 1.f)
	{
		return true;
	}
	Query.IsVisible = Hit.O == Query.Target;
	return false;
}
} // namespace ExtendedEntityesSight

// Проверяет видимость цели, отличая ожидание бюджета от закрытого обзора.
EELSightResult CELSharedSight::Check(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius)
{
	return CheckFrom(OwnerMonster, Candidate, Angle, Radius, OwnerMonster.eye_matrix.c);
}

// Проверяет обзор из заданной точки той же нативной трассировкой и общей квотой.
EELSightResult CELSharedSight::CheckFrom(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, const Fvector& StartPosition)
{
	CEntityAlive* TargetEntity = Candidate.cast_entity_alive();
	if (TargetEntity && ExtendedEntityes::IsIgnored(*TargetEntity))
	{
		return EELSightResult::Occluded;
	}
	Fvector TargetPosition;
	Candidate.Center(TargetPosition);
	Fvector Direction;
	Direction.sub(TargetPosition, StartPosition);
	const float Distance = Direction.magnitude();
	if (Distance <= EPS_L || Distance > Radius)
	{
		return EELSightResult::Occluded;
	}
	Direction.div(Distance);
	if (Angle < 360.f && OwnerMonster.Direction().dotproduct(Direction) < std::cos(deg2rad(Angle * .5f)))
	{
		return EELSightResult::Occluded;
	}
	if (!ExtendedEntityes::PerceptionBudget().TryRay())
	{
		return EELSightResult::WaitingForBudget;
	}
	ExtendedEntityesSight::SQuery Query;
	Query.Observer = &OwnerMonster;
	Query.Target = &Candidate;
	collide::rq_results Results;
	collide::ray_defs Ray(StartPosition, Direction, Distance, CDB::OPT_FULL_TEST, collide::rqtBoth);
	Level().ObjectSpace.RayQuery(Results, Ray, ExtendedEntityesSight::Inspect, &Query, nullptr, &OwnerMonster);
	return Query.IsVisible ? EELSightResult::Visible : EELSightResult::Occluded;
}

// Сообщает только подтверждённую доступной проверкой видимость цели.
bool CELSharedSight::Visible(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius)
{
	return Check(OwnerMonster, Candidate, Angle, Radius) == EELSightResult::Visible;
}
