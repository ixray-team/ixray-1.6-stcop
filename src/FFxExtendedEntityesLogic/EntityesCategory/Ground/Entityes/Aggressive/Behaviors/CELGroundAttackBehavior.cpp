#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundAttackBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundAttackBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	Approach.Reset();
	DeadlineTarget = u16(-1);
}

// Обновляет наземный подход к противнику и обрабатывает текущий результат выполнения.
void CELGroundAttackBehavior::Update(float DeltaTime)
{
	CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
	if (!TargetEntity || !Adapter.IsEnemy(*TargetEntity))
	{
		Validation = {EELBehaviorState::InvalidTarget, "Цель потеряна"};
		return;
	}
	Fvector GoalPosition = Observation.Position;
	u32 GoalVertex = Observation.VertexIdentifier;
	const bool HasVision = Adapter.CanSee(*TargetEntity);
	if (HasVision || Profile.CanTrackLive)
	{
		TargetEntity->Center(GoalPosition);
		GoalVertex = ai().level_graph().vertex_id(GoalPosition);
	}
	Validation = {EELBehaviorState::Running, HasVision ? "Подход к видимой цели" : "К последней известной точке"};
	UpdateDeadline();
	ActionUntil = ApproachDeadline;
	if (s32(Device.dwTimeGlobal - ApproachDeadline) >= 0)
	{
		Validation = {EELBehaviorState::Completed, "Истёк срок сближения"};
		Movement.Stop();
		return;
	}
	Approach.Select(Adapter, Profile, GoalPosition, HasVision);

	Validation = {EELBehaviorState::Running, Approach.Description()};
	if (!Move(Approach.Position(), Approach.Vertex(), Approach.Runs()))
	{
		Validation = {EELBehaviorState::Blocked, "Недоступная точка"};
	}
}

// Оценивает допустимое время движения по длине подготовленного маршрута.
void CELGroundAttackBehavior::UpdateDeadline()
{
	if (DeadlineTarget != Observation.TargetIdentifier)
	{
		DeadlineTarget = Observation.TargetIdentifier;
		ApproachDeadline = StartedAt + Profile.ApproachMax;
		HasRouteEstimate = false;
	}
	if (!Route.HasPath || HasRouteEstimate)
	{
		return;
	}
	float RouteLength = 0.f;
	u32 PointIndex;
	for (PointIndex = 1; PointIndex < Route.Points.size(); ++PointIndex)
	{
		RouteLength += Route.Points[PointIndex - 1].distance_to(Route.Points[PointIndex]);
	}
	float Duration = RouteLength / Profile.ApproachSpeed * 2000.f;
	clamp(Duration, float(Profile.ApproachMin), float(Profile.ApproachMax));
	const u32 Remaining = Profile.ApproachMax - std::min(Profile.ApproachMax, Device.dwTimeGlobal - StartedAt);
	ApproachDeadline = Device.dwTimeGlobal + std::min(Remaining, u32(Duration));
	HasRouteEstimate = true;
}
