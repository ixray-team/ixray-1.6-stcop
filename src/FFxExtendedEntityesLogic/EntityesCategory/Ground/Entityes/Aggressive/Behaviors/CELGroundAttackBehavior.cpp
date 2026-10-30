#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundAttackBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"
#include "xrGame/monster_scale.h"

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundAttackBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	Approach.Reset();
	HasExecutedTacticalMove = false;
	HadVision = false;
	DeadlineTarget = u16(-1);
}

void CELGroundAttackBehavior::Exit()
{
	HasExecutedTacticalMove = false;
	CELGroundBehaviorBase::Exit();
}

// Обновляет наземный подход к противнику и обрабатывает текущий результат выполнения.
void CELGroundAttackBehavior::Update(float DeltaTime)
{
	HasExecutedTacticalMove = false;
	TacticalMoveReason = "нет запроса уворота";
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
	if (HasVision && !HadVision)
	{
		Approach.Reset();
		// Reacquisition may invalidate a stale goal, but tiny visibility fluctuations must not churn path requests.
		if (HasRequestedGoal && RequestedGoal.distance_to_sqr(GoalPosition) >= _sqr(CELSharedSettings::Work().GroundRouteReplanDistance))
		{
			InvalidateRoute();
		}
	}
	HadVision = HasVision;
	Validation = {EELBehaviorState::Running, HasVision ? "Подход к видимой цели" : "К последней известной точке"};
	UpdateDeadline();
	ActionUntil = ApproachDeadline;
	if (s32(Device.dwTimeGlobal - ApproachDeadline) >= 0)
	{
		if (!HasVision && !Profile.CanTrackLive)
		{
			Validation = {EELBehaviorState::Completed, "Истёк срок сближения по старому следу"};
			Movement.Stop();
			return;
		}
		// A live threat must never remain in a completed behavior that issues Stop forever.
		// Start another bounded planning window; do not extend stale, unseen pursuit indefinitely.
		InvalidateRoute();
		Approach.Reset();
		ApproachWindowStartedAt = Device.dwTimeGlobal;
		ApproachDeadline = ApproachWindowStartedAt + Profile.ApproachMax;
		HasRouteEstimate = false;
		ActionUntil = ApproachDeadline;
	}
	if (HasTacticalGoal)
	{
		bool StepAccepted = CELGroundLocalSearch::MoveDirect(Adapter, Movement, TacticalGoal, true, &TacticalMoveReason);
		if (!StepAccepted && HasVision && !ForceDirect)
		{
			Fvector Forward = Fvector().sub(GoalPosition, Adapter.Monster().Position());
			Forward.y = 0.f;
			Forward.normalize_safe();
			const Fvector Offset = Fvector().sub(TacticalGoal, Adapter.Monster().Position());
			Fvector Opposite = Fvector().mad(Adapter.Monster().Position(), Forward, 2.f * Offset.dotproduct(Forward));
			Opposite.sub(Offset);
			Opposite.y = TacticalGoal.y;
			StepAccepted = CELGroundLocalSearch::MoveDirect(Adapter, Movement, Opposite, true, &TacticalMoveReason);
		}
		if (StepAccepted)
		{
			// Do not resume an obsolete approach segment after completing the lateral step.
			if (Route.HasPath || HasRequestedGoal || Navigation.IsPending())
			{
				InvalidateRoute();
			}
			HasExecutedTacticalMove = HasMoveCommand = true;
			TrackProgress();
			Validation = {EELBehaviorState::Running, "Боевой манёвр: уклон и сближение"};
			return;
		}
	}
	Approach.Select(Adapter, Profile, GoalPosition, HasVision, TargetEntity, ForceDirect);

	if (CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Route;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.TargetIdentifier = Observation.TargetIdentifier;
		Event.Start = Adapter.Monster().Position();
		Event.Finish = Approach.Position();
		string256 Label;
		xr_sprintf(Label, "БОЕВОЕ СБЛИЖЕНИЕ: %s | %s | %s | роль %u/%u", Approach.Description(), Approach.Runs() ? "БЕГ" : "ШАГ", HasVision ? "цель видна" : "последняя известная точка", Approach.Rank() + 1, Approach.Count());
		Event.Label = Label;
		CELSharedDebug::Instance().PublishEvent(std::move(Event), 250);
	}
	Validation = {EELBehaviorState::Running, Approach.Description()};
	if (!Move(Approach.Position(), Approach.Vertex(), Approach.Runs()) && Validation.State != EELBehaviorState::Blocked)
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
		ApproachWindowStartedAt = Device.dwTimeGlobal;
		ApproachDeadline = ApproachWindowStartedAt + Profile.ApproachMax;
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
	float Duration = RouteLength / (Profile.ApproachSpeed * MonsterScale::Movement(Adapter.Monster().GetVisualScale())) * 2000.f;
	clamp(Duration, float(Profile.ApproachMin), float(Profile.ApproachMax));
	const u32 Remaining = Profile.ApproachMax - std::min(Profile.ApproachMax, Device.dwTimeGlobal - ApproachWindowStartedAt);
	ApproachDeadline = Device.dwTimeGlobal + std::min(Remaining, u32(Duration));
	HasRouteEstimate = true;
}
