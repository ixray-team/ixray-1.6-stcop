#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include <cmath>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundSearchBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHuntGroups.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "xrGame/monster_scale.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundLocalSearch.h"

// Связывает поиск противника у последней известной позиции с необходимыми службами и состоянием владельца.
CELGroundSearchBehavior::CELGroundSearchBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELGroundBehaviorBase(OwnerAdapter, OwnerProfile), Inspection(OwnerAdapter.Monster()) {}

// Завершает активную схему и освобождает временные заявки.
void CELGroundSearchBehavior::Exit()
{
	Inspection.Clear();
	CELGroundBehaviorBase::Exit();
}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundSearchBehavior::ResolveAnchor()
{
	Fvector Ground;
	u32 Vertex;
	if (CELGroundLocalSearch::Resolve(Adapter, SearchAnchor, Ground, Vertex))
	{
		SearchAnchor = Ground;
		return;
	}
	if (!ai().get_level_graph()) { return; }
	Vertex = ai().level_graph().vertex_id(SearchAnchor);
	if (ai().level_graph().valid_vertex_id(Vertex))
	{
		const Fvector Nearby = ai().level_graph().vertex_position(Vertex);
		if (Nearby.distance_to_sqr(SearchAnchor) <= _sqr(Profile.SearchRadius) && CELGroundLocalSearch::Resolve(Adapter, Nearby, Ground, Vertex))
		{
			SearchAnchor = Ground;
		}
	}
}

void CELGroundSearchBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	SearchUntil = 0;
	TravelUntil = Device.dwTimeGlobal + Profile.ApproachMax;
	HasReachedAnchor = HasCheckedForward = false;
	AnchorObservationAt = Observation.ObservedAt;
	TotalUntil = TravelUntil + Profile.SearchMax;
	const SELHuntGroup SearchGroup = CELSharedHuntGroups::Observe(Adapter.Monster(), Observation.TargetIdentifier, Profile.SharingRadius);
	SearchAnchor = Observation.Position;
	if (SearchGroup.Rank && Observation.HasObservedVelocity && Adapter.Monster().ExtendedIntelligence > 0.f)
	{
		Fvector Travel = Observation.ObservedVelocity;
		Travel.y = 0.f;
		Travel.normalize_safe();
		Fvector Lane = {Travel.z, 0.f, -Travel.x};
		Fvector Exit = Fvector().mad(SearchAnchor, Travel, Profile.SearchForwardDistance * Adapter.Monster().ExtendedIntelligence);
		Exit.mad(Lane, Profile.ApproachWidth * (SearchGroup.Rank % 2 ? 1.f : -1.f));
		Fvector Point;
		u32 Vertex;
		if (CELGroundLocalSearch::Resolve(Adapter, Exit, Point, Vertex))
		{
			SearchAnchor = Point;
			HasCheckedForward = true;
		}
	}

	ResolveAnchor();
	ActionUntil = TravelUntil;
	NextPoint = 0;
	SearchPhase = 0;
}

// Обновляет поиск противника у последней известной позиции и обрабатывает текущий результат выполнения.
void CELGroundSearchBehavior::Update(float DeltaTime)
{
	if (s32(Observation.ObservedAt - AnchorObservationAt) > 0)
	{
		AnchorObservationAt = Observation.ObservedAt;
		HasCheckedForward = false;
		SearchAnchor = Observation.Position;
		HasReachedAnchor = false;
		SearchUntil = 0;
		TravelUntil = Device.dwTimeGlobal + Profile.ApproachMax;
		TotalUntil = TravelUntil + Profile.SearchMax;
		ActionUntil = TravelUntil;
		ResolveAnchor();
		NextPoint = SearchPhase = 0;
		Inspection.Clear();
		InvalidateRoute();
	}
	if (s32(Device.dwTimeGlobal - TotalUntil) >= 0)
	{
		Movement.Stop();
		Validation = {EELBehaviorState::Completed, "Истёк общий срок поиска"};
		return;
	}
	if (!HasReachedAnchor)
	{
		if (Adapter.Monster().Position().distance_to_sqr(SearchAnchor) >= _sqr(2.f * MonsterScale::Arrival(Adapter.Monster().GetVisualScale())))
		{
			if (s32(Device.dwTimeGlobal - TravelUntil) >= 0)
			{
				Movement.Stop();
				Validation = {EELBehaviorState::Completed, "Поиск: не достиг последней точки за отведённое время"};
				return;
			}
			Move(SearchAnchor, ai().level_graph().vertex_id(SearchAnchor), true);
			if (HasMoveCommand)
			{
				Validation = {EELBehaviorState::Running, "Поиск: бег к последней известной точке"};
			}
			return;
		}
		if (!HasCheckedForward && Observation.HasObservedVelocity && Profile.SearchForwardDistance > 0.f)
		{
			HasCheckedForward = true;
			Fvector Direction = Observation.ObservedVelocity;
			Direction.y = 0.f;
			if (Direction.square_magnitude() > EPS_L)
			{
				Direction.normalize_safe();
				Fvector Ahead = Fvector().mad(Observation.Position, Direction, Profile.SearchForwardDistance);
				Fvector Point;
				u32 Vertex;
				if (CELGroundLocalSearch::Resolve(Adapter, Ahead, Point, Vertex))
				{
					SearchAnchor = Point;
					InvalidateRoute();
					Move(SearchAnchor, Vertex, true);
					Validation = {EELBehaviorState::Running, "Проверяю продолжение следа за преградой"};
					return;
				}
			}
		}
		HasReachedAnchor = true;
		SearchUntil = Device.dwTimeGlobal + std::min(u32(Random.randI(Profile.SearchMin, Profile.SearchMax + 1)), TotalUntil - Device.dwTimeGlobal);
		ActionUntil = SearchUntil;
		InvalidateRoute();
	}
	if (s32(Device.dwTimeGlobal - SearchUntil) >= 0)
	{
		Movement.Stop();
		Validation = {EELBehaviorState::Completed, "Поиск завершён"};
		return;
	}
	if (Inspection.Update(Profile.CanInspect))
	{
		Movement.Stop();
		ProgressTime = Device.dwTimeGlobal;
		HasMoveCommand = false;
		Validation = {EELBehaviorState::Running, "Изучение следа"};
		return;
	}
	if (SearchPhase == 0 || (Device.dwTimeGlobal >= NextPoint))
	{
		Fvector TrailDirection = Adapter.Monster().XFORM().k;
		if (Observation.HasObservedVelocity)
		{
			TrailDirection = Observation.ObservedVelocity;
			TrailDirection.normalize_safe();
		}
		else if (Observation.Trail.size() >= 2)
		{
			TrailDirection.sub(Observation.Trail.back(), Observation.Trail.front()).normalize_safe();
		}
		float TrailHeading;
		float TrailPitch;
		TrailDirection.getHP(TrailHeading, TrailPitch);
		const SELHuntGroup Group = CELSharedHuntGroups::Observe(Adapter.Monster(), Observation.TargetIdentifier, Profile.SharingRadius);
		const float Lane = Group.Count > 1 ? float(Group.Rank) / float(Group.Count - 1) - .5f : 0.f;
		const float Spread = std::min(PI, deg2rad(Profile.SearchAngle) * (1.f + float(SearchPhase / 5) * .5f));
		const float Angle = -TrailHeading + Lane * Spread + (float(SearchPhase % 5) / 4.f - .5f) * Spread;
		const float SearchLimit = Group.Count > 1 ? Profile.SharingRadius * Profile.CallSharingRatio * .8f : Profile.SearchRadius;
		const float Radius = std::min(Profile.SearchRadius * (.35f + float(SearchPhase / 5) * .3f), SearchLimit);
		SearchPosition = Observation.Position;
		SearchPosition.x += std::sin(Angle) * Radius;
		SearchPosition.z += std::cos(Angle) * Radius;
		SearchVertex = ai().level_graph().vertex_id(SearchPosition);
		NextPoint = Device.dwTimeGlobal + Profile.SearchPointTime;
		++SearchPhase;
	}
	if (CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Search;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.Start = Adapter.Monster().Position();
		Event.Finish = SearchPosition;
		Event.Radius = .4f;
		Event.Label = "Поиск у последней известной точки";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	Validation = {EELBehaviorState::Running, "Поиск вокруг точки"};
	const bool Runs = Adapter.Monster().Position().distance_to_sqr(SearchPosition) > _sqr(Profile.SearchRunDistance);
	if (!Move(SearchPosition, SearchVertex, Runs))
	{
		Validation = {EELBehaviorState::Blocked, "Точка поиска недоступна"};
	}
}
