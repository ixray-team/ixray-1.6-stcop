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

// Связывает поиск противника у последней известной позиции с необходимыми службами и состоянием владельца.
CELGroundSearchBehavior::CELGroundSearchBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELGroundBehaviorBase(OwnerAdapter, OwnerProfile), Inspection(OwnerAdapter.Monster()) {}

// Завершает активную схему и освобождает временные заявки.
void CELGroundSearchBehavior::Exit()
{
	Inspection.Clear();
	CELGroundBehaviorBase::Exit();
}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundSearchBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	SearchUntil = 0;
	TravelUntil = Device.dwTimeGlobal + Profile.ApproachMax;
	HasReachedAnchor = false;
	SearchAnchor = Observation.Position;
	ActionUntil = TravelUntil;
	NextPoint = 0;
	SearchPhase = 0;
}

// Обновляет поиск противника у последней известной позиции и обрабатывает текущий результат выполнения.
void CELGroundSearchBehavior::Update(float DeltaTime)
{
	if (SearchAnchor.distance_to_sqr(Observation.Position) >= _sqr(CELSharedSettings::Work().GroundRouteReplanDistance))
	{
		SearchAnchor = Observation.Position;
		HasReachedAnchor = false;
		SearchUntil = 0;
		TravelUntil = Device.dwTimeGlobal + Profile.ApproachMax;
		ActionUntil = TravelUntil;
		NextPoint = SearchPhase = 0;
		Inspection.Clear();
		InvalidateRoute();
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
			Move(SearchAnchor, Observation.VertexIdentifier, true);
			if (HasMoveCommand)
			{
				Validation = {EELBehaviorState::Running, "Поиск: бег к последней известной точке"};
			}
			return;
		}
		HasReachedAnchor = true;
		SearchUntil = Device.dwTimeGlobal + Random.randI(Profile.SearchMin, Profile.SearchMax + 1);
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
		if (Observation.Trail.size() >= 2)
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
