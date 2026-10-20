#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include <cmath>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundSearchBehavior.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai_space.h"
#include "level_graph.h"

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
	SearchUntil = Device.dwTimeGlobal + Random.randI(Profile.SearchMin, Profile.SearchMax + 1);
	ActionUntil = SearchUntil;
	NextPoint = 0;
	SearchPhase = 0;
}

// Обновляет поиск противника у последней известной позиции и обрабатывает текущий результат выполнения.
void CELGroundSearchBehavior::Update(float DeltaTime)
{
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
		const float Angle = -TrailHeading + (float(SearchPhase % 5) / 4.f - .5f) * deg2rad(Profile.SearchAngle);
		SearchPosition = Observation.Position;
		SearchPosition.x += std::sin(Angle) * Profile.SearchRadius;
		SearchPosition.z += std::cos(Angle) * Profile.SearchRadius;
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
	if (!Move(SearchPosition, SearchVertex, false))
	{
		Validation = {EELBehaviorState::Blocked, "Точка поиска недоступна"};
	}
}
