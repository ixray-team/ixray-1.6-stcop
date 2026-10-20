#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBodyBehavior.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/monsters/control_direction_base.h"
#include "ai_monster_space.h"

// Связывает исследование известного тела с необходимыми службами и состоянием владельца.
CELGroundBodyBehavior::CELGroundBodyBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELGroundBehaviorBase(OwnerAdapter, OwnerProfile), Inspection(OwnerAdapter.Monster()) {}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundBodyBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	InspectUntil = 0;
	ActionUntil = StartedAt + Profile.SearchMax + Profile.BodyInspectMax;
	HasReachedBody = false;
}

// Обновляет исследование известного тела и обрабатывает текущий результат выполнения.
void CELGroundBodyBehavior::Update(float DeltaTime)
{
	if (CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = Observation.Kind == EELObservationKind::Hazard ? EELDebugEvent::Hazard : EELDebugEvent::Search;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.TargetIdentifier = Observation.TargetIdentifier;
		Event.Start = Adapter.Monster().Position();
		Event.Finish = Observation.Position;
		Event.Radius = Observation.Radius > 0.f ? Observation.Radius : .5f;
		Event.Label = "Назначено исследование тела";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	if (Device.dwTimeGlobal - StartedAt > Profile.SearchMax + Profile.BodyInspectMax)
	{
		Validation = {EELBehaviorState::NoProgress, "Истёк срок исследования тела"};
		return;
	}
	if (Adapter.Monster().Position().distance_to_sqr(Observation.Position) < 6.25f)
	{
		if (!HasReachedBody)
		{
			HasReachedBody = true;
			InspectUntil = Device.dwTimeGlobal + Random.randI(Profile.BodyInspectMin, Profile.BodyInspectMax + 1);
			ActionUntil = InspectUntil;
		}
		Movement.Stop();
		Adapter.Monster().dir().face_target(Observation.Position);
		Adapter.Monster().set_action(ACT_LOOK_AROUND);
		Inspection.Update(Profile.CanInspect);
		HasMoveCommand = false;
		ProgressTime = Device.dwTimeGlobal;
		if (s32(Device.dwTimeGlobal - InspectUntil) >= 0)
		{
			CELSharedDeaths::Release(Adapter.Monster().ID(), true);
			Validation = {EELBehaviorState::Completed, "Тело исследовано"};
		}
		else
		{
			Validation = {EELBehaviorState::Running, "Осмотр тела"};
		}
		return;
	}
	Validation = {EELBehaviorState::Running, "Подход к известному телу"};
	if (!Move(Observation.Position, Observation.VertexIdentifier, false))
	{
		Validation = {EELBehaviorState::Blocked, "Путь к телу перекрыт"};
	}
}

// Завершает активную схему и освобождает временные заявки.
void CELGroundBodyBehavior::Exit()
{
	Inspection.Clear();
	CELSharedDeaths::Release(Adapter.Monster().ID(), false);
	CELGroundBehaviorBase::Exit();
}
