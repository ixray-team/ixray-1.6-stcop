#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/Behaviors/CELCrowRestBehavior.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowAnimation.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingMovement.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingObstacles.h"
#include "ai/monsters/basemonster/base_monster.h"

// Связывает наземный отдых и короткие шаги вороны с необходимыми службами и состоянием владельца.
CELCrowRestBehavior::CELCrowRestBehavior(IELAdapter& OwnerAdapter, CELFlyingMovement& FlightMovement, CELCrowAnimation& CrowAnimation, const SELCrowSettings& CrowSettings, SELCrowState& CrowState) : Adapter(OwnerAdapter), Movement(FlightMovement), Animation(CrowAnimation), Settings(CrowSettings), State(CrowState) {}
// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELCrowRestBehavior::Enter()
{
	CancelStep();
	Movement.Stop();
	StartedAt = Device.dwTimeGlobal;
	StepProgress = 1.f;
	NextStep = StartedAt + u32(Settings.Step.Sample() * 1000.f);
	Validation = SELBehaviorValidation();
	Animation.SetGrounded(true);
}
// Выбирает опору короткого шага и проверяет допустимость прохода.
bool CELCrowRestBehavior::PrepareStep()
{
	Fvector Candidate = Adapter.Monster().Position();
	const float Angle = Random.randF(0.f, PI_MUL_2);
	Candidate.x += std::sin(Angle) * Settings.StepRadius;
	Candidate.z += std::cos(Angle) * Settings.StepRadius;
	Candidate.y += 1.f;

	Fvector Contact;
	const SELFlyingSettings& FlightSettings = Movement.GetSettings();
	if (CELFlyingGeometry::QueryLanding(Candidate, 2.f, FlightSettings.CollisionRadius, FlightSettings.CollisionCenterHeight, Settings.GroundOffset, std::cos(deg2rad(Settings.MaxSlope)), FlightSettings.LandingHeightTolerance, Contact, &Adapter.Monster()) != EELGeometryResult::Clear)
	{
		return false;
	}
	Candidate = Contact;
	Fvector StartCenter = Adapter.Monster().Position();
	StartCenter.y += FlightSettings.CollisionCenterHeight;
	Fvector EndCenter = Candidate;
	EndCenter.y += FlightSettings.CollisionCenterHeight;
	if (!CELFlyingGeometry::ClearSegment(StartCenter, EndCenter, &Adapter.Monster()))
	{
		return false;
	}
	StepStart = Adapter.Monster().Position();
	StepGoal = Candidate;
	StepDuration = std::max(.1f, StepStart.distance_to(StepGoal) / Settings.GroundSpeed);

	return QueueStep();
}
// Отправляет все отрезки наземного шага на проверку корпуса.
bool CELCrowRestBehavior::QueueStep()
{
	const SELFlyingSettings& FlightSettings = Movement.GetSettings();
	SELNavigationResult Route;
	StepIdentity = {ExtendedEntityes::Navigation().GetGeneration(), ExtendedEntityes::OwnerGeneration(Adapter.Monster().ID()), ++StepGeneration, u16(Adapter.Monster().ID())};
	Route.Identity = StepIdentity;
	Route.HasPath = true;
	const u32 PointCount = u32(iCeil(StepStart.distance_to(StepGoal) / FlightSettings.CollisionRadius)) + 1;
	u32 PointIndex;
	Fvector Point;
	for (PointIndex = 0; PointIndex <= PointCount; ++PointIndex)
	{
		Point.lerp(StepStart, StepGoal, float(PointIndex) / float(PointCount));
		Route.Points.push_back(Point);
	}
	const float HopPadding = Settings.GroundHops ? Settings.HopHeight * .5f : 0.f;
	HasPendingStep = CELFlyingGeometry::Submit(std::move(Route), FlightSettings.CollisionRadius + HopPadding, FlightSettings.CollisionCenterHeight + HopPadding);
	return HasPendingStep;
}
// Обновляет наземный отдых и короткие шаги вороны и обрабатывает текущий результат выполнения.
void CELCrowRestBehavior::Update(float DeltaTime)
{
	PollStep();
	if (State.Remaining <= 0.f)
	{
		Validation = {EELBehaviorState::Completed, "Отдых завершён"};
		return;
	}
	if (!HasPendingStep && StepProgress >= 1.f && !Animation.IsPecking() && (State.Rest == EELCrowRest::Ground || State.Rest == EELCrowRest::Night) && s32(Device.dwTimeGlobal - NextStep) >= 0)
	{
		PrepareStep();
		NextStep = Device.dwTimeGlobal + u32((State.Rest == EELCrowRest::Night ? Settings.NightStep : Settings.Step).Sample() * 1000.f);
	}
	if (HasPendingStep)
	{
		Validation = {EELBehaviorState::WaitingForResult, "Проверка наземного шага"};
		return;
	}
	Validation = {EELBehaviorState::Running, State.Rest == EELCrowRest::Corpse ? "Кормление у тела" : State.Rest == EELCrowRest::Shelter ? "В укрытии"
																								  : State.Rest == EELCrowRest::Night	 ? "Ночной отдых"
																																		 : "Отдых"};
}
// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELCrowRestBehavior::FrameUpdate(float DeltaTime)
{
	float Hop = 0.f;
	Fvector NewPosition;

	if (StepProgress < 1.f)
	{
		SELNavigationObstacle Obstacle;
		float ClearDistance;
		if (CELSharedFlyingObstacles::IsBlocked(StepIdentity, Obstacle, ClearDistance))
		{
			CancelStep();
			return;
		}
		SELFlyingCorridor Corridor;
		Corridor.Identity = StepIdentity;
		Corridor.Start = Adapter.Monster().Position();
		Corridor.End = StepGoal;
		const float HopPadding = Settings.GroundHops ? Settings.HopHeight * .5f : 0.f;
		Corridor.Radius = Movement.GetSettings().CollisionRadius + HopPadding;
		Corridor.CenterHeight = Movement.GetSettings().CollisionCenterHeight + HopPadding;
		Corridor.IsLanding = true;
		CELSharedFlyingObstacles::Publish(Corridor);
		StepProgress = std::min(1.f, StepProgress + std::min(.1f, DeltaTime) / StepDuration);
		NewPosition.lerp(StepStart, StepGoal, StepProgress);
		Fvector Direction;
		Direction.sub(StepGoal, StepStart).normalize_safe();
		float Heading;
		float Pitch;
		Direction.getHP(Heading, Pitch);
		Adapter.Monster().XFORM().setHPB(Heading, 0.f, 0.f);
		Adapter.Monster().Position() = NewPosition;
		if (Settings.GroundHops)
		{
			Hop = Settings.HopHeight * std::sin(PI * StepProgress);
		}
		Adapter.Monster().spatial_move();
		if (StepProgress >= 1.f)
		{
			CELSharedFlyingObstacles::Cancel(StepIdentity);
		}
	}
	Animation.SetGrounded(true, Hop);
}
// Возвращает состояние выполнения схемы, включая ожидание, завершение и сбои.
SELBehaviorValidation CELCrowRestBehavior::Validate() const
{
	return Validation;
}
// Выбирает ограниченное восстановление после сбоя либо освобождает схему.
EELRecovery CELCrowRestBehavior::Recover(const SELBehaviorValidation& Failure)
{
	return EELRecovery::Release;
}
// Завершает активную схему и освобождает временные заявки.
void CELCrowRestBehavior::Exit()
{
	CancelStep();
	Animation.SetGrounded(false);
}

// Отменяет проверки шага и прекращает незавершённое перемещение.
void CELCrowRestBehavior::CancelStep()
{
	if (HasPendingStep)
	{
		CELFlyingGeometry::Cancel(StepIdentity);
	}
	CELSharedFlyingObstacles::Cancel(StepIdentity);
	HasPendingStep = false;
	StepProgress = 1.f;
}
// Запускает шаг только после положительного результата его проверки.
void CELCrowRestBehavior::PollStep()
{
	if (!HasPendingStep)
	{
		return;
	}
	SELNavigationResult Result;
	const EELGeometryResult State = CELFlyingGeometry::Take(StepIdentity, Result);
	if (State == EELGeometryResult::Pending)
	{
		return;
	}
	HasPendingStep = false;
	if (State == EELGeometryResult::Clear && Adapter.Monster().Position().distance_to_sqr(StepStart) <= EPS_L)
	{
		StepProgress = 0.f;
	}
}
