#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/Behaviors/CELCrowLandingBehavior.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/monsters/basemonster/base_monster.h"

// Связывает проверяемый заход вороны на посадку с необходимыми службами и состоянием владельца.
CELCrowLandingBehavior::CELCrowLandingBehavior(IELAdapter& OwnerAdapter, CELFlyingMovement& FlightMovement, const SELFlyingSettings& OwnerFlightSettings, const SELCrowSettings& CrowSettings) : Adapter(OwnerAdapter), Movement(FlightMovement), FlightSettings(OwnerFlightSettings), Settings(CrowSettings)
{
	Navigation.SetClearance(FlightSettings.CollisionRadius, FlightSettings.CollisionCenterHeight);
	Navigation.SetSearch(FlightSettings.SearchCellSize, FlightSettings.SearchNodeLimit, FlightSettings.SearchTimeout);
}
// Настраивает условия следующего запуска схемы.
void CELCrowLandingBehavior::Configure(const Fvector& Point, const Fvector& Direction)
{
	Contact = Point;
	Facing = Direction;
	Facing.y = 0.f;
	Facing.normalize_safe();
}
// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELCrowLandingBehavior::Enter()
{
	Phase = 0;
	HasActiveRoute = false;
	StartedAt = ProgressAt = Device.dwTimeGlobal;
	ProgressPosition = Adapter.Monster().Position();
	Validation = SELBehaviorValidation();
	Movement.SetLanding(true);
}
// Отправляет очередной участок посадки на построение и проверку.
bool CELCrowLandingBehavior::Request()
{
	SELNavigationRequest Request;
	Request.Identity = {ExtendedEntityes::Navigation().GetGeneration(), ExtendedEntityes::OwnerGeneration(Adapter.Monster().ID()), Movement.NextPlanGeneration(), u16(Adapter.Monster().ID())};
	Request.IsFlying = true;
	Request.StartPosition = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	Request.StartDirection = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Request.GoalPosition = Contact;
	Request.GoalPosition.y += Settings.GroundOffset;
	if (Phase == 0)
	{
		Request.GoalPosition.mad(Facing, -Settings.LandingApproachDistance);
		Request.GoalPosition.y += Settings.LandingHeight;
	}
	Request.GoalDirection = Facing;
	Request.CurveDistance = std::max(Settings.LandingCurveMin, std::min(Settings.LandingCurveMax, Request.StartPosition.distance_to(Request.GoalPosition) * Settings.LandingCurveRatio));
	return Navigation.Submit(std::move(Request));
}
// Обновляет проверяемый заход вороны на посадку и обрабатывает текущий результат выполнения.
void CELCrowLandingBehavior::Update(float DeltaTime)
{
	if (Movement.WasBlocked())
	{
		Validation = {EELBehaviorState::Blocked, "Препятствие перед посадкой"};
		return;
	}
	if (Phase == 2)
	{
		Validation = {EELBehaviorState::Completed, "Посадка завершена"};
		return;
	}
	if (ProgressPosition.distance_to_sqr(Adapter.Monster().Position()) > .01f)
	{
		ProgressPosition = Adapter.Monster().Position();
		ProgressAt = Device.dwTimeGlobal;
	}
	SELNavigationResult Result;
	if (Navigation.Poll(Result))
	{
		if (!Movement.HasRoute())
		{
			Movement.SetRoute(std::move(Result));
			HasActiveRoute = true;
		}
		else
		{
			// A prepared landing begins at the end of the current certified route.
			Prepared = std::move(Result);
		}
	}
	if (!Movement.HasRoute() && Prepared.HasPath)
	{
		Movement.SetRoute(std::move(Prepared));
		Prepared = SELNavigationResult();
		HasActiveRoute = true;
	}
	if (HasActiveRoute && !Movement.HasRoute())
	{
		HasActiveRoute = false;
		++Phase;
		ProgressAt = Device.dwTimeGlobal;
		if (Phase == 2)
		{
			Movement.Stop();
			Validation = {EELBehaviorState::Completed, "Посадка завершена"};
			return;
		}
	}
	if (!HasActiveRoute && !Navigation.IsPending() && !Prepared.HasPath && !Navigation.WasBlocked())
	{
		Request();
	}
	Validation = {Movement.HasRoute() ? EELBehaviorState::Running : EELBehaviorState::WaitingForResult, "Заход на посадку"};
}
// Возвращает состояние выполнения схемы, включая ожидание, завершение и сбои.
SELBehaviorValidation CELCrowLandingBehavior::Validate() const
{
	if (Navigation.WasBlocked() || Movement.WasBlocked())
	{
		return {EELBehaviorState::Blocked, "Посадочный коридор перекрыт"};
	}
	if (Device.dwTimeGlobal - StartedAt > u32(Settings.LandingLimit * 1000.f))
	{
		return {EELBehaviorState::NoProgress, "Истёк срок посадки"};
	}
	if (HasActiveRoute && Device.dwTimeGlobal - ProgressAt > u32(Settings.LandingStall * 1000.f))
	{
		return {EELBehaviorState::NoProgress, "Нет продвижения при посадке"};
	}
	return Validation;
}
// Выбирает ограниченное восстановление после сбоя либо освобождает схему.
EELRecovery CELCrowLandingBehavior::Recover(const SELBehaviorValidation& Failure)
{
	return EELRecovery::Release;
}
// Завершает активную схему и освобождает временные заявки.
void CELCrowLandingBehavior::Exit()
{
	Navigation.Cancel();
	Prepared = SELNavigationResult();
	Movement.SetLanding(false);
}
