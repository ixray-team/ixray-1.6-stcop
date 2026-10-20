#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Behaviors/CELFlyingRestBehavior.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttackAnimation.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "Level.h"

// Связывает поиск опоры, посадку и отдых летуна с необходимыми службами и состоянием владельца.
CELFlyingRestBehavior::CELFlyingRestBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, CELFlyingAttackAnimation& FlightAnimation) : CELFlyingBehaviorBase(OwnerAdapter, FlightSettings, FlightMovement), Animation(FlightAnimation) {}
// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELFlyingRestBehavior::Enter()
{
	CELFlyingBehaviorBase::Enter();
	Phase = EPhase::FindSupport;
	HasActiveLeg = false;
	Facing = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Facing.y = 0.f;
	if (Facing.square_magnitude() < EPS_L)
	{
		Facing.set(0.f, 0.f, 1.f);
	}
	Facing.normalize_safe();
	Movement.SetLanding(true);
}
// Ищет и проверяет подходящую опору для посадки.
bool CELFlyingRestBehavior::FindSupport()
{
	Fvector From = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	From.mad(Facing, Settings.LandingApproachDistance);
	From.y += Settings.LandingHeight;
	const float Distance = From.y - Level().ObjectSpace.GetBoundingVolume().min.y;
	const float GroundOffset = std::max(0.f, Settings.CollisionRadius - Settings.CollisionCenterHeight) + Settings.GroundClearance;
	const EELGeometryResult State = CELFlyingGeometry::QueryLanding(From, Distance, Settings.CollisionRadius, Settings.CollisionCenterHeight, GroundOffset, Settings.LandingMinimumNormalY, Settings.LandingHeightTolerance, Contact, &Adapter.Monster());
	if (State != EELGeometryResult::Clear)
	{
		Validation = {State == EELGeometryResult::Pending ? EELBehaviorState::WaitingForBudget : EELBehaviorState::Blocked, State == EELGeometryResult::Pending ? "Ожидание проверки места отдыха" : "Нет безопасного места отдыха"};
		return false;
	}
	Phase = EPhase::Approach;
	return true;
}
// Разрешает планирование только в фазах, которым нужен следующий участок.
bool CELFlyingRestBehavior::CanPlanCurve() const
{
	return !HasActiveLeg && (Phase == EPhase::Approach || Phase == EPhase::Touchdown);
}
// Выбирает следующую точку маршрута с учётом текущей фазы поведения.
void CELFlyingRestBehavior::ChooseGoal()
{
	GoalPosition = Contact;
	GoalDirection = Facing;
	if (Phase == EPhase::Approach)
	{
		GoalPosition.mad(Facing, -Settings.LandingApproachDistance);
		GoalPosition.y += Settings.LandingHeight;
	}
}
// Переводит схему в фазу, соответствующую фактически принятому маршруту.
void CELFlyingRestBehavior::OnRouteActivated()
{
	HasActiveLeg = true;
}
// Сообщает, достигнута ли фаза фактического отдыха на опоре.
bool CELFlyingRestBehavior::IsResting() const
{
	return Phase == EPhase::Resting;
}
// Обновляет таймер отдыха и условия его продолжения.
void CELFlyingRestBehavior::UpdateRest()
{
	if (s32(Device.dwTimeGlobal - RestUntil) >= 0)
	{
		Validation = {EELBehaviorState::Completed, "Отдых завершён"};
		return;
	}
	if (s32(Device.dwTimeGlobal - NextSupportCheck) >= 0)
	{
		Fvector From = Adapter.Monster().Position();
		From.y += Settings.LandingHeight;
		Fvector CurrentContact;
		const float GroundOffset = std::max(0.f, Settings.CollisionRadius - Settings.CollisionCenterHeight) + Settings.GroundClearance;
		const EELGeometryResult State = CELFlyingGeometry::QueryLanding(From, Settings.LandingHeight + Settings.CollisionCenterHeight + Settings.CollisionRadius, Settings.CollisionRadius, Settings.CollisionCenterHeight, GroundOffset, Settings.LandingMinimumNormalY, Settings.LandingHeightTolerance, CurrentContact, &Adapter.Monster());
		if (State == EELGeometryResult::Pending)
		{
			Validation = {EELBehaviorState::WaitingForBudget, "Ожидание проверки опоры"};
			return;
		}
		if (State == EELGeometryResult::Blocked || CurrentContact.distance_to_sqr(Contact) > Settings.GroundClearance * Settings.GroundClearance)
		{
			Validation = {EELBehaviorState::Blocked, "Опора для отдыха изменилась"};
			return;
		}
		NextSupportCheck = Device.dwTimeGlobal + Settings.RestSupportInterval;
	}
	Validation = {EELBehaviorState::Running, "Отдых на проверенной опоре"};
}
// Обновляет поиск опоры, посадку и отдых летуна и обрабатывает текущий результат выполнения.
void CELFlyingRestBehavior::Update(float DeltaTime)
{
	if (Phase == EPhase::Resting)
	{
		UpdateRest();
		return;
	}
	if (Phase == EPhase::FindSupport && !FindSupport())
	{
		return;
	}
	if (HasActiveLeg && !Movement.HasRoute())
	{
		HasActiveLeg = false;
		if (Phase == EPhase::Approach)
		{
			Phase = EPhase::Touchdown;
		}
		else
		{
			Phase = EPhase::Resting;
			RestUntil = Device.dwTimeGlobal + u32(Settings.RestDuration * 1000.f);
			NextSupportCheck = Device.dwTimeGlobal;
			Movement.Stop();
			Animation.Rest();
			UpdateRest();
			return;
		}
		ProgressTime = Device.dwTimeGlobal;
	}
	CELFlyingBehaviorBase::Update(DeltaTime);
}
// Возвращает состояние выполнения схемы, включая ожидание, завершение и сбои.
SELBehaviorValidation CELFlyingRestBehavior::Validate() const
{
	if (!Adapter.Monster().g_Alive())
	{
		return {EELBehaviorState::InvalidTarget, "Существо погибло"};
	}
	return Phase == EPhase::Resting ? Validation : CELFlyingBehaviorBase::Validate();
}
// Выбирает ограниченное восстановление после сбоя либо освобождает схему.
EELRecovery CELFlyingRestBehavior::Recover(const SELBehaviorValidation& Failure)
{
	return EELRecovery::Release;
}
// Завершает активную схему и освобождает временные заявки.
void CELFlyingRestBehavior::Exit()
{
	if (Phase == EPhase::Resting)
	{
		Animation.Fly();
	}
	Movement.SetLanding(false);
	CELFlyingBehaviorBase::Exit();
}
