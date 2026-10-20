#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Behaviors/CELFlyingRecoveryBehavior.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELFlyingRecoveryBehavior::Enter()
{
	CELFlyingBehaviorBase::Enter();
	Attempt = 1;
	HasDeparture = false;
}
// Выбирает следующую точку маршрута с учётом текущей фазы поведения.
void CELFlyingRecoveryBehavior::ChooseGoal()
{
	GoalPosition = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	Fvector Departure = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Departure.y = 0.f;
	if (Departure.square_magnitude() < EPS_L)
	{
		Departure.set(0.f, 0.f, 1.f);
	}
	Departure.normalize_safe();
	static const float Turns[] = {0.f, PI_DIV_4, -PI_DIV_4, PI_DIV_2, -PI_DIV_2, PI};
	const float Angle = Turns[(Attempt - 1) % 6];
	const Fvector Forward = Departure;
	Departure.set(Forward.x * cosf(Angle) + Forward.z * sinf(Angle), 0.f, Forward.z * cosf(Angle) - Forward.x * sinf(Angle));
	const float Clearance = float(Attempt) * Settings.RecoveryStep;
	const float Side = (Adapter.Monster().ID() + Attempt) % 2 ? 1.f : -1.f;
	if (Movement.InConfinedSpace())
	{
		const float Radius = std::max(Settings.CollisionRadius, Movement.PlanningSpeed() / Settings.TurnRate);
		GoalPosition.mad(Departure, 2.f * Radius);
		GoalDirection = Departure;
		return;
	}
	GoalPosition.mad(Departure, Settings.RecoveryForward + Clearance);
	GoalPosition.x += Departure.z * Side * Clearance;
	GoalPosition.z -= Departure.x * Side * Clearance;
	GoalPosition.y += Settings.RecoveryClimb + Clearance;
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);
	clamp(GoalPosition.x, Bounds.min.x, Bounds.max.x);
	clamp(GoalPosition.y, Bounds.min.y, Bounds.max.y);
	clamp(GoalPosition.z, Bounds.min.z, Bounds.max.z);
	GoalDirection = Departure;
}
// Переводит схему в фазу, соответствующую фактически принятому маршруту.
void CELFlyingRecoveryBehavior::OnRouteActivated()
{
	HasDeparture = true;
}

// Возвращает выбор обычной схеме сразу после сертификации выхода, оставляя весь его пролёт на расчёт продолжения.
bool CELFlyingRecoveryBehavior::ReadyToResume() const
{
	return HasDeparture;
}
// Обновляет ограниченную попытками схему выхода из препятствий и обрабатывает текущий результат выполнения.
void CELFlyingRecoveryBehavior::Update(float DeltaTime)
{
	if (!HasDeparture)
	{
		CELFlyingBehaviorBase::Update(DeltaTime);
		return;
	}
	Validation = {Movement.HasRoute() ? EELBehaviorState::Running : EELBehaviorState::Completed, Movement.HasRoute() ? "Выход из перекрытого коридора" : "Полёт восстановлен"};
}
// Выбирает ограниченное восстановление после сбоя либо освобождает схему.
EELRecovery CELFlyingRecoveryBehavior::Recover(const SELBehaviorValidation& Failure)
{
	if (Failure.State == EELBehaviorState::InvalidTarget || Attempt >= Settings.RecoverySteps)
	{
		return EELRecovery::Release;
	}
	Movement.AcknowledgeObstacle();
	CancelRouteRequest();
	HasPreparedRoute = false;
	HasDeparture = false;
	++Attempt;
	ProgressTime = Device.dwTimeGlobal;
	RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
	return EELRecovery::Continue;
}
