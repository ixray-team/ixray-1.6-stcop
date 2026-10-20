#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorBase.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/monsters/basemonster/base_monster.h"

// Связывает базовую воздушную схему с проверяемыми маршрутами с необходимыми службами и состоянием владельца.
CELFlyingBehaviorBase::CELFlyingBehaviorBase(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement) : Adapter(OwnerAdapter), Settings(FlightSettings), Movement(FlightMovement)
{
	Navigation.SetClearance(Settings.CollisionRadius, Settings.CollisionCenterHeight);
	Navigation.SetSearch(Settings.SearchCellSize, Settings.SearchNodeLimit, Settings.SearchTimeout);
}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELFlyingBehaviorBase::Enter()
{
	Movement.AcknowledgeObstacle();
	ProgressPosition = Adapter.Monster().Position();
	ProgressTime = Device.dwTimeGlobal;
	RecoveryCount = 0;
	RetryAfter = 0;
	Validation = SELBehaviorValidation();
	RequestedAt = Device.dwTimeGlobal;
	LastRouteReadyMilliseconds = 0;
	IsRecovering = false;
	Continuity.Clear();
}

// Обновляет известные сведения о цели для текущей схемы.
void CELFlyingBehaviorBase::SetObservation(const SELObservation* TargetObservation)
{
	HasTarget = TargetObservation != nullptr;
	if (TargetObservation)
	{
		Observation = *TargetObservation;
	}
}

// Отправляет следующую воздушную кривую от конца сохранённого маршрута.
bool CELFlyingBehaviorBase::RequestCurve()
{
	SELNavigationRequest Request;
	Request.IsFlying = true;
	Request.Identity = {ExtendedEntityes::Navigation().GetGeneration(), ExtendedEntityes::OwnerGeneration(Adapter.Monster().ID()), Movement.NextPlanGeneration(), u16(Adapter.Monster().ID())};
	Request.StartPosition = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	Request.StartDirection = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	CELSharedAnomalies::Snapshot(Adapter.Monster().ID(), Request.Obstacles);
	Movement.AppendObstacles(Request.Obstacles);
	Request.GoalPosition = GoalPosition;
	Request.GoalDirection = GoalDirection;
	Request.StartDirection.normalize_safe();
	Request.GoalDirection.normalize_safe();
	Request.CurveDistance = std::max(Settings.CurveMinimum, std::min(Settings.CurveMaximum, Request.StartPosition.distance_to(Request.GoalPosition) * Settings.CurveRatio));
	ConfigureCurveRequest(Request);
	Request.InvertedBodyRadius = std::max(Request.InvertedBodyRadius, Movement.TransitionBodyRadius());
	Navigation.SetClearance(Request.InvertedBodyRadius > 0.f ? Request.InvertedBodyRadius : Settings.CollisionRadius, Request.InvertedBodyRadius > 0.f ? 0.f : Settings.CollisionCenterHeight);
	bool StreamRoute = !IsRecovering && CanStreamRoute();
	if (!Request.CanInvert && !Movement.IsLandingFlight())
	{
		float Minimum;
		float Maximum;
		bool CoveredStart = false;
		const float Radius = Request.InvertedBodyRadius > 0.f ? Request.InvertedBodyRadius : Settings.CollisionRadius;
		const float CenterHeight = Request.InvertedBodyRadius > 0.f ? 0.f : Settings.CollisionCenterHeight;
		EELGeometryResult Space = CELFlyingGeometry::QueryHeightBand(Request.StartPosition, Request.StartPosition, Radius, CenterHeight, Settings.GroundClearance, Minimum, Maximum, CoveredStart);
		if (Space != EELGeometryResult::Clear)
		{
			if (Space == EELGeometryResult::Blocked)
			{
				Navigation.Reject();
			}
			return false;
		}
		Movement.SetConfined(CoveredStart);
		if (CoveredStart && Request.GuidePoints.empty() && LimitsGoalUnderCover())
		{
			Fvector Horizontal = Fvector().sub(Request.GoalPosition, Request.StartPosition);
			Horizontal.y = 0.f;
			const float Limit = std::max(Settings.CollisionRadius * 2.f, 2.f * Movement.PlanningSpeed() / Settings.TurnRate);
			if (Horizontal.magnitude() > Limit)
			{
				Horizontal.normalize_safe().mul(Limit);
				Request.GoalPosition.x = Request.StartPosition.x + Horizontal.x;
				Request.GoalPosition.z = Request.StartPosition.z + Horizontal.z;
				GoalPosition = Request.GoalPosition;
			}
		}
		Navigation.SetSearch(CoveredStart ? std::min(Settings.SearchCellSize, std::max(1.f, Radius * 2.f + Settings.GroundClearance)) : Settings.SearchCellSize, Settings.SearchNodeLimit, Settings.SearchTimeout);
		bool CoveredGoal = false;
		Space = CELFlyingGeometry::QueryHeightBand(Request.StartPosition, Request.GoalPosition, Radius, CenterHeight, Settings.GroundClearance, Minimum, Maximum, CoveredGoal);
		if (Space != EELGeometryResult::Clear)
		{
			if (Space == EELGeometryResult::Blocked)
			{
				Navigation.Reject();
			}
			return false;
		}
		// В помещении нельзя отдать подлёт до проверки выхода и затем остаться у непроходимого хвоста.
		StreamRoute = StreamRoute && !CoveredStart && !CoveredGoal;
		for (u32 Index = 1; Index + 1 < Request.GuidePoints.size() && !Request.GuideDirections.empty(); ++Index)
		{
			bool CoveredContact = false;
			float ContactMinimum;
			float ContactMaximum;
			Space = CELFlyingGeometry::QueryHeightBand(Request.GuidePoints[Index], Request.GuidePoints[Index], Radius, CenterHeight, Settings.GroundClearance, ContactMinimum, ContactMaximum, CoveredContact);
			if (Space != EELGeometryResult::Clear)
			{
				if (Space == EELGeometryResult::Blocked)
				{
					Navigation.Reject();
				}
				return false;
			}
			StreamRoute = StreamRoute && !CoveredContact;
		}
		const float PreviousHeight = Request.GoalPosition.y;
		clamp(Request.GoalPosition.y, Minimum, Maximum);
		if (CoveredStart && CoveredGoal && CentersUnderCover())
		{
			Request.GoalPosition.y = std::min(Request.GoalPosition.y, (Minimum + Maximum) * .5f);
		}
		if (!fsimilar(PreviousHeight, Request.GoalPosition.y))
		{
			Request.GoalDirection.y = 0.f;
			Request.GoalDirection.normalize_safe();
			if (!Request.GuidePoints.empty())
			{
				Request.GuidePoints.back() = Request.GoalPosition;
				if (!Request.GuideDirections.empty())
				{
					Request.GuideDirections.back() = Request.GoalDirection;
				}
			}
			GoalPosition = Request.GoalPosition;
			GoalDirection = Request.GoalDirection;
		}
		if (CoveredStart && !Movement.HasRoute() && Movement.GetSpeed() < .2f && Request.StartDirection.y > 0.f)
		{
			// У уже остановившегося под потолком нет вертикальной инерции, требующей нового захода в перекрытие.
			Request.StartDirection.y = 0.f;
			Request.StartDirection.normalize_safe();
			if (Request.StartDirection.square_magnitude() <= EPS_L)
			{
				Request.StartDirection = Request.GoalDirection;
			}
			if (!Request.GuideDirections.empty())
			{
				Request.GuideDirections.front() = Request.StartDirection;
			}
		}
	}
	RequestedAt = Device.dwTimeGlobal;
	HasPendingContact = !Request.GuideDirections.empty() && Request.GuidePoints.size() > 2;
	if (HasPendingContact)
	{
		PendingContact = Request.GuidePoints[1];
	}
	Navigation.SetStreaming(StreamRoute);
	return Navigation.Submit(std::move(Request));
}

// Ставит следующий участок заранее; активное движение при этом сохраняется.
void CELFlyingBehaviorBase::PrepareNextRoute()
{
	// Отказ будущего участка не отменяет пригодный текущий: повторяем подготовку, пока он ещё выполняется.
	if (Movement.HasRoute() && Navigation.WasBlocked())
	{
		OnRouteRejected();
		CancelRouteRequest();
		RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
	}
	if (CanPlanCurve() && !Navigation.IsPending() && !Navigation.WasBlocked() && !HasPreparedRoute && s32(Device.dwTimeGlobal - RetryAfter) >= 0)
	{
		float Minimum;
		float Maximum;
		bool Covered = false;
		const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
		const EELGeometryResult Space = CELFlyingGeometry::QueryHeightBand(Origin, Origin, Settings.CollisionRadius, Settings.CollisionCenterHeight, Settings.GroundClearance, Minimum, Maximum, Covered);
		if (Space != EELGeometryResult::Clear)
		{
			if (Space == EELGeometryResult::Blocked)
			{
				Navigation.Reject();
			}
			RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
			return;
		}
		Movement.SetConfined(Covered);
		ChooseGoal();
		if (!RequestCurve())
		{
			RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
		}
	}
}

// Обновляет базовую воздушную схему с проверяемыми маршрутами и обрабатывает текущий результат выполнения.
void CELFlyingBehaviorBase::Update(float DeltaTime)
{
	ActivatePreparedRoute();
	PrepareNextRoute();
	if (Navigation.WasBlocked())
	{
		Validation = {EELBehaviorState::Blocked, "Воздушный коридор перекрыт"};
		RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
	}
	else
	{
		Validation = {Movement.HasRoute() ? EELBehaviorState::Running : EELBehaviorState::WaitingForResult, Movement.HasRoute() ? "Полёт" : Navigation.Status()};
	}
}

// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELFlyingBehaviorBase::FrameUpdate(float DeltaTime)
{
	ActivatePreparedRoute();
	PrepareNextRoute();
	MaintainContinuity();
	if (ProgressPosition.distance_to_sqr(Adapter.Monster().Position()) >= Settings.ProgressDistance * Settings.ProgressDistance)
	{
		ProgressPosition = Adapter.Monster().Position();
		ProgressTime = Device.dwTimeGlobal;
		RecoveryCount = 0;
	}
}

// Возвращает состояние выполнения схемы, включая ожидание, завершение и сбои.
SELBehaviorValidation CELFlyingBehaviorBase::Validate() const
{
	if (!Adapter.Monster().g_Alive())
	{
		return {EELBehaviorState::InvalidTarget, "Существо погибло"};
	}
	if (Movement.WasBlocked())
	{
		return {EELBehaviorState::Blocked, "Впереди подвижное препятствие"};
	}
	if (Movement.HasRoute() && Device.dwTimeGlobal - ProgressTime > Settings.ProgressTimeout)
	{
		return {EELBehaviorState::NoProgress, "Нет продвижения в полёте"};
	}
	if (!Movement.HasRoute() && Device.dwTimeGlobal - ProgressTime > Settings.RouteWaitTimeout)
	{
		return {EELBehaviorState::NoProgress, "Истёк срок полётного маршрута"};
	}
	if (Navigation.WasBlocked())
	{
		return {EELBehaviorState::Blocked, "Нет безопасного продолжения маршрута"};
	}
	if (Validation.State == EELBehaviorState::Completed || Validation.State == EELBehaviorState::Blocked || Validation.State == EELBehaviorState::InvalidTarget)
	{
		return Validation;
	}
	if (Movement.HasRoute())
	{
		return {EELBehaviorState::Running, Movement.IsContinuityFlight() ? "Безопасный манёвр: основной поиск продолжается" : FlightDescription()};
	}
	if (Navigation.IsPending())
	{
		return {EELBehaviorState::WaitingForResult, Navigation.Status()};
	}
	if (CanPlanCurve())
	{
		return {EELBehaviorState::WaitingForBudget, "Ожидание постановки продолжения маршрута"};
	}
	return Validation;
}

// Выбирает ограниченное восстановление после сбоя либо освобождает схему.
EELRecovery CELFlyingBehaviorBase::Recover(const SELBehaviorValidation& Failure)
{
	if (Failure.State == EELBehaviorState::InvalidTarget || RecoveryCount >= Settings.RecoverySteps)
	{
		return EELRecovery::Release;
	}
	if (IsRecovering && Navigation.IsPending() && Device.dwTimeGlobal - RequestedAt <= Settings.RouteWaitTimeout)
	{
		return EELRecovery::Continue;
	}
	Movement.AcknowledgeObstacle();
	CancelRouteRequest();
	HasPreparedRoute = false;
	IsRecovering = true;
	ProgressTime = Device.dwTimeGlobal;
	++RecoveryCount;
	GoalPosition = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	Fvector Departure = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Departure.y = 0.f;
	if (Departure.square_magnitude() < EPS_L)
	{
		Departure.set(0.f, 0.f, 1.f);
	}
	Departure.normalize_safe();
	const float Side = Adapter.Monster().ID() % 2 ? 1.f : -1.f;
	const float Clearance = float(std::min(RecoveryCount, Settings.RecoverySteps)) * Settings.RecoveryStep;
	GoalPosition.mad(Departure, Settings.RecoveryForward + Clearance);
	GoalPosition.x += Departure.z * Side * Clearance;
	GoalPosition.z -= Departure.x * Side * Clearance;
	GoalPosition.y += Settings.RecoveryClimb + Clearance;
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);
	clamp(GoalPosition.x, Bounds.min.x, Bounds.max.x);
	clamp(GoalPosition.y, Bounds.min.y, Bounds.max.y);
	clamp(GoalPosition.z, Bounds.min.z, Bounds.max.z);
	GoalDirection.set(Departure.x, .7f, Departure.z).normalize_safe();
	RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
	RequestCurve();
	return EELRecovery::Continue;
}

// Завершает активную схему и освобождает временные заявки.
void CELFlyingBehaviorBase::Exit()
{
	CancelRouteRequest();
	HasPreparedRoute = false;
	Continuity.Clear();
}

// Переводит схему в фазу, соответствующую фактически принятому маршруту.
void CELFlyingBehaviorBase::OnRouteActivated()
{
}

// Принимает проверенный маршрут, сохраняя непрерывность движения.
void CELFlyingBehaviorBase::ActivatePreparedRoute()
{
	if (Movement.WasBlocked())
	{
		return;
	}
	SELNavigationResult Incoming;
	if (Navigation.Poll(Incoming))
	{
		if (!Incoming.IsContinuation)
		{
			LastRouteReadyMilliseconds = Device.dwTimeGlobal - RequestedAt;
		}
		if (Movement.AppendRouteTail(Incoming))
		{
			return;
		}
		if (Incoming.IsContinuation && HasPreparedRoute && PreparedRoute.Identity == Incoming.Identity && !PreparedRoute.Points.empty() && PreparedRoute.Points.back().similar(Incoming.Points.front(), EPS_L))
		{
			PreparedRoute.Points.insert(PreparedRoute.Points.end(), Incoming.Points.begin() + 1, Incoming.Points.end());
			PreparedRoute.IsPartial = Incoming.IsPartial;
		}
		else if (Incoming.IsContinuation)
		{
			// Хвост без своего принятого начала не превращаем в новый путь со скачком к чужому узлу.
			CancelRouteRequest();
			HasPreparedRoute = false;
			OnRouteRejected();
			RetryAfter = Device.dwTimeGlobal + Settings.RouteRetryDelay;
			return;
		}
		else
		{
			PreparedRoute = std::move(Incoming);
			PreparedAt = Device.dwTimeGlobal;
		}
		HasPreparedRoute = true;
	}
	// Устаревший следующий участок заменяем, пока текущий ещё движется, а не после остановки в его конце.
	if (HasPreparedRoute && !IsRecovering && !CanActivatePreparedRoute())
	{
		HasPreparedRoute = false;
		PreparedRoute = SELNavigationResult();
		RetryAfter = 0;
	}
	if (HasPreparedRoute && Movement.HasRoute() && !IsRecovering && ActivateReplacementRoute(PreparedRoute))
	{
		HasPreparedRoute = false;
		OnRouteActivated();
	}
	if (HasPreparedRoute && !Movement.IsLandingFlight())
	{
		const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
		if (!PreparedRoute.Points.empty() && !PreparedRoute.Points.front().similar(Origin, EPS_L))
		{
			if (Device.dwTimeGlobal - PreparedAt > 2000u)
			{
				CancelRouteRequest();
				OnRouteRejected();
				RetryAfter = 0;
				return;
			}
			if (!Movement.HasRoute() || Movement.IsStalled())
			{
				MaintainContinuity();
			}
			const EELGeometryResult Connection = Continuity.Connect(Adapter, Movement, Settings, PreparedRoute, HasPendingContact ? &PendingContact : nullptr);
			if (Connection == EELGeometryResult::Pending)
			{
				return;
			}
			if (Connection == EELGeometryResult::Blocked)
			{
				return;
			}
		}
	}
	if (HasPreparedRoute && Movement.HasRoute() && Movement.IsContinuityFlight() && Movement.SpliceRoute(PreparedRoute))
	{
		HasPreparedRoute = false;
		Continuity.Clear();
		if (!IsRecovering)
		{
			OnRouteActivated();
		}
		IsRecovering = false;
	}
	if (!Movement.HasRoute() && HasPreparedRoute)
	{
		Movement.SetRoute(std::move(PreparedRoute));
		HasPreparedRoute = false;
		if (!IsRecovering)
		{
			OnRouteActivated();
		}
		IsRecovering = false;
		Continuity.Clear();
	}
}

// Принимает готовое продолжение в том же кадре, в котором достигнут конец текущего пути.
void CELFlyingBehaviorBase::AfterMovement()
{
	ActivatePreparedRoute();
	PrepareNextRoute();
	MaintainContinuity();
}

// A tactical search may wait; movement must not depend on receiving its entire answer.
void CELFlyingBehaviorBase::MaintainContinuity()
{
	if (Movement.IsLandingFlight() || !Adapter.CanControl() || !Adapter.Monster().g_Alive())
	{
		return;
	}
	const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	if (!Movement.IsStalled() && HasPreparedRoute && !PreparedRoute.Points.empty() && PreparedRoute.Points.front().similar(Origin, EPS_L))
	{
		return;
	}
	Continuity.Prepare(Adapter, Movement, Settings, HasTarget ? Observation.Position : GoalPosition);
	if (!Movement.HasRoute() || Movement.IsStalled())
	{
		SELNavigationResult Route;
		if (Continuity.Take(Adapter.Monster().Position(), Route))
		{
			Movement.AcknowledgeObstacle();
			Movement.SetContactTarget(u16(-1));
			Movement.SetRoute(std::move(Route), true);
		}
	}
}

// Возвращает фактический возраст незавершённого запроса для проверки задержки реакции.
u32 CELFlyingBehaviorBase::PendingRouteAge() const
{
	return Navigation.IsPending() ? Device.dwTimeGlobal - RequestedAt : 0;
}

// Отменяет производителя хвоста; сохранённый префикс остаётся конечным проверенным маршрутом.
void CELFlyingBehaviorBase::CancelRouteRequest()
{
	Movement.CloseRouteStream(Navigation.Identity());
	Navigation.Cancel();
	HasPreparedRoute = false;
}
