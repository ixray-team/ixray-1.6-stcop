#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttackAnimation.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Behaviors/CELFlyingAttackBehavior.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"

// Связывает фазы налёта с движением и проверкой фактического контакта.
CELFlyingAttackBehavior::CELFlyingAttackBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, CELFlyingAttackAnimation& FlightAnimation)
	: CELFlyingBehaviorBase(OwnerAdapter, FlightSettings, FlightMovement), Animation(FlightAnimation) {}

// Снимает разрешение на контакт с целью и освобождает заявки схемы.
void CELFlyingAttackBehavior::Exit()
{
	Movement.SetContactTarget(u16(-1));
	Animation.Fly();
	CELFlyingBehaviorBase::Exit();
}

// Выбирает индивидуальное направление и размеры захода без общего для стаи угла.
void CELFlyingAttackBehavior::Enter()
{
	CELFlyingBehaviorBase::Enter();
	Animation.Fly();
	Phase = PlannedPhase = ActivePhase = EELAttackPhase::Climb;
	RefreshTarget();
	PassRadius = Settings.OrbitRadius * Random.randF(Settings.AttackApproachRatio, 1.f);
	LastCorrectionAt = 0;
	FailedPlans = 0;
	NeedsDifferentEntry = false;
	IsCorrectingDive = false;
	CorrectionAttempts = 0;
	PrefersDivePass = Random.randF() < Settings.AttackDiveProbability;
	SelectEntryPhase();
}

// При смене противника отменяет будущую заявку, сохраняя весь текущий безопасный участок движения.
void CELFlyingAttackBehavior::SetObservation(const SELObservation* TargetObservation)
{
	const bool HasNewTarget = TargetObservation && (!HasTarget || Observation.TargetIdentifier != TargetObservation->TargetIdentifier);
	if (HasNewTarget)
	{
		Movement.SetContactTarget(u16(-1));
		CancelRouteRequest();
		HasPreparedRoute = false;
		Phase = PlannedPhase = ActivePhase = EELAttackPhase::Climb;
		TargetSampleAt = 0;
		LastVisibleAt = 0;
		FailedPlans = 0;
		NeedsDifferentEntry = false;
		IsCorrectingDive = false;
		CorrectionAttempts = 0;
		TargetVelocity.set(0.f, 0.f, 0.f);
		AimPosition = TargetObservation->Position;
	}
	CELFlyingBehaviorBase::SetObservation(TargetObservation);
	RefreshTarget();
	if (HasNewTarget)
	{
		SelectEntryPhase();
	}
}

// Выбирает краткий вход из текущего направления, не заставляя уже готового к удару летуна делать лишний верхний круг.
void CELFlyingAttackBehavior::SelectEntryPhase()
{
	const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	const Fvector Forward = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Fvector ToTarget = Fvector().sub(AimPosition, Origin);
	const float Distance = ToTarget.magnitude();
	const float TurnRadius = std::max(Settings.CollisionRadius, Movement.PlanningSpeed() / Settings.TurnRate);
	ToTarget.normalize_safe();
	if (Movement.TransitionBodyRadius() > 0.f)
	{
		Phase = EELAttackPhase::Climb;
	}
	else if (Attack.CooldownRemaining() > Distance / Movement.PlanningSpeed())
	{
		Phase = EELAttackPhase::Exit;
	}
	else if (!NeedsDifferentEntry && Distance >= TurnRadius && (Forward.dotproduct(ToTarget) > 0.f || ToTarget.y < -.5f))
	{
		Phase = EELAttackPhase::Dive;
	}
	else
	{
		Phase = EELAttackPhase::Approach;
	}
	const float Height = Origin.y + CELFlyingAttack::LocalContactPoint(Adapter.Monster()).y - AimPosition.y;
	IsDivePass = Height > 2.f * TurnRadius || (Height > TurnRadius && PrefersDivePass);
}

// Уточняет центр только видимой цели; скрытая цель остаётся в последней известной точке.
void CELFlyingAttackBehavior::RefreshTarget()
{
	CEntityAlive* TargetEntity = HasTarget ? ExtendedEntityes::FindEntity(Observation.TargetIdentifier) : nullptr;
	HasVisibleTarget = TargetEntity && Adapter.IsEnemy(*TargetEntity) && Adapter.CanSee(*TargetEntity);
	if (!HasVisibleTarget)
	{
		// Плановая память может быть старше последнего покадрового подтверждения того же противника.
		if (!LastVisibleAt || Observation.ObservedAt > LastVisibleAt)
		{
			AimPosition = Observation.Position;
		}
		TargetVelocity.set(0.f, 0.f, 0.f);
		TargetSampleAt = 0;
		return;
	}
	Fvector CurrentCenter;
	TargetEntity->Center(CurrentCenter);
	const u32 CurrentTime = Device.dwTimeGlobal;
	if (TargetSampleAt && CurrentTime > TargetSampleAt)
	{
		const float SampleTime = float(CurrentTime - TargetSampleAt) * .001f;
		const Fvector SampleVelocity = Fvector().sub(CurrentCenter, AimPosition).div(SampleTime);
		TargetVelocity.lerp(TargetVelocity, SampleVelocity, 1.f - expf(-SampleTime / .1f));
	}
	AimPosition = CurrentCenter;
	Observation.Position = TargetEntity->Position();
	TargetSampleAt = CurrentTime;
	LastVisibleAt = CurrentTime;
}

// Готовит коррекцию от будущего узла, сохраняя весь старый путь до принятия новой ветки.
void CELFlyingAttackBehavior::CorrectDive()
{
	if (Movement.IsContinuityFlight())
	{
		return;
	}
	if (IsCorrectingDive && (Navigation.WasBlocked() || !Movement.CanSpliceAt(CorrectionStart)))
	{
		// Опоздавшая ветка не должна возвращать летуна назад или оставлять его на коротком префиксе.
		CancelRouteRequest();
		HasPreparedRoute = false;
		IsCorrectingDive = false;
		Phase = EELAttackPhase::Exit;
		RetryAfter = 0;
	}
	// Потеря обзора запрещает удар, но не отменяет уже проверенный пролёт по сохранённой точке.
	if (Movement.IsAwaitingTail() || !HasVisibleTarget || HasHitPass || IsCorrectingDive || CorrectionAttempts >= 2 || ActivePhase != EELAttackPhase::Dive || Device.dwTimeGlobal - LastCorrectionAt < Settings.RouteRetryDelay)
	{
		return;
	}
	if (ActiveTargetPosition.distance_to_sqr(AimPosition) <= _sqr(Settings.AttackReplanDistance))
	{
		return;
	}
	if (!Movement.CanSpliceAt(ActiveContact))
	{
		return;
	}
	const float Lookahead = std::max(Settings.CollisionRadius * 2.f, Movement.GetSpeed() * Settings.InterruptLookahead);
	if (Adapter.Monster().Position().distance_to(ActiveContact) <= Lookahead || !Movement.PreviewHandoff(Lookahead, CorrectionStart, CorrectionDirection))
	{
		return;
	}
	CancelRouteRequest();
	HasPreparedRoute = false;
	Phase = EELAttackPhase::Dive;
	ActiveTargetPosition = AimPosition;
	LastCorrectionAt = Device.dwTimeGlobal;
	RetryAfter = 0;
	IsCorrectingDive = true;
	++CorrectionAttempts;
}

// Показывает фактическую фазу налёта и отделяет отсутствие зрения от ожидания маршрута.
const char* CELFlyingAttackBehavior::FlightDescription() const
{
	if (ActivePhase == EELAttackPhase::Dive && !Movement.IsAwaitingTail() && !Movement.CanSpliceAt(ActiveContact))
	{
		return "Выход из налёта";
	}
	if (!HasVisibleTarget)
	{
		if (ActivePhase == EELAttackPhase::Dive && Movement.HasRoute())
		{
			return "Завершение налёта по сохранённой точке: удар без зрения запрещён";
		}
		return "Известное место цели: нет зрительного подтверждения для удара";
	}
	if (IsCorrectingDive)
	{
		return "Уточнение налёта: выполняется сохранённый участок";
	}
	switch (ActivePhase)
	{
		case EELAttackPhase::Climb: return "Набор высоты перед налётом";
		case EELAttackPhase::Approach: return "Снижающийся заход на цель";
		case EELAttackPhase::Dive: return ActiveDivePass ? "Пикирование: атака цели" : "Полувираж: атака цели";
		case EELAttackPhase::Exit: return "Выход из налёта";
	}
	return "Полёт к цели";
}

// Уточняет цель до приёма фонового результата и планирует следующую фазу.
void CELFlyingAttackBehavior::Update(float DeltaTime)
{
	RefreshTarget();
	CorrectDive();
	CELFlyingBehaviorBase::Update(DeltaTime);
	if (Validation.State == EELBehaviorState::Running)
	{
		Validation.Reason = FlightDescription();
	}
}

// Запоминает начало покадрового контакта; проверка удара выполняется после фактического движения.
void CELFlyingAttackBehavior::FrameUpdate(float DeltaTime)
{
	RefreshTarget();
	CorrectDive();
	if (Movement.IsContinuityFlight())
	{
		Animation.Fly();
	}
	HasContactSample = IsAttacking() && !HasHitPass;
	if (HasContactSample)
	{
		Adapter.Monster().XFORM().transform_tiny(PreviousContact, CELFlyingAttack::LocalContactPoint(Adapter.Monster()));
		PreviousTargetTransform = ExtendedEntityes::FindEntity(Observation.TargetIdentifier)->XFORM();
	}
	if (HasVisibleTarget && CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Search;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.TargetIdentifier = Observation.TargetIdentifier;
		Adapter.Monster().XFORM().transform_tiny(Event.Start, CELFlyingAttack::LocalContactPoint(Adapter.Monster()));
		Event.Finish = AimPosition;
		Event.Radius = Settings.AttackDistance;
		Event.Label = "Центр видимой цели налёта";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	CELFlyingBehaviorBase::FrameUpdate(DeltaTime);
}

// Строит конечный выход по текущему курсу с подъёмом только при выравнивании после переворота.
void CELFlyingAttackBehavior::ChooseDeparture(const Fvector& Origin)
{
	Fvector Forward = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Forward.y = 0.f;
	Forward.normalize_safe();
	const float Distance = std::max(Settings.RecoveryForward, std::max(2.f * Movement.PlanningSpeed() / Settings.TurnRate, Movement.PlanningSpeed() * Attack.CooldownRemaining()));
	if (Movement.InConfinedSpace())
	{
		const float Radius = std::max(Settings.CollisionRadius, Movement.PlanningSpeed() / Settings.TurnRate);
		const Fvector Side = Fvector().set(Forward.z, 0.f, -Forward.x);
		const float Sign = Side.dotproduct(Fvector().sub(AimPosition, Origin)) >= 0.f ? 1.f : -1.f;
		GoalPosition.mad(Origin, Forward, 2.f * Radius);
		GoalPosition.mad(Side, Sign * Radius * .5f);
		GoalDirection.mad(Forward, Side, Sign * .5f).normalize_safe();
		return;
	}
	GoalPosition.mad(Origin, Forward, Distance);
	GoalPosition.y += Phase == EELAttackPhase::Climb ? Settings.RecoveryClimb : 0.f;
	GoalDirection.set(Forward.x, Phase == EELAttackPhase::Climb ? Settings.AttackPulloutPitch : 0.f, Forward.z).normalize_safe();
}

// Направляет промежуточный заход к цели с реальным снижением от высоты виража.
void CELFlyingAttackBehavior::ChooseApproach(const Fvector& Origin)
{
	Fvector Forward = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Forward.y = 0.f;
	Forward.normalize_safe();
	Fvector ToTarget = Fvector().sub(AimPosition, Origin);
	ToTarget.y = 0.f;
	const float Distance = ToTarget.magnitude();
	ToTarget.normalize_safe();
	const float TurnRadius = std::max(Settings.CollisionRadius, Movement.PlanningSpeed() / Settings.TurnRate);
	if (Forward.dotproduct(ToTarget) <= 0.f || NeedsDifferentEntry)
	{
		// Цель позади либо прежняя сторона закрыта: один разворот, затем новая оценка, без орбиты вокруг цели.
		Fvector Side = Fvector().set(Forward.z, 0.f, -Forward.x);
		float SideSign = Side.dotproduct(ToTarget) >= 0.f ? 1.f : -1.f;
		if (FailedPlans % 2)
		{
			SideSign = -SideSign;
		}
		const float Radius = Movement.InConfinedSpace() ? TurnRadius * (1.f + .25f * float(FailedPlans % 3)) : TurnRadius + float(FailedPlans / 2) * Settings.RecoveryStep + (NeedsDifferentEntry ? PassRadius * Settings.AttackArcSideRatio : 0.f);
		GoalPosition.mad(Origin, Side, 2.f * Radius * SideSign);
		GoalDirection.sub(AimPosition, GoalPosition);
		GoalDirection.y = 0.f;
		GoalDirection.normalize_safe();
	}
	else
	{
		// При открытом направлении уменьшаем высоту и расстояние, вместо ещё одного полного круга.
		const float Shoulder = std::min(Movement.InConfinedSpace() ? 2.f * TurnRadius : PassRadius * Settings.AttackApproachRatio, Distance * .5f);
		GoalPosition.mad(AimPosition, ToTarget, -Shoulder);
		const float Height = std::max(0.f, Origin.y + CELFlyingAttack::LocalContactPoint(Adapter.Monster()).y - AimPosition.y);
		GoalPosition.y = AimPosition.y + (IsDivePass ? Height * Settings.AttackApproachHeightRatio : std::min(Height, Settings.CollisionRadius + Settings.GroundClearance));
		GoalDirection = ToTarget;
		GoalPosition = CELFlyingAttack::RootAtContact(Adapter.Monster(), GoalPosition, GoalDirection);
	}
	if (Movement.TransitionBodyRadius() > 0.f)
	{
		GoalPosition.y = std::max(GoalPosition.y, Observation.Position.y + Movement.TransitionBodyRadius() + Settings.GroundClearance);
	}
}

// Назначает контакт у центра цели с ограниченным упреждением и зазором под реальное тело летуна.
void CELFlyingAttackBehavior::ChooseContact(const Fvector& Origin)
{
	const float ExpectedSpeed = std::max(Movement.PlanningSpeed() * Settings.TurningSpeedRatio, Movement.GetSpeed());
	const float WaitTime = IsCorrectingDive ? Adapter.Monster().Position().distance_to(Origin) / ExpectedSpeed : Movement.RemainingDistance() / ExpectedSpeed;
	float TravelTime = Origin.distance_to(AimPosition) / ExpectedSpeed;
	Fvector Lead;
	for (u32 Iteration = 0; Iteration < 3; ++Iteration)
	{
		// Упреждение само меняет длину подлёта; три дешёвых уточнения вместо кадрового поиска пути.
		Lead.mul(TargetVelocity, std::min(Settings.AttackLeadTime, WaitTime + TravelTime));
		TravelTime = Origin.distance_to(Fvector().add(AimPosition, Lead)) / ExpectedSpeed;
	}
	Lead.mul(TargetVelocity, std::min(Settings.AttackLeadTime, WaitTime + TravelTime));
	if (Lead.magnitude() > Settings.OrbitRadius)
	{
		Lead.normalize_safe().mul(Settings.OrbitRadius);
	}
	const Fvector ContactPoint = Fvector().add(AimPosition, Lead);
	GoalDirection.sub(ContactPoint, Origin);
	GoalDirection.y = 0.f;
	if (GoalDirection.square_magnitude() <= EPS_L)
	{
		// Для цели строго снизу сохраняем курс захода: нулевая касательная не является допустимым виражом.
		GoalDirection = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
		GoalDirection.y = 0.f;
		if (GoalDirection.square_magnitude() <= EPS_L)
		{
			GoalDirection.set(0.f, 0.f, 1.f);
		}
	}
	GoalDirection.normalize_safe();
	GoalPosition = CELFlyingAttack::RootAtContact(Adapter.Monster(), ContactPoint, GoalDirection);
	float FloorHeight = Observation.Position.y;
	Fvector SupportPoint;
	Fvector SupportNormal;
	Fvector SupportOrigin = GoalPosition;
	SupportOrigin.y = ContactPoint.y;
	if (CELFlyingGeometry::QuerySupport(SupportOrigin, Settings.Altitude + Settings.CollisionCenterHeight, SupportPoint, SupportNormal, &Adapter.Monster()) == EELGeometryResult::Clear)
	{
		FloorHeight = SupportPoint.y;
	}
	GoalPosition.y = std::max(GoalPosition.y, FloorHeight + Settings.CollisionRadius - Settings.CollisionCenterHeight + Settings.GroundClearance);
}

// Формирует отдельную цель фазы и сохраняет снимок, относительно которого проверяется готовый маршрут.
void CELFlyingAttackBehavior::ChooseGoal()
{
	if (!IsCorrectingDive)
	{
		SelectEntryPhase();
	}
	PlannedPhase = Phase;
	PlannedDivePass = IsDivePass;
	PlannedTargetPosition = AimPosition;
	const Fvector Origin = IsCorrectingDive ? CorrectionStart : Movement.HasRoute() ? Movement.EndPosition()
																					: Adapter.Monster().Position();
	switch (Phase)
	{
		case EELAttackPhase::Climb:
		case EELAttackPhase::Exit: ChooseDeparture(Origin); break;
		case EELAttackPhase::Approach: ChooseApproach(Origin); break;
		case EELAttackPhase::Dive:
			ChooseContact(Origin);
			PlannedContact = GoalPosition;
			ContactDirection = GoalDirection;
			// Контакт не является концом маршрута: выход проверяется до разрешения пикирования.
			GoalPosition.mad(PlannedContact, ContactDirection, std::max(Settings.RecoveryForward, 2.f * Movement.PlanningSpeed() / Settings.TurnRate));
			GoalPosition.y += std::max(Settings.RecoveryClimb, Settings.CollisionRadius + Settings.GroundClearance);
			GoalDirection.set(ContactDirection.x, Settings.AttackPulloutPitch, ContactDirection.z).normalize_safe();
			break;
	}
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);
	clamp(GoalPosition.x, Bounds.min.x, Bounds.max.x);
	clamp(GoalPosition.y, Bounds.min.y, Bounds.max.y);
	clamp(GoalPosition.z, Bounds.min.z, Bounds.max.z);
}

// Ограничивает плечо пикирования, чтобы начальная касательная не уводила кривую ниже контакта.
void CELFlyingAttackBehavior::ConfigureCurveRequest(SELNavigationRequest& Request)
{
	if (IsCorrectingDive)
	{
		Request.StartPosition = CorrectionStart;
		Request.StartDirection = CorrectionDirection;
		Request.CurveDistance = clampr(Request.StartPosition.distance_to(Request.GoalPosition) * Settings.CurveRatio, Settings.CurveMinimum, Settings.CurveMaximum);
	}
	if (PlannedPhase == EELAttackPhase::Dive)
	{
		Request.GuidePoints = {Request.StartPosition, PlannedContact, Request.GoalPosition};
		Request.GuideDirections = {Request.StartDirection, ContactDirection, Request.GoalDirection};
	}
	if (PlannedPhase != EELAttackPhase::Dive || Request.StartDirection.y >= -EPS_L)
	{
		return;
	}
	const float DescentHeight = Request.StartPosition.y - PlannedContact.y;
	if (DescentHeight > EPS_L)
	{
		Request.CurveDistance = std::min(Request.CurveDistance, DescentHeight / -Request.StartDirection.y);
	}
}

// Проверяет узел коррекции без бесконечной отмены готового захода из-за движения той же цели.
bool CELFlyingAttackBehavior::CanActivatePreparedRoute()
{
	RefreshTarget();
	if (IsCorrectingDive && !Movement.CanSpliceAt(CorrectionStart))
	{
		IsCorrectingDive = false;
		Phase = EELAttackPhase::Exit;
		return false;
	}
	// Координаты выбранного противника меняются во время фонового расчёта.
	// Проверенный пролёт остаётся допустимым; уточнение выполняется впереди атомарной заменой хвоста.
	// Смена ID отменяется в SetObservation, а смерть/отношения проверяются в Validate и TryHit.
	return true;
}

// Принимает проверенную коррекцию только стыковкой к ещё не пройденному узлу текущего пути.
bool CELFlyingAttackBehavior::ActivateReplacementRoute(SELNavigationResult& Route)
{
	return IsCorrectingDive && Movement.SpliceRoute(Route);
}

// Переключает фазу только при фактическом начале движения по принятому маршруту.
void CELFlyingAttackBehavior::OnRouteActivated()
{
	const bool IsReplacement = IsCorrectingDive;
	ActivePhase = PlannedPhase;
	ActiveDivePass = PlannedDivePass;
	if (ActivePhase == EELAttackPhase::Dive)
	{
		FailedPlans = 0;
	}
	NeedsDifferentEntry = false;
	IsCorrectingDive = false;
	if (!IsReplacement)
	{
		CorrectionAttempts = 0;
	}
	ActiveTargetPosition = PlannedTargetPosition;
	ActiveContact = PlannedContact;
	Movement.SetContactPosition(ActiveContact, ContactDirection);
	if (!IsReplacement)
	{
		HasHitPass = false;
		PassContactChecks = 0;
		PassClosestGap = FLT_MAX;
		HasReportedPass = false;
		HadContactNode = false;
	}
	Movement.SetContactTarget(ActivePhase == EELAttackPhase::Dive ? Observation.TargetIdentifier : u16(-1));
	if (ActivePhase == EELAttackPhase::Dive)
	{
		Animation.Attack();
		Adapter.Monster().XFORM().transform_tiny(PreviousContact, CELFlyingAttack::LocalContactPoint(Adapter.Monster()));
		CEntityAlive* Target = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
		HasContactSample = HasVisibleTarget && Target;
		if (HasContactSample)
		{
			PreviousTargetTransform = Target->XFORM();
		}
	}
	else
	{
		Animation.Fly();
	}
}

// Не продолжает схему для исчезнувшего, погибшего или запрещённого противника.
SELBehaviorValidation CELFlyingAttackBehavior::Validate() const
{
	CEntityAlive* TargetEntity = HasTarget ? ExtendedEntityes::FindEntity(Observation.TargetIdentifier) : nullptr;
	if (!TargetEntity || !Adapter.IsEnemy(*TargetEntity))
	{
		return {EELBehaviorState::InvalidTarget, "Цель налёта недоступна"};
	}
	if (FailedPlans >= Settings.RecoverySteps)
	{
		return {EELBehaviorState::Blocked, "Заходы перекрыты: смена манёвра через восстановление"};
	}
	return CELFlyingBehaviorBase::Validate();
}

// Освобождает отказавший налёт для штатной схемы воздушного восстановления.
EELRecovery CELFlyingAttackBehavior::Recover(const SELBehaviorValidation& Failure)
{
	return EELRecovery::Release;
}

// Проверяет пройденный контакт до переключения фазы, затем принимает готовый следующий участок.
void CELFlyingAttackBehavior::AfterMovement()
{
	if (HasContactSample && !HasHitPass && HasVisibleTarget && ActivePhase == EELAttackPhase::Dive && !Movement.IsContinuityFlight())
	{
		++PassContactChecks;
		HasHitPass = Attack.TryHit(Adapter, Settings, Observation.TargetIdentifier, PreviousContact, PreviousTargetTransform);
		Adapter.Monster().XFORM().transform_tiny(PreviousContact, CELFlyingAttack::LocalContactPoint(Adapter.Monster()));
		CEntityAlive* Target = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
		HasContactSample = Target != nullptr;
		if (Target)
		{
			PreviousTargetTransform = Target->XFORM();
		}
	}
	if (ActivePhase == EELAttackPhase::Dive && !Movement.IsAwaitingTail() && !Movement.CanSpliceAt(ActiveContact))
	{
		Animation.Fly();
	}
	DebugPass();
	CELFlyingBehaviorBase::AfterMovement();
}

// Не повторяет бесконечно ту же отвергнутую сторону захода; число попыток ограничено настройками восстановления.
void CELFlyingAttackBehavior::OnRouteRejected()
{
	++FailedPlans;
	NeedsDifferentEntry = true;
	IsCorrectingDive = false;
	CorrectionAttempts = 2;
}

// Выводит один итог захода; отправка HitEntity и подтверждённая потеря здоровья намеренно различаются.
void CELFlyingAttackBehavior::DebugPass()
{
	if (ActivePhase != EELAttackPhase::Dive || !CELSharedDebug::Instance().IsEnabled())
	{
		return;
	}
	HadContactNode = HadContactNode || Movement.CanSpliceAt(ActiveContact);
	CEntityAlive* Target = HasVisibleTarget ? ExtendedEntityes::FindEntity(Observation.TargetIdentifier) : nullptr;
	if (Target)
	{
		Fvector Point;
		Adapter.Monster().XFORM().transform_tiny(Point, CELFlyingAttack::LocalContactPoint(Adapter.Monster()));
		Fmatrix Inverse;
		Inverse.invert(Target->XFORM());
		Inverse.transform_tiny(Point);
		Fvector Center;
		Fvector HalfSize;
		Target->BoundingBox().get_CD(Center, HalfSize);
		HalfSize.mul(Settings.AttackTargetScale);
		const Fvector Minimum = Fvector().sub(Center, HalfSize);
		const Fvector Maximum = Fvector().add(Center, HalfSize);
		Fvector Closest = Point;
		clamp(Closest.x, Minimum.x, Maximum.x);
		clamp(Closest.y, Minimum.y, Maximum.y);
		clamp(Closest.z, Minimum.z, Maximum.z);
		PassClosestGap = std::min(PassClosestGap, Point.distance_to(Closest));
	}
	if (!HasReportedPass && !Movement.IsAwaitingTail() && !Movement.CanSpliceAt(ActiveContact))
	{
		Msg("[EL flight attack] id=%u target=%u hit_sent=%u checks=%u closest_gap=%.3f contact_distance=%.3f visible=%u contact_node=%u corrections=%u target_speed=%.2f reason=%s",
			Adapter.Monster().ID(),
			Observation.TargetIdentifier,
			unsigned(HasHitPass),
			PassContactChecks,
			PassClosestGap == FLT_MAX ? -1.f : PassClosestGap,
			Settings.AttackDistance,
			unsigned(HasVisibleTarget),
			unsigned(HadContactNode),
			CorrectionAttempts,
			TargetVelocity.magnitude(),
			PassContactChecks ? Attack.LastContactResult() : "Нет зрительного подтверждения контакта");
		HasReportedPass = true;
	}
}
