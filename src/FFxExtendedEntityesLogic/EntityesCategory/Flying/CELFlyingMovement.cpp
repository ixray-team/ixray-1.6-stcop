#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingMovement.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/movement_manager.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingObstacles.h"


namespace ExtendedEntityesFlightMovement
{
// Находит точку впереди по ломаной на заданной длине пути.
Fvector LookaheadEndpoint(const SELNavigationResult& Route, u32 Segment, const Fvector& StartPosition, float Distance, u32& LastPoint)
{
	Fvector EndPosition = StartPosition;
	float Remaining = Distance;
	Fvector Direction;
	Fvector Previous = StartPosition;
	float Length;
	u32 PointIndex;
	LastPoint = Segment;
	for (PointIndex = Segment; PointIndex < Route.Points.size() && Remaining > EPS_L; ++PointIndex)
	{
		Direction.sub(Route.Points[PointIndex], Previous);
		Length = Direction.magnitude();
		if (Length <= EPS_L)
		{
			continue;
		}
		Direction.div(Length);
		EndPosition.mad(Previous, Direction, std::min(Remaining, Length));
		Previous = EndPosition;
		Remaining -= Length;
		LastPoint = PointIndex;
	}
	return EndPosition;
}
// Оценивает отклонение кривого участка от прямого проверочного коридора.
float EnvelopeDeviation(const SELNavigationResult& Route, u32 Segment, u32 LastPoint, const Fvector& StartPosition, const Fvector& EndPosition)
{
	Fvector Direction;
	u32 PointIndex;
	// Include the curved centerline in the straight lookahead envelope.
	Direction.sub(EndPosition, StartPosition);
	const float LengthSquared = Direction.square_magnitude();
	Fvector Relative;
	Fvector Closest;
	float Fraction;
	float DeviationSquared = 0.f;
	for (PointIndex = Segment; PointIndex < LastPoint && LengthSquared > EPS_L; ++PointIndex)
	{
		Relative.sub(Route.Points[PointIndex], StartPosition);
		Fraction = Relative.dotproduct(Direction) / LengthSquared;
		clamp(Fraction, 0.f, 1.f);
		Closest.mad(StartPosition, Direction, Fraction);
		DeviationSquared = std::max(DeviationSquared, Closest.distance_to_sqr(Route.Points[PointIndex]));
	}
	return _sqrt(DeviationSquared);
}
} // namespace ExtendedEntityesFlightMovement

// Связывает движение по проверенной воздушной траектории с необходимыми службами и состоянием владельца.
CELFlyingMovement::CELFlyingMovement(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings) : Adapter(OwnerAdapter), Settings(FlightSettings) {}
// Завершает движение по проверенной воздушной траектории и освобождает связанные ресурсы.
CELFlyingMovement::~CELFlyingMovement()
{
	Stop();
}
// Отклоняет наземное назначение: летуну нужен проверенный воздушный маршрут.
bool CELFlyingMovement::MoveTo(const Fvector& TargetPosition, u32 TargetVertex, bool IsRunning)
{
	return false;
}
// Задаёт допустимый индивидуальный множитель скорости.
void CELFlyingMovement::SetSpeedScale(float Scale)
{
	SpeedScale = _valid(Scale) ? Scale : 1.f;
	clamp(SpeedScale, .1f, 2.f);
}

// Останавливает движение и отменяет связанные проверки маршрута.
void CELFlyingMovement::Stop()
{
	CELSharedFlyingObstacles::Cancel(ActiveRoute.Identity);
	HasDynamicObstacle = false;
	IsBraking = false;
	ActiveRoute = SELNavigationResult();
	HasContinuityRoute = false;
	SlowSince = 0;
	Velocity.set(0.f, 0.f, 0.f);
	Speed = 0.f;
	LastStopReason = "Явная остановка или снятие управления";
}
// Сообщает, остались ли непройденные точки активного маршрута.
bool CELFlyingMovement::HasRoute() const
{
	return Segment < ActiveRoute.Points.size();
}

bool CELFlyingMovement::IsStalled() const
{
	return SlowSince && Device.dwTimeGlobal - SlowSince >= 300u;
}
// Возвращает долю пройденных точек текущего маршрута.
float CELFlyingMovement::Progress() const
{
	return ActiveRoute.Points.empty() ? 0.f : float(Segment) / float(ActiveRoute.Points.size());
}
// Возвращает направление скорости либо текущую ось сущности при остановке.
Fvector CELFlyingMovement::Direction() const
{
	return Velocity.square_magnitude() > EPS_L ? Fvector(Velocity).normalize_safe() : Adapter.Monster().XFORM().k;
}
// Возвращает конечную точку активного маршрута.
const Fvector& CELFlyingMovement::EndPosition() const
{
	return ActiveRoute.Points.back();
}
// Возвращает конечную касательную активного маршрута.
const Fvector& CELFlyingMovement::EndDirection() const
{
	return FinalDirection;
}

// Принимает проверенную последовательность точек и заменяет предыдущий маршрут.
void CELFlyingMovement::SetRoute(SELNavigationResult Route, bool IsContinuity)
{
	if (!Route.HasPath || Route.Points.size() < 2)
	{
		return;
	}
	CELSharedFlyingObstacles::Cancel(ActiveRoute.Identity);
	HasDynamicObstacle = false;
	IsBraking = false;
	HasContinuityRoute = IsContinuity;
	SlowSince = 0;
	FinalDirection.sub(Route.Points.back(), Route.Points[Route.Points.size() - 2]).normalize_safe();
	ActiveRoute = std::move(Route);
	Segment = 1;
}

// Завершает отменённую выдачу на последнем проверенном узле, сохраняя пройденный курс и скорость.
void CELFlyingMovement::CloseRouteStream(const SELNavigationIdentity& Identity)
{
	if (ActiveRoute.Identity == Identity)
	{
		ActiveRoute.IsPartial = false;
	}
}

// Измеряет оставшуюся длину реально принятого пути, включая неполный текущий отрезок.
float CELFlyingMovement::RemainingDistance() const
{
	float Distance = 0.f;
	Fvector Previous = Adapter.Monster().Position();
	for (u32 Index = Segment; Index < ActiveRoute.Points.size(); ++Index)
	{
		Distance += Previous.distance_to(ActiveRoute.Points[Index]);
		Previous = ActiveRoute.Points[Index];
	}
	return Distance;
}

// Продлевает ту же сертифицированную кривую без сброса скорости, сегмента и фазы атаки.
bool CELFlyingMovement::AppendRouteTail(const SELNavigationResult& Route)
{
	if (!Route.IsContinuation || !ActiveRoute.IsPartial || !Route.HasPath || ActiveRoute.Points.empty() || Route.Points.size() < 2 || !(ActiveRoute.Identity == Route.Identity) || !ActiveRoute.Points.back().similar(Route.Points.front(), EPS_L))
	{
		return false;
	}
	ActiveRoute.Points.insert(ActiveRoute.Points.end(), Route.Points.begin() + 1, Route.Points.end());
	ActiveRoute.IsPartial = Route.IsPartial;
	FinalDirection.sub(Route.Points.back(), Route.Points[Route.Points.size() - 2]).normalize_safe();
	return true;
}

// Обновляет движение по проверенной воздушной траектории и обрабатывает текущий результат выполнения.
void CELFlyingMovement::Update(float DeltaTime)
{
	RemainingTime = 0.f;
	if (CELSharedDebug::Instance().IsDrawEnabled() && HasRoute())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Route;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.Start = Adapter.Monster().Position();
		Event.Finish = ActiveRoute.Points.back();
		Event.Points = ActiveRoute.Points;
		Event.Radius = .25f;
		Event.Label = "Исполняемый маршрут";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}

	if (DeltaTime <= 0.f)
	{
		return;
	}
	if (!HasRoute())
	{
		if (!ActiveRoute.Points.empty())
		{
			LastStopReason = ActiveRoute.IsPartial ? "Закончился проверенный участок: ожидание хвоста" : "Маршрут закончился: продолжение не готово";
		}
		Velocity.set(0.f, 0.f, 0.f);
		return;
	}
	float ClearDistance;
	if (CELSharedFlyingObstacles::IsBlocked(ActiveRoute.Identity, LastObstacle, ClearDistance))
	{
		ObstacleUntil = Device.dwTimeGlobal + Settings.ObstacleMemory;
		ClearDistance -= Settings.GroundClearance;
		if (ClearDistance <= EPS_L)
		{
			Stop();
			LastStopReason = "Динамическое препятствие: нет свободного тормозного пути";
			HasDynamicObstacle = true;
			return;
		}
		RetainRoutePrefix(ClearDistance);
		HasDynamicObstacle = true;
		IsBraking = true;
	}
	else if (IsBraking)
	{
		IsBraking = false;
		HasDynamicObstacle = false;
		ObstacleUntil = 0;
	}
	PublishObstacleProbe();
	clamp(DeltaTime, 0.f, .1f);
	CBaseMonster& OwnerMonster = Adapter.Monster();
	Fvector DirectionToPoint;
	DirectionToPoint.sub(ActiveRoute.Points[Segment], OwnerMonster.Position());
	DirectionToPoint.normalize_safe();
	const float Alignment = std::max(-1.f, std::min(1.f, OwnerMonster.XFORM().k.dotproduct(DirectionToPoint)));
	const float TurnRatio = 1.f - (1.f - Settings.TurningSpeedRatio) * std::min(1.f, (1.f - Alignment) * 2.f);
	const float VerticalRatio = DirectionToPoint.y > 0.f ? 1.f + DirectionToPoint.y * (Settings.ClimbSpeedRatio - 1.f) : 1.f - DirectionToPoint.y * (Settings.DiveSpeedRatio - 1.f);
	float TargetSpeed = PlanningSpeed() * TurnRatio * VerticalRatio;
	if (Segment > 0 && Segment + 1 < ActiveRoute.Points.size())
	{
		Fvector Incoming = Fvector().sub(ActiveRoute.Points[Segment], ActiveRoute.Points[Segment - 1]);
		Fvector Outgoing = Fvector().sub(ActiveRoute.Points[Segment + 1], ActiveRoute.Points[Segment]);
		const float LegLength = std::min(Incoming.magnitude(), Outgoing.magnitude());
		const float ChordLength = Fvector().add(Incoming, Outgoing).magnitude();
		Incoming.normalize_safe();
		Outgoing.normalize_safe();
		float AlignmentAtJoin = Incoming.dotproduct(Outgoing);
		clamp(AlignmentAtJoin, -1.f, 1.f);
		const float TurnAngle = acosf(AlignmentAtJoin);
		if (LegLength > EPS_L && TurnAngle > EPS_L)
		{
			// Радиус описанной окружности не зависит от неравномерного шага точек у стыка.
			const float Sine = Fvector().crossproduct(Incoming, Outgoing).magnitude();
			const float TurnRadius = Sine > EPS_L ? ChordLength / (2.f * Sine) : LegLength / TurnAngle;
			TargetSpeed = std::min(TargetSpeed, Settings.TurnRate * TurnRadius);
		}
	}
	if (IsLanding || IsBraking)
	{
		const float Remaining = OwnerMonster.Position().distance_to(ActiveRoute.Points.back());
		TargetSpeed = std::min(TargetSpeed, std::max(IsBraking ? 0.f : Settings.LandingSpeedMinimum, _sqrt(2.f * Settings.Acceleration * Remaining)));
	}
	LastStopReason = IsBraking ? "Торможение перед препятствием" : IsLanding ? "Посадка"
																			 : "Движение по кривой: ограничение скорости или разгон";
	Speed += std::min(Settings.Acceleration * DeltaTime, std::max(0.f, TargetSpeed - Speed));
	if (Speed > TargetSpeed)
	{
		Speed = std::max(TargetSpeed, Speed - Settings.Acceleration * DeltaTime);
	}
	float RemainingDistance = Speed * DeltaTime;
	float SegmentDistance;
	float TravelDistance;
	Fvector PreviousPosition = OwnerMonster.Position();
	Fvector NewPosition = PreviousPosition;
	while (RemainingDistance > EPS_L && HasRoute())
	{
		DirectionToPoint.sub(ActiveRoute.Points[Segment], NewPosition);
		SegmentDistance = DirectionToPoint.magnitude();
		if (SegmentDistance <= EPS_L)
		{
			++Segment;
			continue;
		}
		DirectionToPoint.div(SegmentDistance);
		TravelDistance = std::min(RemainingDistance, SegmentDistance);
		NewPosition.mad(DirectionToPoint, TravelDistance);
		RemainingDistance -= TravelDistance;
		if (TravelDistance >= SegmentDistance)
		{
			++Segment;
		}
	}
	if (CELSharedAnomalies::Crosses(OwnerMonster.ID(), PreviousPosition, NewPosition))
	{
		Stop();
		LastStopReason = "Пересечение известной аномалии";
		return;
	}
	RemainingTime = !HasRoute() && !IsBraking && !IsLanding && Speed > EPS_L ? RemainingDistance / Speed : 0.f;
	Velocity.sub(NewPosition, PreviousPosition).div(std::max(EPS_L, DeltaTime - RemainingTime));
	if (Velocity.square_magnitude() < .04f)
	{
		if (!SlowSince)
		{
			SlowSince = std::max(1u, Device.dwTimeGlobal);
		}
	}
	else
	{
		SlowSince = 0;
	}
	if (DirectionToPoint.square_magnitude() <= EPS_L)
	{
		DirectionToPoint = FinalDirection;
	}
	Orient(DirectionToPoint, NewPosition, std::max(0.f, DeltaTime - RemainingTime));
	float Heading;
	float Pitch;
	float Bank;
	OwnerMonster.XFORM().getHPB(Heading, Pitch, Bank);
	OwnerMonster.Position() = NewPosition;
	OwnerMonster.movement().m_body.current.yaw = Heading;
	OwnerMonster.movement().m_body.current.pitch = Pitch;
	OwnerMonster.movement().m_body.target = OwnerMonster.movement().m_body.current;
	OwnerMonster.spatial_move();
	if (!HasRoute())
	{
		CELSharedFlyingObstacles::Cancel(ActiveRoute.Identity);
	}
}

// Оставляет безопасный участок старого маршрута без сброса текущей скорости.
void CELFlyingMovement::RetainRoutePrefix(float Distance)
{
	if (!HasRoute() || !_valid(Distance) || Distance <= EPS_L)
	{
		return;
	}
	SELNavigationResult Prefix;
	Prefix.Identity = ActiveRoute.Identity;
	Prefix.InvertedBodyRadius = ActiveRoute.InvertedBodyRadius;
	Prefix.CanInvert = ActiveRoute.CanInvert;
	Prefix.HasPath = true;
	Prefix.Points.push_back(Adapter.Monster().Position());
	Fvector Previous = Prefix.Points.back();
	Fvector Direction;
	Fvector Endpoint;
	float SegmentLength;
	u32 PointIndex;
	for (PointIndex = Segment; PointIndex < ActiveRoute.Points.size() && Distance > EPS_L; ++PointIndex)
	{
		Direction.sub(ActiveRoute.Points[PointIndex], Previous);
		SegmentLength = Direction.magnitude();
		if (SegmentLength <= EPS_L)
		{
			continue;
		}
		Direction.div(SegmentLength);
		Endpoint.mad(Previous, Direction, std::min(Distance, SegmentLength));
		Prefix.Points.push_back(Endpoint);
		Previous = Endpoint;
		Distance -= SegmentLength;
	}
	if (Prefix.Points.size() > 1)
	{
		const u32 PreviousSlowSince = SlowSince;
		SetRoute(std::move(Prefix), HasContinuityRoute);
		SlowSince = PreviousSlowSince;
	}
}

// Выбирает ещё не пройденный узел стыка впереди, не обрезая исполняемый путь.
bool CELFlyingMovement::PreviewHandoff(float Distance, Fvector& Position, Fvector& Direction) const
{
	if (!HasRoute() || ActiveRoute.CanInvert || !_valid(Distance) || Distance <= EPS_L)
	{
		return false;
	}
	Fvector Previous = Adapter.Monster().Position();
	u32 PointIndex;
	for (PointIndex = Segment; PointIndex + 1 < ActiveRoute.Points.size(); ++PointIndex)
	{
		Distance -= Previous.distance_to(ActiveRoute.Points[PointIndex]);
		if (Distance <= 0.f)
		{
			Position = ActiveRoute.Points[PointIndex];
			Direction.sub(Position, Previous).normalize_safe();
			return Direction.square_magnitude() > EPS_L;
		}
		Previous = ActiveRoute.Points[PointIndex];
	}
	return false;
}

// Проверяет, остался ли узел стыка впереди на текущем проверенном пути.
bool CELFlyingMovement::CanSpliceAt(const Fvector& Position) const
{
	u32 PointIndex;
	for (PointIndex = Segment; PointIndex < ActiveRoute.Points.size(); ++PointIndex)
	{
		if (ActiveRoute.Points[PointIndex].similar(Position, EPS_L))
		{
			return true;
		}
	}
	return false;
}

// Атомарно заменяет только хвост пути: старый проверенный префикс и скорость сохраняются.
bool CELFlyingMovement::SpliceRoute(SELNavigationResult Route)
{
	if (!Route.HasPath || Route.Points.size() < 2 || ActiveRoute.CanInvert || Route.CanInvert || !CanSpliceAt(Route.Points.front()))
	{
		return false;
	}
	xr_vector<Fvector> Joined;
	Joined.push_back(Adapter.Monster().Position());
	u32 PointIndex;
	for (PointIndex = Segment; PointIndex < ActiveRoute.Points.size(); ++PointIndex)
	{
		Joined.push_back(ActiveRoute.Points[PointIndex]);
		if (ActiveRoute.Points[PointIndex].similar(Route.Points.front(), EPS_L))
		{
			break;
		}
	}
	Joined.insert(Joined.end(), Route.Points.begin() + 1, Route.Points.end());
	Route.Points = std::move(Joined);
	Route.InvertedBodyRadius = std::max(Route.InvertedBodyRadius, ActiveRoute.InvertedBodyRadius);
	SetRoute(std::move(Route));
	return true;
}

// Добавляет ещё актуальные препятствия в снимок следующего обхода.
void CELFlyingMovement::AppendObstacles(xr_vector<SELNavigationObstacle>& Obstacles) const
{
	if (s32(Device.dwTimeGlobal - ObstacleUntil) < 0)
	{
		Obstacles.push_back(LastObstacle);
	}
}
// Публикует коридор впереди с запасом на торможение и кривизну.
void CELFlyingMovement::PublishObstacleProbe()
{
	SELFlyingCorridor Corridor;
	Corridor.Identity = ActiveRoute.Identity;
	Corridor.Start = Adapter.Monster().Position();
	Corridor.End = Corridor.Start;
	Corridor.CenterHeight = ActiveRoute.InvertedBodyRadius > 0.f ? 0.f : Settings.CollisionCenterHeight;
	Corridor.Radius = ActiveRoute.InvertedBodyRadius > 0.f ? ActiveRoute.InvertedBodyRadius : Settings.CollisionRadius;
	Corridor.ContactTarget = ContactTarget;
	Corridor.IsLanding = IsLanding;
	u32 LastPoint;
	Corridor.End = ExtendedEntityesFlightMovement::LookaheadEndpoint(ActiveRoute, Segment, Corridor.Start, std::max(Settings.CollisionRadius * 2.f, (Speed * Speed / (2.f * Settings.Acceleration)) + Settings.Speed * Settings.ObstacleLookahead), LastPoint);
	Corridor.Radius += ExtendedEntityesFlightMovement::EnvelopeDeviation(ActiveRoute, Segment, LastPoint, Corridor.Start, Corridor.End);
	CELSharedFlyingObstacles::Publish(Corridor);
}

// Сглаживает ориентацию по XYZ; при полной петле переносит поперечную ось без разрыва углов в верхней точке.
void CELFlyingMovement::Orient(const Fvector& Direction, const Fvector& NewPosition, float DeltaTime)
{
	CBaseMonster& OwnerMonster = Adapter.Monster();
	if (ActiveRoute.CanInvert)
	{
		Fmatrix Desired;
		Desired.identity();
		Desired.k = Direction;
		Desired.i = OwnerMonster.XFORM().i;
		Desired.i.mad(Direction, -Desired.i.dotproduct(Direction)).normalize_safe();
		if (Desired.i.square_magnitude() < EPS_L)
		{
			Desired.i.crossproduct(OwnerMonster.XFORM().j, Direction).normalize_safe();
		}
		Desired.j.crossproduct(Direction, Desired.i).normalize_safe();
		Fquaternion Current;
		Fquaternion Target;
		Fquaternion Blended;
		Current.set(OwnerMonster.XFORM());
		Target.set(Desired);
		float Dot = std::abs(Current.x * Target.x + Current.y * Target.y + Current.z * Target.z + Current.w * Target.w);
		clamp(Dot, 0.f, 1.f);
		const float Angle = 2.f * acosf(Dot);
		const float Response = 1.f - expf(-Settings.OrientationResponse * DeltaTime);
		const float Fraction = Angle > EPS_L ? std::min(Response, Settings.TurnRate * DeltaTime / Angle) : Response;
		Blended.slerp(Current, Target, Fraction);
		OwnerMonster.XFORM().rotation(Blended);
		return;
	}
	float Heading;
	float Pitch;
	float Bank;
	OwnerMonster.XFORM().getHPB(Heading, Pitch, Bank);
	float DesiredHeading;
	float DesiredPitch;
	Direction.getHP(DesiredHeading, DesiredPitch);
	if (ContactTarget != u16(-1) && CanSpliceAt(ContactPosition))
	{
		// Выравниваем переднюю точку заранее: запоздалый pitch смещает её мимо рассчитанного контакта.
		float ContactHeading;
		float ContactPitch;
		FlightContactDirection.getHP(ContactHeading, ContactPitch);
		const float AlignmentTime = std::abs(Pitch - ContactPitch) / Settings.TurnRate + 2.f / Settings.OrientationResponse;
		const float AlignmentDistance = std::max(Settings.CollisionRadius * 2.f, Speed * AlignmentTime);
		const float Blend = 1.f - clampr(NewPosition.distance_to(ContactPosition) / AlignmentDistance, 0.f, 1.f);
		DesiredHeading = angle_normalize_signed(DesiredHeading + angle_normalize_signed(ContactHeading - DesiredHeading) * Blend);
		DesiredPitch += (ContactPitch - DesiredPitch) * Blend;
	}
	const float Turn = angle_normalize_signed(DesiredHeading - Heading);
	const float Response = 1.f - expf(-Settings.OrientationResponse * DeltaTime);
	// Положение следует проверенному пути; ориентация корпуса плавно выравнивается к касательной контакта.
	float HeadingStep = Turn * Response;
	clamp(HeadingStep, -Settings.TurnRate * DeltaTime, Settings.TurnRate * DeltaTime);
	Heading = angle_normalize_signed(Heading + HeadingStep);
	float PitchStep = (DesiredPitch - Pitch) * Response;
	clamp(PitchStep, -Settings.TurnRate * DeltaTime, Settings.TurnRate * DeltaTime);
	Pitch += PitchStep;
	float DesiredBank = std::max(-Settings.MaximumBank, std::min(Settings.MaximumBank, -Turn));
	if (ContactTarget != u16(-1))
	{
		// Перед касанием выравниваем тело: крен не должен уносить высоко смещённую модель вбок от контакта.
		float ContactBlend = NewPosition.distance_to(ContactPosition) / std::max(Settings.CollisionRadius, Speed * Settings.InterruptLookahead);
		clamp(ContactBlend, 0.f, 1.f);
		DesiredBank *= ContactBlend;
	}
	float BankStep = angle_normalize_signed(DesiredBank - Bank) * Response;
	clamp(BankStep, -Settings.TurnRate * DeltaTime, Settings.TurnRate * DeltaTime);
	Bank = angle_normalize_signed(Bank + BankStep);
	OwnerMonster.XFORM().setHPB(Heading, Pitch, Bank);
}

// Сохраняет полный габарит при прерывании петли, пока тело ещё не выровнено для обычного полёта.
float CELFlyingMovement::TransitionBodyRadius() const
{
	return ActiveRoute.CanInvert || Adapter.Monster().XFORM().j.y < cosf(Settings.MaximumBank) ? ActiveRoute.InvertedBodyRadius : 0.f;
}
