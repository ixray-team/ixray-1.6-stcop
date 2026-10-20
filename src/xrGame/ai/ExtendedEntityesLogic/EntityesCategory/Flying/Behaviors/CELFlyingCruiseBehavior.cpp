#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingCruiseBehavior.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "Level.h"

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELFlyingCruiseBehavior::Enter()
{
	CELFlyingBehaviorBase::Enter();
	WanderHeading = Random.randF(PI_MUL_2);
}

// Назначает интересующую точку и срок удержания внимания.
void CELFlyingCruiseBehavior::SetAttention(const Fvector& Position, bool IsEscape)
{
	if (!HasAttention || IsEscaping != IsEscape || (IsEscape && AttentionPosition.distance_to_sqr(Position) > Settings.CollisionRadius * Settings.CollisionRadius))
	{
		CancelRouteRequest();
		HasPreparedRoute = false;
		RetryAfter = 0;
	}
	if (IsEscape && (!HasAttention || !IsEscaping))
	{
		Movement.RetainRoutePrefix(std::max(Settings.CollisionRadius * 2.f, Movement.GetSpeed() * Settings.InterruptLookahead));
	}
	// Повторные шаги и выстрелы обновляют следующее назначение, не обнуляя ещё незавершённый поиск.
	AttentionPosition = Position;
	HasAttention = true;
	IsEscaping = IsEscape;
}

// Снимает временную интересующую точку свободного полёта.
void CELFlyingCruiseBehavior::ClearAttention()
{
	if (HasAttention)
	{
		CancelRouteRequest();
		HasPreparedRoute = false;
		RetryAfter = 0;
	}
	HasAttention = false;
	IsEscaping = false;
}

// Обновляет схему и показывает в отладке фактический режим свободного полёта.
void CELFlyingCruiseBehavior::Update(float DeltaTime)
{
	CELFlyingBehaviorBase::Update(DeltaTime);
	if (Validation.State == EELBehaviorState::Running)
	{
		Validation.Reason = HasAttention ? (IsEscaping ? "Уход от источника шума" : "Осмотр источника шума") : "Свободное блуждание";
	}
}

// Выбирает очередной самостоятельный перелёт от текущего конца пути, без привязки к спавну.
void CELFlyingCruiseBehavior::ChooseWander(const Fvector& Origin)
{
	if (Movement.HasRoute())
	{
		const Fvector Tangent = Movement.EndDirection();
		if (Tangent.x * Tangent.x + Tangent.z * Tangent.z > EPS_L)
		{
			WanderHeading = std::atan2(Tangent.x, Tangent.z);
		}
	}
	WanderHeading = angle_normalize_signed(WanderHeading + Random.randF(-Settings.WanderTurnAngle, Settings.WanderTurnAngle));
	const float Distance = Random.randF(Settings.WanderDistanceMinimum, Settings.WanderDistanceMaximum);
	GoalDirection.set(std::sin(WanderHeading), 0.f, std::cos(WanderHeading));
	GoalPosition.mad(Origin, GoalDirection, Distance);
}

// Осматривает конкретный стимул либо уходит от него; этот режим не задаёт свободное блуждание.
void CELFlyingCruiseBehavior::ChooseAttention(const Fvector& Origin)
{
	if (IsEscaping)
	{
		GoalDirection.sub(Origin, AttentionPosition);
		GoalDirection.y = 0.f;
		if (GoalDirection.square_magnitude() < EPS_L)
		{
			GoalDirection.set(std::sin(WanderHeading), 0.f, std::cos(WanderHeading));
		}
		GoalDirection.normalize_safe();
		GoalPosition.mad(Origin, GoalDirection, Settings.OrbitRadius);
		return;
	}
	// Источник может быть за дверью: орбита вокруг него выбирает другую комнату или стену.
	GoalPosition = AttentionPosition;
	GoalDirection.sub(GoalPosition, Origin).normalize_safe();
	if (Origin.distance_to_sqr(GoalPosition) < _sqr(std::max(1.f, Settings.CollisionRadius * 2.f)))
	{
		// Осмотр продолжается перелётом, а не нулевым маршрутом в уже достигнутую точку звука.
		GoalDirection = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
		GoalPosition.mad(Origin, GoalDirection, std::max(Settings.CollisionRadius * 2.f, 2.f * Movement.PlanningSpeed() / Settings.TurnRate));
	}
	if (GoalDirection.square_magnitude() <= EPS_L)
	{
		GoalDirection = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	}
}

// Разворачивает назначение у края карты, чтобы особи не собирались на одной обрезанной точке.
void CELFlyingCruiseBehavior::ConstrainGoal()
{
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);
	if (GoalPosition.x < Bounds.min.x || GoalPosition.x > Bounds.max.x)
	{
		GoalPosition.x = 2.f * (GoalPosition.x < Bounds.min.x ? Bounds.min.x : Bounds.max.x) - GoalPosition.x;
		GoalDirection.x = -GoalDirection.x;
	}
	if (GoalPosition.z < Bounds.min.z || GoalPosition.z > Bounds.max.z)
	{
		GoalPosition.z = 2.f * (GoalPosition.z < Bounds.min.z ? Bounds.min.z : Bounds.max.z) - GoalPosition.z;
		GoalDirection.z = -GoalDirection.z;
	}
	clamp(GoalPosition.x, Bounds.min.x, Bounds.max.x);
	clamp(GoalPosition.z, Bounds.min.z, Bounds.max.z);
	if (!HasAttention)
	{
		WanderHeading = std::atan2(GoalDirection.x, GoalDirection.z);
	}
}

// Назначает случайную разрешённую высоту центра тела над поверхностью под новым участком.
void CELFlyingCruiseBehavior::ApplyHeight(const Fvector& Origin)
{
	float SurfaceHeight;
	GoalPosition.y = Origin.y;
	if (CELFlyingGeometry::Surface(GoalPosition, SurfaceHeight))
	{
		const float Height = HasAttention ? Settings.Altitude + Settings.CollisionCenterHeight : Random.randF(Settings.WanderHeightMinimum, Settings.WanderHeightMaximum);
		GoalPosition.y = SurfaceHeight + Height - Settings.CollisionCenterHeight;
		if (HasAttention && IsEscaping)
		{
			GoalPosition.y = std::max(GoalPosition.y, Origin.y);
		}
	}
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);
	clamp(GoalPosition.y, Bounds.min.y, Bounds.max.y);
}

// Разделяет свободный перелёт и реакцию на стимул, сохраняя общий проверяемый конвейер маршрута.
void CELFlyingCruiseBehavior::ChooseGoal()
{
	ManeuverPoints.clear();
	InvertedBodyRadius = 0.f;
	PlannedManeuver = EELFlightManeuver::Glide;
	const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	if (HasAttention)
	{
		ChooseAttention(Origin);
	}
	else
	{
		ChooseWander(Origin);
	}
	if (Movement.InConfinedSpace() && LimitsGoalUnderCover())
	{
		// Под перекрытием нужен короткий перелёт; фигуры открытого неба сюда не переносятся.
		Fvector Direction = Fvector().sub(GoalPosition, Origin);
		Direction.y = 0.f;
		Direction.normalize_safe();
		if (Direction.square_magnitude() <= EPS_L)
		{
			Direction = Movement.Direction();
			Direction.y = 0.f;
			Direction.normalize_safe();
		}
		if (Direction.square_magnitude() <= EPS_L)
		{
			Direction.set(0.f, 0.f, 1.f);
		}
		const float Distance = std::max(Settings.CollisionRadius * 2.f, 2.f * Movement.PlanningSpeed() / Settings.TurnRate);
		GoalPosition.mad(Origin, Direction, Distance);
		GoalDirection = Direction;
		ConstrainGoal();
		return;
	}
	ConstrainGoal();
	if (!HasAttention || IsEscaping)
	{
		ApplyHeight(Origin);
	}
	if (!HasAttention)
	{
		ChooseManeuver(Origin);
	}
}

// Независимо выбирает разрешённый фигурный перелёт; боевые и шумовые схемы сюда не попадают.
void CELFlyingCruiseBehavior::ChooseManeuver(const Fvector& Origin)
{
	EELFlightManeuver Allowed[u32(EELFlightManeuver::Count)];
	u32 ModeIndex;
	u32 Count = 0;
	for (ModeIndex = 0; ModeIndex < u32(EELFlightManeuver::Count); ++ModeIndex)
	{
		if (Settings.WanderManeuvers & (1u << ModeIndex))
		{
			Allowed[Count++] = EELFlightManeuver(ModeIndex);
		}
	}
	PlannedManeuver = Count ? Allowed[Random.randI(Count)] : EELFlightManeuver::Glide;
	const float Radius = std::max(Settings.Speed / Settings.TurnRate, Random.randF(Settings.ManeuverRadiusMinimum, Settings.ManeuverRadiusMaximum));
	if (PlannedManeuver == EELFlightManeuver::Loop)
	{
		if (Radius > Settings.ManeuverRadiusMaximum || !BuildLoop(Origin, Radius))
		{
			PlannedManeuver = EELFlightManeuver::Glide;
			InvertedBodyRadius = 0.f;
			ManeuverPoints.clear();
		}
	}
	else if (PlannedManeuver != EELFlightManeuver::Glide)
	{
		BuildVariation(Origin, Radius);
	}
}

// Задаёт полную вертикальную петлю с запасом под вращающееся тело и разрешённой высотой.
bool CELFlyingCruiseBehavior::BuildLoop(const Fvector& Origin, float Radius)
{
	if (!Movement.HasRoute())
	{
		return false;
	}
	float SurfaceHeight;
	if (!CELFlyingGeometry::Surface(Origin, SurfaceHeight))
	{
		return false;
	}
	const Fbox& ModelBounds = Adapter.Monster().BoundingBox();
	Fvector Extent;
	Fvector MaximumExtent;
	Extent.abs(ModelBounds.min);
	MaximumExtent.abs(ModelBounds.max);
	Extent.max(MaximumExtent);
	const float BodyRadius = std::max(Extent.magnitude(), Settings.CollisionRadius + Settings.CollisionCenterHeight);
	if (Origin.y - BodyRadius < SurfaceHeight + Settings.GroundClearance || Origin.y + Settings.CollisionCenterHeight + 2.f * Radius > SurfaceHeight + Settings.WanderHeightMaximum)
	{
		return false;
	}
	Fvector Forward = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	Forward.y = 0.f;
	Forward.normalize_safe();
	if (Forward.square_magnitude() < EPS_L)
	{
		return false;
	}
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, BodyRadius, 0.f);
	Fvector Point;
	float Angle;
	u32 Quarter;
	for (Quarter = 0; Quarter <= 4; ++Quarter)
	{
		Angle = float(Quarter) * PI_DIV_2;
		Point.mad(Origin, Forward, std::sin(Angle) * Radius);
		Point.y += (1.f - std::cos(Angle)) * Radius;
		if (!Bounds.contains(Point))
		{
			return false;
		}
		ManeuverPoints.push_back(Point);
	}
	ManeuverPoints.back() = Origin;
	GoalPosition = Origin;
	GoalDirection = Forward;
	InvertedBodyRadius = BodyRadius;
	ManeuverCurveDistance = Radius;
	return true;
}

// Формирует арку, вертикальную волну, змейку или пространственную спираль между плавными стыками.
void CELFlyingCruiseBehavior::BuildVariation(const Fvector& Origin, float Radius)
{
	float SurfaceHeight;
	if (!CELFlyingGeometry::Surface(Origin, SurfaceHeight))
	{
		PlannedManeuver = EELFlightManeuver::Glide;
		return;
	}
	const Fvector Side = Fvector().set(GoalDirection.z, 0.f, -GoalDirection.x);
	const float Handedness = Random.randI(2) ? 1.f : -1.f;
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);
	Fvector Displacement = Fvector().sub(GoalPosition, Origin);
	Fvector Point;
	float Fraction;
	u32 Sample;
	ManeuverPoints.push_back(Origin);
	for (Sample = 1; Sample < 4; ++Sample)
	{
		Fraction = float(Sample) / 4.f;
		Point.mad(Origin, Displacement, Fraction);
		if (PlannedManeuver == EELFlightManeuver::Arch)
		{
			Point.y += std::sin(PI * Fraction) * Radius;
		}
		else if (PlannedManeuver == EELFlightManeuver::Wave)
		{
			Point.y += std::sin(PI_MUL_2 * Fraction) * Radius * .5f;
		}
		else
		{
			Point.mad(Side, Handedness * std::sin(PI_MUL_2 * Fraction) * Radius);
			Point.y += PlannedManeuver == EELFlightManeuver::Spiral ? (1.f - std::cos(PI_MUL_2 * Fraction)) * Radius * .5f : std::sin(PI * Fraction) * Radius * .25f;
		}
		clamp(Point.y, SurfaceHeight + Settings.WanderHeightMinimum - Settings.CollisionCenterHeight, SurfaceHeight + Settings.WanderHeightMaximum - Settings.CollisionCenterHeight);
		if (!Bounds.contains(Point))
		{
			ManeuverPoints.clear();
			PlannedManeuver = EELFlightManeuver::Glide;
			return;
		}
		ManeuverPoints.push_back(Point);
	}
	ManeuverPoints.push_back(GoalPosition);
	ManeuverCurveDistance = Radius;
}

// Передаёт опорные точки worker и выбирает безопасный габарит полной ориентации только для петли.
void CELFlyingCruiseBehavior::ConfigureCurveRequest(SELNavigationRequest& Request)
{
	Request.GuidePoints = ManeuverPoints;
	Request.InvertedBodyRadius = InvertedBodyRadius;
	Request.CanInvert = PlannedManeuver == EELFlightManeuver::Loop;
	if (!ManeuverPoints.empty())
	{
		Request.CurveDistance = ManeuverCurveDistance;
	}
}

// Название фигуры меняется только вместе с реально принятым маршрутом, а не с будущей заявкой.
void CELFlyingCruiseBehavior::OnRouteActivated()
{
	ActiveManeuver = PlannedManeuver;
}

// Показывает выполняемую фигуру или текущую реакцию на источник шума.
const char* CELFlyingCruiseBehavior::FlightDescription() const
{
	if (HasAttention)
	{
		return IsEscaping ? "Уход от источника шума" : "Осмотр источника шума";
	}
	static const char* const Names[] = {"Свободный перелёт", "Набор и снижение по арке", "Вертикальная волна", "Пространственная змейка", "Пространственная спираль", "Мёртвая петля"};
	static_assert(sizeof(Names) / sizeof(Names[0]) == u32(EELFlightManeuver::Count));
	return Names[u32(ActiveManeuver)];
}
