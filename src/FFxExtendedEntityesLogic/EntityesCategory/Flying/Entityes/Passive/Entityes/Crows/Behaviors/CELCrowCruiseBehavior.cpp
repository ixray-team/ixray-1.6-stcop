#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/Behaviors/CELCrowCruiseBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/Level.h"

// Связывает полёт вороны, взлёт и облёт пищи с необходимыми службами и состоянием владельца.
CELCrowCruiseBehavior::CELCrowCruiseBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, const SELCrowSettings& SpeciesSettings, SELCrowState& CrowState) : CELFlyingBehaviorBase(OwnerAdapter, FlightSettings, FlightMovement), CrowSettings(SpeciesSettings), State(CrowState) {}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELCrowCruiseBehavior::Enter()
{
	CELFlyingBehaviorBase::Enter();
	Home = Adapter.Monster().Position();
	CircleAngle = float(Adapter.Monster().ID() % 32) * PI_MUL_2 / 32.f;
	HasDestination = false;
	Movement.SetLanding(false);
}

// Настраивает условия следующего запуска схемы.
void CELCrowCruiseBehavior::Configure(EELCrowFlightMode NewMode, const Fvector& NewAnchor, bool IsTakingOff)
{
	if (Mode != NewMode || (NewMode != EELCrowFlightMode::Roam && Anchor.distance_to_sqr(NewAnchor) > 16.f))
	{
		CancelRouteRequest();
		HasPreparedRoute = false;
		HasDestination = false;
		RetryAfter = 0;
	}
	NeedsTakeoff = IsTakingOff;
	Mode = NewMode;
	Anchor = NewAnchor;
}

// Выбирает следующую точку маршрута с учётом текущей фазы поведения.
void CELCrowCruiseBehavior::ChooseGoal()
{
	const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	const Fbox Bounds = CELFlyingGeometry::AirBounds(Settings.BoundaryMargin, Settings.CollisionRadius, Settings.CollisionCenterHeight);

	Fvector Direction;
	if (s32(Device.dwTimeGlobal - NextHeightChange) >= 0)
	{
		State.Altitude = CrowSettings.Altitude.Sample();
		NextHeightChange = Device.dwTimeGlobal + u32(CrowSettings.HeightChange.Sample() * 1000.f);
	}
	if (NeedsTakeoff)
	{
		Direction = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
		Direction.y = 0.f;
		if (Direction.square_magnitude() < EPS_L)
		{
			Direction.set(0.f, 0.f, 1.f);
		}
		Direction.normalize_safe();
		const float Height = CrowSettings.TakeoffHeight.Sample();
		GoalPosition.mad(Origin, Direction, std::max(Settings.CollisionRadius * 2.f, Height * CrowSettings.TakeoffForwardRatio));
		GoalPosition.y += Height;
		GoalDirection = Direction;
	}
	else if (Mode == EELCrowFlightMode::Escape)
	{
		Direction.sub(Origin, State.Danger);
		Direction.y = 0.f;
		if (Direction.square_magnitude() < EPS_L)
		{
			Direction = Movement.Direction();
			Direction.y = 0.f;
		}
		Direction.normalize_safe();
		GoalPosition.mad(Origin, Direction, CrowSettings.EscapeDistance);
		GoalPosition.y = Origin.y + CrowSettings.EscapeHeight;
		GoalDirection.set(Direction.x, .3f, Direction.z);
	}
	else if (Mode == EELCrowFlightMode::Circle || Mode == EELCrowFlightMode::Investigate)
	{
		CircleAngle += PI_DIV_2;
		GoalPosition = Anchor;
		GoalPosition.x += std::sin(CircleAngle) * CrowSettings.CircleRadius.Sample();
		GoalPosition.z += std::cos(CircleAngle) * CrowSettings.CircleRadius.Sample();
		GoalPosition.y += CrowSettings.CircleHeight.Sample();
		GoalDirection.set(std::cos(CircleAngle), 0.f, -std::sin(CircleAngle));
	}
	else
	{
		if (!HasDestination || Origin.distance_to_sqr(Destination) < CrowSettings.LegDistance * CrowSettings.LegDistance)
		{
			if (CrowSettings.Mapwide)
			{
				Destination.set(Random.randF(Bounds.min.x, Bounds.max.x), 0.f, Random.randF(Bounds.min.z, Bounds.max.z));
			}
			else
			{
				Destination = Home;
				Destination.x += Random.randF(-CrowSettings.RoamRadius, CrowSettings.RoamRadius);
				Destination.z += Random.randF(-CrowSettings.RoamRadius, CrowSettings.RoamRadius);
			}
			HasDestination = true;
		}
		Direction.sub(Destination, Origin);
		Direction.y = 0.f;
		Direction.normalize_safe();
		GoalPosition.mad(Origin, Direction, CrowSettings.LegDistance);
		GoalDirection = Direction;
	}
	clamp(GoalPosition.x, Bounds.min.x, Bounds.max.x);
	clamp(GoalPosition.z, Bounds.min.z, Bounds.max.z);
	float SurfaceHeight;
	if (CELFlyingGeometry::Surface(GoalPosition, SurfaceHeight))
	{
		GoalPosition.y = std::max(Mode == EELCrowFlightMode::Roam && !NeedsTakeoff ? SurfaceHeight + State.Altitude : GoalPosition.y, SurfaceHeight + Settings.CollisionCenterHeight + Settings.CollisionRadius);
	}
	clamp(GoalPosition.y, Bounds.min.y, Bounds.max.y);
}

// Переводит схему в фазу, соответствующую фактически принятому маршруту.
void CELCrowCruiseBehavior::OnRouteActivated()
{
	NeedsTakeoff = false;
}
