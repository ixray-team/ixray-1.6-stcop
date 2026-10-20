#include "stdafx.h"
#include <algorithm>
#include <cmath>
#include "CELFlyingContinuity.h"
#include "CELSharedFlyingObstacles.h"
#include "../CELFlyingMovement.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "ai/monsters/basemonster/base_monster.h"

SELNavigationResult CELFlyingContinuity::Curve(const SELNavigationIdentity& Identity, const Fvector& Start, const Fvector& Forward, const Fvector& Goal, const Fvector& EndDirection, float Handle)
{
	SELNavigationResult Route;
	Route.Identity = Identity;
	Route.HasPath = true;
	const Fvector First = Fvector(Start).mad(Forward, Handle);
	const Fvector Last = Fvector(Goal).mad(EndDirection, -Handle);
	Route.Points.push_back(Start);
	for (u32 Step = 1; Step < 3; ++Step)
	{
		const float T = float(Step) / 3.f;
		const float U = 1.f - T;
		Fvector Point = Fvector(Start).mul(U * U * U);
		Point.mad(First, 3.f * U * U * T).mad(Last, 3.f * U * T * T).mad(Goal, T * T * T);
		Route.Points.push_back(Point);
	}
	Route.Points.push_back(Goal);
	return Route;
}

void CELFlyingContinuity::Clear()
{
	Prepared = SELNavigationResult();
	AttemptFrame = u32(-1);
	ConnectFrame = u32(-1);
	Attempt = 0;
}

void CELFlyingContinuity::Prepare(IELAdapter& Adapter, CELFlyingMovement& Movement, const SELFlyingSettings& Settings, const Fvector& Destination)
{
	if (Movement.IsLandingFlight() || (Movement.HasRoute() && !Movement.IsStalled() && Movement.RemainingDistance() > Movement.PlanningSpeed() * 1.f))
	{
		return;
	}
	const bool UseEnd = Movement.HasRoute() && !Movement.IsStalled();
	const Fvector Origin = UseEnd ? Movement.EndPosition() : Adapter.Monster().Position();
	if (Prepared.HasPath && Prepared.Points.front().similar(Origin, EPS_L))
	{
		return;
	}
	Prepared = SELNavigationResult();
	if (AttemptFrame == Device.dwFrame)
	{
		return;
	}
	AttemptFrame = Device.dwFrame;
	Fvector Forward = UseEnd ? Movement.EndDirection() : Movement.Direction();
	if (!UseEnd && Movement.GetSpeed() < .2f)
	{
		// Orientation is not momentum: a stalled nose-down flyer can first level its departure.
		Forward.y = 0.f;
	}
	Forward.normalize_safe();
	if (Forward.square_magnitude() <= EPS_L)
	{
		Forward.set(0.f, 0.f, 1.f);
	}
	static const float Angles[] = {0.f, PI_DIV_4, -PI_DIV_4, PI_DIV_2, -PI_DIV_2, 0.f};
	const u32 Variant = Attempt % std::size(Angles);
	Fvector Toward = Fvector().sub(Destination, Origin);
	Toward.y = 0.f;
	Toward.normalize_safe();
	float TargetAngle = atan2f(Forward.z * Toward.x - Forward.x * Toward.z, Forward.x * Toward.x + Forward.z * Toward.z);
	clamp(TargetAngle, -PI_DIV_4, PI_DIV_4);
	const float Angle = Variant == 0 ? 0.f : TargetAngle + Angles[Variant] * (Adapter.Monster().ID() % 2 ? 1.f : -1.f);
	Fvector Heading = {Forward.x * cosf(Angle) + Forward.z * sinf(Angle), 0.f, Forward.z * cosf(Angle) - Forward.x * sinf(Angle)};
	Heading.normalize_safe();
	const float TransitionRadius = Movement.TransitionBodyRadius();
	const float Radius = std::max(Settings.CollisionRadius, TransitionRadius);
	const float CenterHeight = TransitionRadius > 0.f ? 0.f : Settings.CollisionCenterHeight;
	const float TurnRadius = std::max(Radius, Movement.PlanningSpeed() / Settings.TurnRate);
	const float Length = std::max(Radius * 3.f, std::min(Movement.PlanningSpeed(), TurnRadius * 2.f));
	Fvector Goal = Fvector(Origin).mad(Forward, Length * .5f).mad(Heading, Length);
	Goal.y += (Variant % 2 ? -1.f : 1.f) * Radius;
	SELNavigationIdentity Identity{ExtendedEntityes::Navigation().GetGeneration(), ExtendedEntityes::OwnerGeneration(Adapter.Monster().ID()), Movement.NextPlanGeneration(), u16(Adapter.Monster().ID())};
	SELNavigationResult Candidate = Curve(Identity, Origin, Forward, Goal, Heading, TurnRadius);
	if (Variant == 0)
	{
		// First try a cheap near-forward departure; turns retain their smooth control points.
		Candidate.Points.clear();
		Candidate.Points.push_back(Origin);
		Candidate.Points.push_back(Goal);
	}
	Candidate.InvertedBodyRadius = TransitionRadius;
	SELFlyingCorridor Corridor;
	Corridor.Identity = Identity;
	Corridor.Radius = Radius;
	Corridor.CenterHeight = CenterHeight;
	const EELGeometryResult Result = CELFlyingGeometry::CheckEmergency(Candidate, Radius, CenterHeight, Corridor);
	if (Result != EELGeometryResult::Pending)
	{
		++Attempt;
	}
	if (Result == EELGeometryResult::Clear)
	{
		Prepared = std::move(Candidate);
	}
}

bool CELFlyingContinuity::Take(const Fvector& Origin, SELNavigationResult& Route)
{
	if (!Prepared.HasPath || !Prepared.Points.front().similar(Origin, EPS_L))
	{
		return false;
	}
	Route = std::move(Prepared);
	Prepared = SELNavigationResult();
	return true;
}

EELGeometryResult CELFlyingContinuity::Connect(IELAdapter& Adapter, CELFlyingMovement& Movement, const SELFlyingSettings& Settings, SELNavigationResult& Route, const Fvector* RequiredContact)
{
	if (Route.Points.size() < 2 || Route.CanInvert || Movement.TransitionBodyRadius() > 0.f)
	{
		return EELGeometryResult::Blocked;
	}
	const Fvector Origin = Movement.HasRoute() ? Movement.EndPosition() : Adapter.Monster().Position();
	if (Route.Points.front().similar(Origin, EPS_L))
	{
		return EELGeometryResult::Clear;
	}
	if (ConnectFrame == Device.dwFrame)
	{
		return EELGeometryResult::Pending;
	}
	ConnectFrame = Device.dwFrame;
	const Fvector Forward = Movement.HasRoute() ? Movement.EndDirection() : Movement.Direction();
	u32 LastJoin = u32(Route.Points.size() - 2);
	if (RequiredContact)
	{
		for (u32 Index = 0; Index < Route.Points.size(); ++Index)
		{
			if (Route.Points[Index].similar(*RequiredContact, EPS_L))
			{
				LastJoin = std::min(LastJoin, Index);
				break;
			}
		}
	}
	u32 Join = 0;
	for (u32 Index = 1; Index <= LastJoin; ++Index)
	{
		if (Origin.distance_to_sqr(Route.Points[Index]) < Origin.distance_to_sqr(Route.Points[Join]))
		{
			Join = Index;
		}
	}
	const Fvector Arrival = Fvector().sub(Route.Points[Join + 1], Route.Points[Join]).normalize_safe();
	const float Handle = std::max(Settings.CollisionRadius, Movement.PlanningSpeed() / Settings.TurnRate);
	SELNavigationResult Connector = Curve(Route.Identity, Origin, Forward, Route.Points[Join], Arrival, Handle);
	SELFlyingCorridor Corridor;
	Corridor.Identity = Route.Identity;
	Corridor.Radius = Settings.CollisionRadius;
	Corridor.CenterHeight = Settings.CollisionCenterHeight;
	const EELGeometryResult Result = CELFlyingGeometry::CheckEmergency(Connector, Settings.CollisionRadius, Settings.CollisionCenterHeight, Corridor);
	if (Result == EELGeometryResult::Clear)
	{
		Connector.Points.insert(Connector.Points.end(), Route.Points.begin() + Join + 1, Route.Points.end());
		Connector.IsPartial = Route.IsPartial;
		Route = std::move(Connector);
	}
	return Result;
}
