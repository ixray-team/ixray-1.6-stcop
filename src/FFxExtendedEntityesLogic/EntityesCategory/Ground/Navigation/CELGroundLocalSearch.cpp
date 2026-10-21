#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundLocalSearch.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundMovement.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"
#include "xrGame/Level.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"

void CELGroundLocalSearch::Reset()
{
	HasPoint = false;
	NextAttempt = PointUntil = Phase = 0;
}

bool CELGroundLocalSearch::Resolve(IELAdapter& Adapter, const Fvector& Position, Fvector& Result, u32& Vertex)
{
	if (!ai().get_level_graph())
	{
		return false;
	}
	Vertex = ai().level_graph().vertex_id(Position);
	if (!ai().level_graph().valid_vertex_id(Vertex) || !ai().level_graph().inside(Vertex, Position))
	{
		return false;
	}
	Result = Position;
	Result.y = ai().level_graph().vertex_plane_y(Vertex, Result.x, Result.z);
	return Adapter.CanReach(Result, Vertex);
}

bool CELGroundLocalSearch::Move(IELAdapter& Adapter, CELGroundMovement& Movement, const SELPerceptionProfile& Profile, const Fvector& Center, bool Runs)
{
	const Fvector& Origin = Adapter.Monster().Position();
	const u32 Now = Device.dwTimeGlobal;
	if (HasPoint && Origin.distance_to_sqr(ProgressPosition) > .04f)
	{
		ProgressPosition = Origin;
		ProgressAt = Now;
	}
	if (HasPoint && Now - ProgressAt < Profile.GroundStallTime && s32(PointUntil - Now) > 0 && Origin.distance_to_sqr(Point) > .5625f)
	{
		return Movement.MoveTo(Point, PointVertex, Runs);
	}
	HasPoint = false;
	if (s32(Now - NextAttempt) < 0 || !ai().get_level_graph())
	{
		return false;
	}
	NextAttempt = Now + Profile.GroundLocalRetry;
	const u32 StartVertex = Adapter.Monster().ai_location().level_vertex_id();
	if (!ai().level_graph().valid_vertex_id(StartVertex) || !ai().level_graph().inside(StartVertex, Origin))
	{
		return false;
	}
	const float Heading = std::atan2(Center.x - Origin.x, Center.z - Origin.z);
	const float Side = Adapter.Monster().ID() % 2 ? 1.f : -1.f;
	// Two candidates per update; rejected headings are resumed on the next update.
	for (u32 Attempt = 0; Attempt < 2; ++Attempt)
	{
		const u32 Choice = Phase++ % 8;
		const float Angle = Heading + Side * float(Choice) * (PI * .25f);
		Fvector Candidate = Origin;
		Candidate.x += std::sin(Angle) * 3.f;
		Candidate.z += std::cos(Angle) * 3.f;
		if (!Resolve(Adapter, Candidate, Point, PointVertex) ||
			!ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(StartVertex, Origin, Point)) ||
			CELSharedAnomalies::Crosses(Adapter.Monster().ID(), Origin, Point) || CELSharedDeaths::Crosses(Adapter.Monster().ID(), Origin, Point))
		{
			continue;
		}
		if (!ExtendedEntityes::Navigation().TryGeometryRays(3))
		{
			return false;
		}
		Fvector Direction = Fvector().sub(Point, Origin);
		const float Length = Direction.magnitude();
		if (Length < EPS_L)
		{
			continue;
		}
		Direction.div(Length);
		Fvector Lateral = {-Direction.z, 0.f, Direction.x};
		Fvector BodyCenter;
		Adapter.Monster().Center(BodyCenter);
		const float Width = std::clamp(Adapter.Monster().Radius() * .5f, .2f, .8f);
		bool Blocked = false;
		for (s32 Ray = -1; Ray <= 1; ++Ray)
		{
			Fvector Start = Fvector().mad(BodyCenter, Lateral, float(Ray) * Width);
			if (Level().ObjectSpace.RayTest(Start, Direction, Length, collide::rqtStatic, nullptr, &Adapter.Monster()))
			{
				Blocked = true;
				break;
			}
		}
		if (!Blocked && Movement.MoveTo(Point, PointVertex, Runs))
		{
			HasPoint = true;
			PointUntil = Now + Profile.SearchPointTime;
			ProgressPosition = Origin;
			ProgressAt = Now;
			return true;
		}
	}
	return false;
}
