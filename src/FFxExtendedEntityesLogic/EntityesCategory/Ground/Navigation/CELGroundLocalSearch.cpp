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
#include "xrGame/monster_scale.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"

void CELGroundLocalSearch::Reset()
{
	HasPoint = HasCenter = false;
	NextAttempt = DirectCheckAfter = PointUntil = Phase = 0;
	IsDirect = false;
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

// Execute a short tactical step immediately, within the existing collision-query budget.
bool CELGroundLocalSearch::MoveDirect(IELAdapter& Adapter, CELGroundMovement& Movement, const Fvector& Goal, bool Runs, const char** Failure, bool* WaitingForBudget)
{
	if (WaitingForBudget)
	{
		*WaitingForBudget = false;
	}
	const auto Report = [Failure](const char* Reason)
	{
		if (Failure)
		{
			*Failure = Reason;
		}
		return false;
	};
	Fvector Point;
	u32 Vertex;
	const Fvector& Origin = Adapter.Monster().Position();
	const u32 StartVertex = Adapter.Monster().ai_location().level_vertex_id();
	if (!Resolve(Adapter, Goal, Point, Vertex) || !ai().level_graph().valid_vertex_id(StartVertex))
	{
		return Report("нет достижимой вершины");
	}
	if (!ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(StartVertex, Origin, Point)))
	{
		return Report("нет прямого прохода по АИ-сетке");
	}
	if (CELSharedAnomalies::Crosses(Adapter.Monster().ID(), Origin, Point) || CELSharedDeaths::Crosses(Adapter.Monster().ID(), Origin, Point))
	{
		return Report("опасная зона на боковом шаге");
	}
	Fvector Direction = Fvector().sub(Point, Origin);
	const float Length = Direction.magnitude();
	if (Length <= EPS_L)
	{
		return Report("боковая точка уже достигнута");
	}
	if (!ExtendedEntityes::Navigation().TryGeometryRays(3))
	{
		if (WaitingForBudget)
		{
			*WaitingForBudget = true;
		}
		return Report("ожидание квоты проверки геометрии");
	}
	Direction.div(Length);
	const Fvector Side = {-Direction.z, 0.f, Direction.x};
	const float PathScale = MonsterScale::Navigation(Adapter.Monster().GetVisualScale());
	const float Width = std::clamp(Adapter.Monster().Radius() * .5f, .2f * PathScale, .8f * PathScale);
	Fvector Center;
	Adapter.Monster().Center(Center);
	for (s32 Ray = -1; Ray <= 1; ++Ray)
	{
		const Fvector Start = Fvector().mad(Center, Side, float(Ray) * Width);
		if (Level().ObjectSpace.RayTest(Start, Direction, Length, collide::rqtStatic, nullptr, &Adapter.Monster()))
		{
			return Report("геометрия перекрыла боковой шаг");
		}
	}
	if (!Movement.MoveTo(Point, Vertex, Runs))
	{
		return Report("контроллер не принял движение");
	}
	if (Failure)
	{
		*Failure = "бег по боковому шагу принят";
	}
	return true;
}

bool CELGroundLocalSearch::Move(IELAdapter& Adapter, CELGroundMovement& Movement, const SELPerceptionProfile& Profile, const Fvector& Center, bool Runs)
{
	const Fvector& Origin = Adapter.Monster().Position();
	const u32 Now = Device.dwTimeGlobal;
	const float PathScale = MonsterScale::Navigation(Adapter.Monster().GetVisualScale());
	if (!HasCenter || GoalCenter.distance_to_sqr(Center) >= _sqr(CELSharedSettings::Work().GroundRouteReplanDistance))
	{
		// A cached step towards an old stimulus must not delay a new combat direction.
		HasPoint = IsDirect = false;
		GoalCenter = Center;
		HasCenter = true;
	}
	if (s32(Now - DirectCheckAfter) >= 0)
	{
		DirectCheckAfter = Now + Profile.GroundLocalRetry;
		Fvector DirectPoint;
		u32 DirectVertex;
		if (Resolve(Adapter, Center, DirectPoint, DirectVertex) && MoveDirect(Adapter, Movement, DirectPoint, Runs))
		{
			Point = DirectPoint;
			PointVertex = DirectVertex;
			HasPoint = IsDirect = true;
			PointUntil = Now + MonsterScale::LocalMoveTime(Profile.SearchPointTime, Adapter.Monster().GetVisualScale());
			ProgressPosition = Origin;
			ProgressAt = Now;
			return true;
		}
		if (IsDirect)
		{
			HasPoint = IsDirect = false;
		}
	}
	if (HasPoint && Origin.distance_to_sqr(ProgressPosition) > .04f * _sqr(std::min(1.f, MonsterScale::Movement(Adapter.Monster().GetVisualScale()))))
	{
		ProgressPosition = Origin;
		ProgressAt = Now;
	}
	if (HasPoint && Now - ProgressAt < Profile.GroundStallTime && s32(PointUntil - Now) > 0 && Origin.distance_to_sqr(Point) > .5625f * _sqr(Adapter.Monster().GetVisualScale()))
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
		Candidate.x += std::sin(Angle) * 3.f * PathScale;
		Candidate.z += std::cos(Angle) * 3.f * PathScale;
		if (Resolve(Adapter, Candidate, Point, PointVertex) && MoveDirect(Adapter, Movement, Point, Runs))
		{
			HasPoint = true;
			IsDirect = false;
			PointUntil = Now + MonsterScale::LocalMoveTime(Profile.SearchPointTime, Adapter.Monster().GetVisualScale());
			ProgressPosition = Origin;
			ProgressAt = Now;
			return true;
		}
	}
	return false;
}
