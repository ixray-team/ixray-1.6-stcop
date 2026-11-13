#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include <cmath>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundReactionBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/level_graph.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnvironment.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/Level.h"

// Obstacle waiting uses native navigable ground and bounded species inspection animations.
CELGroundReactionBehavior::CELGroundReactionBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile)
	: CELGroundBehaviorBase(OwnerAdapter, OwnerProfile), Inspection(OwnerAdapter.Monster())
{
}

void CELGroundReactionBehavior::ConfigureObstacle(const Fvector& EnemyPosition, u32 WaitUntil)
{
	if (!IsObstaclePatrol || SourcePosition.distance_to_sqr(EnemyPosition) > _sqr(Profile.ReactionReplanDistance))
	{
		Inspection.Clear();
		IsInspectingPoint = HasPoint = false;
		NextPoint = 0;
	}
	IsObstaclePatrol = true;
	IsWatching = IsFleeing = HasEvasionGoal = false;
	SourcePosition = EnemyPosition;
	Until = ActionUntil = WaitUntil;
}

void CELGroundReactionBehavior::Exit()
{
	Inspection.Clear();
	IsInspectingPoint = false;
	CELGroundBehaviorBase::Exit();
}

// Настраивает условия следующего запуска схемы.
void CELGroundReactionBehavior::Configure(const Fvector& SoundPosition, u32 ReactionUntil, bool ShouldFlee)
{
	if (IsObstaclePatrol)
	{
		Inspection.Clear();
		IsInspectingPoint = false;
	}
	IsObstaclePatrol = false;
	if (IsFleeing != ShouldFlee || (HasPoint ? PlannedSourcePosition : SourcePosition).distance_to_sqr(SoundPosition) > _sqr(Profile.ReactionReplanDistance))
	{
		HasPoint = false;
		NextPoint = 0;
		SearchPhase = 0;
	}
	IsWatching = false;
	HasEvasionGoal = false;
	SourcePosition = SoundPosition;
	Until = ReactionUntil;
	ActionUntil = Until;
	IsFleeing = ShouldFlee;
}

// Reuse bounded graph navigation for cautious following; enemy orientation is sampled only while perceived.
void CELGroundReactionBehavior::ConfigureWatching(const Fvector& EnemyPosition, const Fvector& EnemyForward, u32 WatchUntil, bool Visible, const Fvector* Eye)
{
	if (IsObstaclePatrol)
	{
		Inspection.Clear();
		IsInspectingPoint = false;
	}
	IsObstaclePatrol = false;
	if (!IsWatching || IsFleeing)
	{
		HasPoint = false;
		NextPoint = 0;
		WatchSide = Adapter.Monster().ID() % 2 ? 1.f : -1.f;
	}
	IsWatching = true;
	IsFleeing = false;
	HasEvasionGoal = false;
	WatchVisible = Visible;
	SourcePosition = EnemyPosition;
	Until = ActionUntil = WatchUntil;
	if (Visible)
	{
		if (Eye)
		{
			WatchEyeHeight = Eye->y - EnemyPosition.y;
		}
		WatchedForward = EnemyForward;
		WatchedForward.y = 0.f;
		WatchedForward.normalize_safe();
	}
}

void CELGroundReactionBehavior::SetEvasionGoal(const Fvector* Goal)
{
	HasEvasionGoal = Goal != nullptr;
	if (Goal)
	{
		EvasionGoal = *Goal;
	}
}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundReactionBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	HasPoint = false;
	NextPoint = 0;
	SearchPhase = 0;
	EscapeBlockedSince = 0;
	ActionUntil = Until;
	PointStartedAt = Device.dwTimeGlobal;
}

// Выбирает достижимую точку для реакции на стимул.
bool CELGroundReactionBehavior::SelectPoint()
{
	if (IsObstaclePatrol)
	{
		return SelectObstaclePoint();
	}
	if (IsWatching)
	{
		return SelectWatchPoint();
	}
	Fvector Away;
	Fvector Candidate;
	float Heading;
	float Pitch;
	float Angle;
	u32 CandidateVertex;
	u32 Attempt;
	Away.sub(Adapter.Monster().Position(), SourcePosition);
	Away.y = 0.f;
	if (Away.square_magnitude() < EPS_L)
	{
		Away = Adapter.Monster().XFORM().k;
		Away.y = 0.f;
	}
	Away.normalize_safe();
	Away.getHP(Heading, Pitch);
	for (Attempt = 0; Attempt < Profile.ReactionPointAttempts; ++Attempt)
	{
		if (IsFleeing)
		{
			Angle = Heading + float(Attempt % 2 ? s32(Attempt) : -s32(Attempt)) * Profile.ReactionEscapeAngle;
			Candidate = Adapter.Monster().Position();
			Candidate.x -= std::sin(Angle) * Profile.ReactionEscapeStep;
			Candidate.z += std::cos(Angle) * Profile.ReactionEscapeStep;
		}
		else
		{
			Candidate = SourcePosition;
			if (SearchPhase || Attempt)
			{
				const u32 Phase = SearchPhase + Attempt;
				Angle = float(Phase) * (PI_MUL_2 / float(Profile.ReactionSearchDirections)) + float(Adapter.Monster().ID() % Profile.ReactionSearchDirections) * (PI_MUL_2 / float(Profile.ReactionSearchDirections));
				const float Radius = std::min(Profile.SearchRadius, Profile.ReactionSearchStartRadius + float(Phase / Profile.ReactionSearchDirections) * Profile.ReactionSearchRadiusStep);
				Candidate.x += std::sin(Angle) * Radius;
				Candidate.z += std::cos(Angle) * Radius;
			}
		}
		if (CELGroundLocalSearch::Resolve(Adapter, Candidate, Candidate, CandidateVertex))
		{
			PlannedSourcePosition = SourcePosition;
			ReactionPosition = Candidate;
			ReactionVertex = CandidateVertex;
			return true;
		}
		++SearchPhase;
	}
	return false;
}

// Advance around the obstruction, rather than requesting the enemy's non-navigable vertex again.
bool CELGroundReactionBehavior::SelectObstaclePoint()
{
	CBaseMonster& Monster = Adapter.Monster();
	Fvector Away = Fvector().sub(Monster.Position(), SourcePosition);
	Away.y = 0.f;
	const float Heading = Away.square_magnitude() > EPS_L ? atan2f(Away.x, Away.z) : Monster.Direction().getH();
	const float Side = Monster.ID() % 2 ? 1.f : -1.f;
	const float Radius = std::max(Profile.ReactionArrivalDistance * 2.f, std::min(Profile.SearchRadius, Profile.NativeAttackDistance));
	const u32 StartVertex = Monster.ai_location().level_vertex_id();
	bool HasFallback = false;
	Fvector Fallback;
	u32 FallbackVertex = u32(-1);
	for (u32 Attempt = 0; Attempt < Profile.ReactionPointAttempts; ++Attempt)
	{
		const float Angle = Heading + Side * (Attempt % 2 ? -1.f : 1.f) * (Profile.CharacterWatchAngle + float(Attempt / 2) * Profile.ReactionEscapeAngle);
		Fvector Point = SourcePosition;
		const float ProbeRadius = std::max(Profile.ReactionArrivalDistance, Radius / float(Attempt / 2 + 1));
		Point.x += std::sin(Angle) * ProbeRadius;
		Point.z += std::cos(Angle) * ProbeRadius;
		u32 Vertex;
		if (!CELGroundLocalSearch::Resolve(Adapter, Point, Point, Vertex) || Monster.Position().distance_to_sqr(Point) <= _sqr(Profile.ReactionArrivalDistance))
		{
			continue;
		}
		const bool Direct = ai().level_graph().valid_vertex_id(StartVertex) && ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(StartVertex, Monster.Position(), Point));
		if (Direct)
		{
			ReactionPosition = Point;
			ReactionVertex = Vertex;
			PlannedSourcePosition = SourcePosition;
			return true;
		}
		if (!HasFallback)
		{
			HasFallback = true;
			Fallback = Point;
			FallbackVertex = Vertex;
		}
	}
	if (HasFallback)
	{
		ReactionPosition = Fallback;
		ReactionVertex = FallbackVertex;
		PlannedSourcePosition = SourcePosition;
	}
	return HasFallback;
}

// Prefer free space, cover and a stable side; the configured distance is a lower preference, not a ring.
bool CELGroundReactionBehavior::SelectWatchPoint()
{
	CBaseMonster& Monster = Adapter.Monster();
	const Fvector& Origin = Monster.Position();
	const u32 Now = Device.dwTimeGlobal;
	Fvector Away = Fvector().sub(Origin, SourcePosition);
	Away.y = 0.f;
	const float Distance = Away.magnitude();
	Away.normalize_safe();
	if (Away.square_magnitude() < EPS_L)
	{
		Away = Monster.Direction();
	}
	const float ViewDistance = std::max(Profile.CharacterWatchMinLeg, Monster.eye_range * CELSharedEnvironment::Vision(Profile) * Profile.CharacterWatchVisibilityRatio);
	const float MinimumDistance = std::min(Profile.CharacterWatchDistance * Profile.CharacterWatchRetreatRatio, ViewDistance);
	float BestScore = -flt_max;
	bool BestIsDirect = false;
	Fvector BestPoint;
	u32 BestVertex = u32(-1);
	float BestSide = WatchSide;
	for (u32 Attempt = 0; Attempt < Profile.ReactionPointAttempts; ++Attempt)
	{
		const float Side = WatchSide * (Attempt % 2 ? -1.f : 1.f);
		const float Angle = atan2f(Away.x, Away.z) + Side * (Profile.CharacterWatchAngle + float(Attempt / 2) * (Profile.ReactionEscapeAngle + Profile.CharacterWatchAngleStep));
		const float Length = std::max(Profile.CharacterWatchMinLeg, std::min(Profile.ReactionEscapeStep / float(Attempt / 2 + 1), ViewDistance));
		Fvector Candidate = Origin;
		Candidate.x += std::sin(Angle) * Length;
		Candidate.z += std::cos(Angle) * Length;
		u32 Vertex;
		if (!CELGroundLocalSearch::Resolve(Adapter, Candidate, Candidate, Vertex) || Origin.distance_to_sqr(Candidate) < _sqr(Profile.CharacterWatchMinLeg))
		{
			continue;
		}
		Fvector Offset = Fvector().sub(Candidate, SourcePosition);
		Offset.y = 0.f;
		const float NextDistance = Offset.magnitude();
		if ((Distance < MinimumDistance && NextDistance < Distance) || (Distance >= ViewDistance && NextDistance > Distance))
		{
			continue;
		}
		Offset.normalize_safe();
		const u32 StartVertex = Monster.ai_location().level_vertex_id();
		const bool Direct = ai().level_graph().valid_vertex_id(StartVertex) &&
			ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(StartVertex, Origin, Candidate));
		float Score = -Offset.dotproduct(WatchedForward);
		Fvector Travel = Fvector().sub(Candidate, Origin);
		Travel.y = 0.f;
		Travel.normalize_safe();
		// Evaluate facing at the END of a leg, where the path controller will actually orient the body.
		const float PeripheralLimit = std::cos(deg2rad(Profile.DefensiveViewAngle * .5f));
		const float EndFacing = -Offset.dotproduct(Travel);
		Score -= std::max(0.f, PeripheralLimit - EndFacing) * Profile.CharacterWatchCoverWeight;
		Score -= std::max(0.f, MinimumDistance - NextDistance) / std::max(EPS_L, MinimumDistance);
		Score -= std::max(0.f, NextDistance - ViewDistance) / ViewDistance;
		Score -= Side != WatchSide ? Profile.ReactionSideEpsilon : 0.f;
		if ((PreviousWatchAt && Now - PreviousWatchAt < Profile.CharacterWatchRevisitTime && Candidate.distance_to_sqr(PreviousWatchPoint) < _sqr(Profile.CharacterWatchMinLeg)) ||
			(OlderWatchAt && Now - OlderWatchAt < Profile.CharacterWatchRevisitTime && Candidate.distance_to_sqr(OlderWatchPoint) < _sqr(Profile.CharacterWatchMinLeg)))
		{
			Score -= Profile.CharacterWatchCoverWeight;
		}
		if (ExtendedEntityes::Navigation().TryGeometryRays(1))
		{
			Fvector Eye = SourcePosition;
			Fvector Body;
			Monster.Center(Body);
			Eye.y += WatchEyeHeight;
			Body.add(Fvector().sub(Candidate, Origin));
			Fvector Ray = Fvector().sub(Body, Eye);
			const float RayLength = Ray.magnitude();
			if (RayLength > EPS_L)
			{
				Ray.div(RayLength);
				const bool Covered = Level().ObjectSpace.RayTest(Eye, Ray, RayLength, collide::rqtStatic, nullptr, &Monster);
				Score += (WatchVisible ? Covered : !Covered) ? Profile.CharacterWatchCoverWeight : 0.f;
			}
		}
		// Prefer an immediately traversable graph segment over an attractive point behind a long detour.
		if (BestVertex == u32(-1) || (Direct && !BestIsDirect) || (Direct == BestIsDirect && Score > BestScore))
		{
			BestIsDirect = Direct;
			BestScore = Score;
			BestPoint = Candidate;
			BestVertex = Vertex;
			BestSide = Side;
		}
	}
	if (BestVertex == u32(-1))
	{
		return false;
	}
	OlderWatchPoint = PreviousWatchPoint;
	OlderWatchAt = PreviousWatchAt;
	PreviousWatchPoint = Origin;
	PreviousWatchAt = Now;
	WatchSide = BestSide;
	PlannedSourcePosition = SourcePosition;
	ReactionPosition = BestPoint;
	ReactionVertex = BestVertex;
	if (CELSharedDebug::Instance().CanLogCombat(Monster.Position()))
	{
		CELSharedDebug::Instance().LogCombat(Monster.ID(), "[monsters_debug] WATCH_POINT t=%u id=%u visible=%d side=%.0f score=%.2f view_limit=%.2f direct=%d goal=%.2f,%.2f,%.2f", Now, Monster.ID(), int(WatchVisible), WatchSide, BestScore, ViewDistance, int(BestIsDirect), BestPoint.x, BestPoint.y, BestPoint.z);
	}
	return true;
}

// Обновляет реакцию на опасную или интересующую точку и обрабатывает текущий результат выполнения.
void CELGroundReactionBehavior::Update(float DeltaTime)
{
	EvasionHardBlocked = false;
	if (IsObstaclePatrol && IsInspectingPoint)
	{
		if (Inspection.Update(Profile.CanInspect))
		{
			Movement.Stop();
			HasMoveCommand = false;
			ProgressTime = Device.dwTimeGlobal;
			Validation = {EELBehaviorState::Running, "Осмотр у преграды: затем продолжаю обход"};
			return;
		}
		IsInspectingPoint = false;
	}
	if (s32(Device.dwTimeGlobal - Until) >= 0)
	{
		Movement.Stop();
		Validation = {EELBehaviorState::Completed, "Реакция на шум завершена"};
		return;
	}
	if (HasEvasionGoal)
	{
		const char* Failure = nullptr;
		bool WaitingForBudget = false;
		if (CELGroundLocalSearch::MoveDirect(Adapter, Movement, EvasionGoal, true, &Failure, &WaitingForBudget))
		{
			if (Route.HasPath || HasRequestedGoal || Navigation.IsPending())
			{
				InvalidateRoute();
			}
			HasPoint = false;
			NextPoint = 0;
			HasMoveCommand = true;
			TrackProgress();
			Validation = {EELBehaviorState::Running, "Защитный рывок: разрываю линию огня"};
			return;
		}
		HasEvasionGoal = false;
		EvasionHardBlocked = !WaitingForBudget;
		Validation = {EELBehaviorState::Running, Failure};
	}
	TrackProgress();
	// A new leg gets its own progress window. Old lack of progress must not cancel every replacement.
	const u32 LegProgressAt = std::max(ProgressTime, PointStartedAt);
	if ((IsWatching || IsObstaclePatrol) && HasPoint && (HasMoveCommand || !Navigation.IsPending()) && Device.dwTimeGlobal - LegProgressAt >= Profile.ReactionCornerStallTime)
	{
		OlderWatchPoint = PreviousWatchPoint;
		OlderWatchAt = PreviousWatchAt;
		PreviousWatchPoint = ReactionPosition;
		PreviousWatchAt = Device.dwTimeGlobal;
		HasPoint = false;
		NextPoint = 0;
		WatchSide = -WatchSide;
	}
	if (IsObstaclePatrol && HasPoint && Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(Profile.ReactionArrivalDistance))
	{
		HasPoint = false;
		NextPoint = 0;
		IsInspectingPoint = Inspection.Update(Profile.CanInspect);
		if (IsInspectingPoint)
		{
			Movement.Stop();
			HasMoveCommand = false;
			ProgressTime = Device.dwTimeGlobal;
			Validation = {EELBehaviorState::Running, "Осмотр у преграды: затем продолжаю обход"};
			return;
		}
	}
	if ((IsFleeing || IsWatching) && HasPoint && Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(Profile.ReactionArrivalDistance))
	{
		HasPoint = false;
		NextPoint = 0;
	}
	if (!HasPoint && s32(Device.dwTimeGlobal - NextPoint) >= 0)
	{
		HasPoint = SelectPoint();
		if (HasPoint)
		{
			EscapeBlockedSince = 0;
		}
		else if (!EscapeBlockedSince)
		{
			EscapeBlockedSince = Device.dwTimeGlobal;
		}
		NextPoint = Device.dwTimeGlobal + Profile.GroundLocalRetry;
		PointStartedAt = Device.dwTimeGlobal;
	}
	if (!HasPoint)
	{
		if (!IsFleeing && !IsWatching && !IsObstaclePatrol && LocalSearch.Move(Adapter, Movement, Profile, SourcePosition, true))
		{
			HasMoveCommand = true;
			Validation = {EELBehaviorState::Running, "Обход недоступного источника шума"};
			return;
		}
		if ((IsFleeing || IsWatching) && HasMoveCommand && Device.dwTimeGlobal - ProgressTime < Profile.ReactionCornerStallTime)
		{
			Validation = {EELBehaviorState::Running, "Продолжаю безопасный участок: подбираю отход"};
			return;
		}
		Movement.Stop();
		HasMoveCommand = false;
		Validation = {EELBehaviorState::Running, "Нет безопасного прохода по АИ-сетке: выбираю обход"};
		return;
	}
	if (!IsFleeing && !IsWatching && !IsObstaclePatrol && Profile.CanExpandNoiseSearch && (Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(Profile.ReactionArrivalDistance) || Device.dwTimeGlobal - PointStartedAt >= Profile.SearchPointTime))
	{
		++SearchPhase;
		HasPoint = false;
		NextPoint = 0;
		Validation = {EELBehaviorState::Running, "Расширенный поиск вокруг источника шума"};
		Move(SourcePosition, ai().level_graph().vertex_id(SourcePosition), true);
		return;
	}
	if (Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(Profile.ReactionArrivalDistance))
	{
		Movement.Stop();
		HasPoint = !IsFleeing && !IsWatching;
		Validation = {EELBehaviorState::Running, IsFleeing ? "Бегство от шума" : "Осмотр источника шума"};
		ProgressTime = Device.dwTimeGlobal;
		return;
	}
	if (CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Search;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.Start = Adapter.Monster().Position();
		Event.Finish = ReactionPosition;
		Event.Radius = .4f;
		Event.Label = IsObstaclePatrol ? "Обход недоступной цели по АИ-сетке" : IsWatching ? "Осторожное сопровождение: свободная позиция" : IsFleeing ? "Уход от шума" : "Исследование шума";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	Validation = {EELBehaviorState::Running, IsWatching ? "Наблюдение за угрозой: смена позиции" : IsFleeing ? "Бегство от шума" : "Подход к источнику шума"};
	// Losing visual contact is not permission to relax while a dangerous enemy is still being followed.
	if (!Move(ReactionPosition, ReactionVertex, IsFleeing || IsWatching || IsObstaclePatrol))
	{
		if (IsWatching)
		{
			OlderWatchPoint = PreviousWatchPoint;
			OlderWatchAt = PreviousWatchAt;
			PreviousWatchPoint = ReactionPosition;
			PreviousWatchAt = Device.dwTimeGlobal;
			WatchSide = -WatchSide;
		}
		HasPoint = false;
		Validation = {EELBehaviorState::Blocked, "Путь к источнику шума перекрыт"};
	}
}

bool CELGroundReactionBehavior::IsCornered() const
{
	return (IsFleeing || IsWatching) && ((EscapeBlockedSince && Device.dwTimeGlobal - EscapeBlockedSince >= Profile.ReactionCornerBlockedTime) || (HasMoveCommand && Device.dwTimeGlobal - ProgressTime >= Profile.ReactionCornerStallTime));
}
