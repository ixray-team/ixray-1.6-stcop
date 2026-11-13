#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include <cmath>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundReactionBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"

// Настраивает условия следующего запуска схемы.
void CELGroundReactionBehavior::Configure(const Fvector& SoundPosition, u32 ReactionUntil, bool ShouldFlee)
{
	if (IsFleeing != ShouldFlee || (HasPoint ? PlannedSourcePosition : SourcePosition).distance_to_sqr(SoundPosition) > _sqr(Profile.ReactionReplanDistance))
	{
		HasPoint = false;
		NextPoint = 0;
		SearchPhase = 0;
	}
	IsWatching = false;
	SourcePosition = SoundPosition;
	Until = ReactionUntil;
	ActionUntil = Until;
	IsFleeing = ShouldFlee;
}

// Reuse bounded graph navigation for cautious following; enemy orientation is sampled only while perceived.
void CELGroundReactionBehavior::ConfigureWatching(const Fvector& EnemyPosition, const Fvector& EnemyForward, u32 WatchUntil)
{
	const bool WasWatching = IsWatching;
	Configure(EnemyPosition, WatchUntil, false);
	IsWatching = true;
	WatchedForward = EnemyForward;
	WatchedForward.y = 0.f;
	WatchedForward.normalize_safe();
	if (!WasWatching)
	{
		HasPoint = false;
		NextPoint = 0;
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
		if (IsWatching)
		{
			// Move around the current side of the enemy instead of crossing through it.
			const float Cross = WatchedForward.z * Away.x - WatchedForward.x * Away.z;
			const float Side = std::abs(Cross) > Profile.ReactionSideEpsilon ? (Cross > 0.f ? 1.f : -1.f) : (Adapter.Monster().ID() % 2 ? 1.f : -1.f);
			const float Step = Profile.CharacterWatchAngle + Profile.CharacterWatchAngleStep * float(Attempt / 2);
			Angle = atan2f(Away.x, Away.z) + Side * Step * (Attempt % 2 ? -1.f : 1.f);
			Candidate = SourcePosition;
			Candidate.x += std::sin(Angle) * Profile.CharacterWatchDistance;
			Candidate.z += std::cos(Angle) * Profile.CharacterWatchDistance;
		}
		else if (IsFleeing)
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

// Обновляет реакцию на опасную или интересующую точку и обрабатывает текущий результат выполнения.
void CELGroundReactionBehavior::Update(float DeltaTime)
{
	if (s32(Device.dwTimeGlobal - Until) >= 0)
	{
		Movement.Stop();
		Validation = {EELBehaviorState::Completed, "Реакция на шум завершена"};
		return;
	}
	if ((IsFleeing || IsWatching) && HasPoint && Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(CELSharedSettings::Work().GroundWaypointLookahead))
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
		if (!IsFleeing && !IsWatching && LocalSearch.Move(Adapter, Movement, Profile, SourcePosition, true))
		{
			HasMoveCommand = true;
			Validation = {EELBehaviorState::Running, "Обход недоступного источника шума"};
			return;
		}
		Movement.Stop();
		HasMoveCommand = false;
		Validation = {EELBehaviorState::Running, "Нет безопасного прохода по АИ-сетке: выбираю обход"};
		return;
	}
	if (!IsFleeing && !IsWatching && Profile.CanExpandNoiseSearch && (Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(Profile.ReactionArrivalDistance) || Device.dwTimeGlobal - PointStartedAt >= Profile.SearchPointTime))
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
		Event.Label = IsWatching ? "Осторожное сопровождение: боковой обход" : IsFleeing ? "Уход от шума" : "Исследование шума";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	Validation = {EELBehaviorState::Running, IsWatching ? "Наблюдение за угрозой: смена позиции" : IsFleeing ? "Бегство от шума" : "Подход к источнику шума"};
	if (!Move(ReactionPosition, ReactionVertex, !IsWatching || Adapter.Monster().Position().distance_to_sqr(SourcePosition) > _sqr(Profile.CharacterWatchDistance * Profile.CharacterWatchRunRatio)))
	{
		HasPoint = false;
		Validation = {EELBehaviorState::Blocked, "Путь к источнику шума перекрыт"};
	}
}

bool CELGroundReactionBehavior::IsCornered() const
{
	return IsFleeing && ((EscapeBlockedSince && Device.dwTimeGlobal - EscapeBlockedSince >= Profile.ReactionCornerBlockedTime) || (HasMoveCommand && Device.dwTimeGlobal - ProgressTime >= Profile.ReactionCornerStallTime));
}
