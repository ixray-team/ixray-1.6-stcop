#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include <cmath>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundReactionBehavior.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai_space.h"
#include "level_graph.h"

// Настраивает условия следующего запуска схемы.
void CELGroundReactionBehavior::Configure(const Fvector& SoundPosition, u32 ReactionUntil, bool ShouldFlee)
{
	if (IsFleeing != ShouldFlee || (HasPoint ? PlannedSourcePosition : SourcePosition).distance_to_sqr(SoundPosition) > 4.f)
	{
		HasPoint = false;
		NextPoint = 0;
		SearchPhase = 0;
	}
	SourcePosition = SoundPosition;
	Until = ReactionUntil;
	ActionUntil = Until;
	IsFleeing = ShouldFlee;
}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundReactionBehavior::Enter()
{
	CELGroundBehaviorBase::Enter();
	HasPoint = false;
	NextPoint = 0;
	SearchPhase = 0;
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
	for (Attempt = 0; Attempt < 8; ++Attempt)
	{
		if (IsFleeing)
		{
			Angle = Heading + float(Attempt % 2 ? s32(Attempt) : -s32(Attempt)) * PI_DIV_6;
			Candidate = Adapter.Monster().Position();
			Candidate.x -= std::sin(Angle) * 12.f;
			Candidate.z += std::cos(Angle) * 12.f;
		}
		else
		{
			Candidate = SourcePosition;
			if (SearchPhase || Attempt)
			{
				const u32 Phase = SearchPhase + Attempt;
				Angle = float(Phase) * (PI * .25f) + float(Adapter.Monster().ID() % 8) * (PI * .25f);
				const float Radius = std::min(Profile.SearchRadius, 2.f + float(Phase / 8) * 3.f);
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
	if (IsFleeing && HasPoint && Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < _sqr(CELSharedSettings::Work().GroundWaypointLookahead))
	{
		HasPoint = false;
		NextPoint = 0;
	}
	if (!HasPoint && s32(Device.dwTimeGlobal - NextPoint) >= 0)
	{
		HasPoint = SelectPoint();
		NextPoint = Device.dwTimeGlobal + Profile.GroundLocalRetry;
		PointStartedAt = Device.dwTimeGlobal;
	}
	if (!HasPoint)
	{
		if (LocalSearch.Move(Adapter, Movement, Profile, SourcePosition, true))
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
	if (!IsFleeing && Profile.CanExpandNoiseSearch && (Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < 2.25f || Device.dwTimeGlobal - PointStartedAt >= Profile.SearchPointTime))
	{
		++SearchPhase;
		HasPoint = false;
		NextPoint = 0;
		Validation = {EELBehaviorState::Running, "Расширенный поиск вокруг источника шума"};
		Move(SourcePosition, ai().level_graph().vertex_id(SourcePosition), true);
		return;
	}
	if (Adapter.Monster().Position().distance_to_sqr(ReactionPosition) < 2.25f)
	{
		Movement.Stop();
		HasPoint = !IsFleeing;
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
		Event.Label = IsFleeing ? "Уход от шума" : "Исследование шума";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	Validation = {EELBehaviorState::Running, IsFleeing ? "Бегство от шума" : "Подход к источнику шума"};
	if (!Move(ReactionPosition, ReactionVertex, true))
	{
		HasPoint = false;
		Validation = {EELBehaviorState::Blocked, "Путь к источнику шума перекрыт"};
	}
}
