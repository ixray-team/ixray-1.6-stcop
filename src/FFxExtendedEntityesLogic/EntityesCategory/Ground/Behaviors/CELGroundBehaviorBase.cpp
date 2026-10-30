#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorBase.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"
#include "xrGame/monster_scale.h"
#include "xrGame/movement_manager.h"
#include "xrGame/detail_path_manager.h"

// Связывает базовую наземную схему и контроль продвижения с необходимыми службами и состоянием владельца.
CELGroundBehaviorBase::CELGroundBehaviorBase(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : Adapter(OwnerAdapter), Profile(OwnerProfile), Movement(OwnerAdapter), Navigation(OwnerAdapter) {}

// Обновляет известные сведения о цели для текущей схемы.
void CELGroundBehaviorBase::SetObservation(const SELObservation& NewObservation)
{
	if (Observation.TargetIdentifier != NewObservation.TargetIdentifier)
	{
		StartedAt = ProgressTime = WaitingSince = Device.dwTimeGlobal;
		RecoveryCount = 0;
		// A different enemy must not inherit the old enemy's route or local detour.
		InvalidateRoute();
	}
	Observation = NewObservation;
}

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELGroundBehaviorBase::Enter()
{
	StartedAt = ProgressTime = WaitingSince = Device.dwTimeGlobal;
	ProgressPosition = Adapter.Monster().Position();
	RecoveryCount = 0;
	HasMoveCommand = false;
	RetryAfter = DirectCheckAfter = 0;
	HasRequestedGoal = false;
	Validation = SELBehaviorValidation();
	LocalSearch.Reset();
	ActionUntil = 0;
}

// Завершает активную схему и освобождает временные заявки.
void CELGroundBehaviorBase::Exit()
{
	Adapter.Monster().movement().detail().SetMovementFactor(1.f);
	Navigation.Reset();
	Route = SELNavigationResult();
	RouteCursor = 0;
	++PlanGeneration;
	HasMoveCommand = false;
	HasRequestedGoal = false;
}

// Discard obsolete movement plans without restarting animation or the behavior itself.
void CELGroundBehaviorBase::InvalidateRoute()
{
	Navigation.Reset();
	Route = SELNavigationResult();
	RouteCursor = 0;
	LocalSearch.Reset();
	HasRequestedGoal = HasMoveCommand = false;
	RetryAfter = 0;
	WaitingSince = ProgressTime = Device.dwTimeGlobal;
	ProgressPosition = Adapter.Monster().Position();
	++PlanGeneration;
}

// Обновляет контроль продвижения по позиции владельца.
void CELGroundBehaviorBase::TrackProgress()
{
	if (ProgressPosition.distance_to_sqr(Adapter.Monster().Position()) >= .5625f * _sqr(std::min(1.f, MonsterScale::Movement(Adapter.Monster().GetVisualScale()))))
	{
		ProgressPosition = Adapter.Monster().Position();
		ProgressTime = WaitingSince = Device.dwTimeGlobal;
		RecoveryCount = 0;
	}
}

// Принимает новый путь отдельно от исполняемого и заново находит курсор возле текущей позиции.
void CELGroundBehaviorBase::TakePreparedRoute()
{
	SELNavigationResult Candidate;
	if (!Navigation.Poll(Candidate))
	{
		return;
	}
	if (!Candidate.HasPath || Candidate.Points.empty() || Candidate.Vertices.size() != Candidate.Points.size())
	{
		HasRequestedGoal = false;
		RetryAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
		return;
	}
	const Fvector& Position = Adapter.Monster().Position();
	const u32 StartVertex = Adapter.Monster().ai_location().level_vertex_id();
	if (!ai().level_graph().valid_vertex_id(StartVertex))
	{
		HasRequestedGoal = false;
		return;
	}
	float BestDistance = FLT_MAX;
	float Distance;
	u32 BestCursor = u32(-1);
	u32 PointIndex;
	for (PointIndex = 0; PointIndex < Candidate.Points.size(); ++PointIndex)
	{
		Distance = Position.distance_to_sqr(Candidate.Points[PointIndex]);
		if (Distance < BestDistance && ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(StartVertex, Position, Candidate.Points[PointIndex])))
		{
			BestDistance = Distance;
			BestCursor = PointIndex;
		}
	}
	if (BestCursor == u32(-1))
	{
		HasRequestedGoal = false;
		RetryAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
		return;
	}
	Route = std::move(Candidate);
	RouteCursor = BestCursor;
	WaitingSince = Device.dwTimeGlobal;
}

// Пропускает пройденные точки и выбирает видимый участок пути впереди вместо остановки на каждом узле.
void CELGroundBehaviorBase::AdvanceRoute()
{
	if (!Route.HasPath || RouteCursor >= Route.Points.size())
	{
		return;
	}
	const Fvector& Position = Adapter.Monster().Position();
	while (RouteCursor + 1 < Route.Points.size() && Position.distance_to_sqr(Route.Points[RouteCursor]) < 1.f)
	{
		++RouteCursor;
	}
	if (RouteCursor + 1 == Route.Points.size() && Position.distance_to_sqr(Route.Points.back()) < _sqr(MonsterScale::Arrival(Adapter.Monster().GetVisualScale())))
	{
		Route.HasPath = false;
		return;
	}
	const float LookaheadSquared = _sqr(CELSharedSettings::Work().GroundWaypointLookahead);
	const u32 StartVertex = Adapter.Monster().ai_location().level_vertex_id();
	u32 PointIndex;
	for (PointIndex = RouteCursor + 1; PointIndex < Route.Points.size(); ++PointIndex)
	{
		if (Position.distance_to_sqr(Route.Points[PointIndex]) > LookaheadSquared ||
			!ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(StartVertex, Position, Route.Points[PointIndex])) ||
			CELSharedAnomalies::Crosses(Adapter.Monster().ID(), Position, Route.Points[PointIndex]) || CELSharedDeaths::Crosses(Adapter.Monster().ID(), Position, Route.Points[PointIndex]))
		{
			break;
		}
		RouteCursor = PointIndex;
	}
}

// Запрашивает изменившееся назначение, пока прежний проверенный путь ещё ведёт монстра вперёд.
void CELGroundBehaviorBase::PrepareContinuation(const Fvector& GoalPosition, u32 GoalVertex)
{
	if (s32(Device.dwTimeGlobal - RetryAfter) < 0 || Adapter.Monster().Position().distance_to_sqr(GoalPosition) < _sqr(MonsterScale::Arrival(Adapter.Monster().GetVisualScale())))
	{
		return;
	}
	const float GoalShiftSquared = RequestedGoal.distance_to_sqr(GoalPosition);
	if (Navigation.IsPending())
	{
		if (HasRequestedGoal && GoalShiftSquared >= _sqr(CELSharedSettings::Work().GroundRouteReplanDistance))
		{
			if (Navigation.Retarget(GoalPosition, GoalVertex))
			{
				RequestedGoal = GoalPosition;
				Validation = {EELBehaviorState::Running, "Цель сместилась: обновляю огибающую"};
			}
			RetryAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
		}
		return;
	}
	const bool IsNearEnd = Route.HasPath && Adapter.Monster().Position().distance_to_sqr(Route.Points.back()) < _sqr(CELSharedSettings::Work().GroundWaypointLookahead);
	if (Route.HasPath && HasRequestedGoal && GoalShiftSquared < _sqr(CELSharedSettings::Work().GroundRouteReplanDistance) && !(IsNearEnd && GoalShiftSquared >= 1.f))
	{
		return;
	}
	if (Navigation.Prepare(GoalPosition, GoalVertex, ++PlanGeneration))
	{
		RequestedGoal = GoalPosition;
		HasRequestedGoal = true;
	}
	RetryAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
}

// Исполняет только проверенный путь; ожидающий следующий расчёт не выключает текущий.
bool CELGroundBehaviorBase::FollowRoute(bool IsRunning)
{
	if (CELSharedAnomalies::Crosses(Adapter.Monster().ID(), Adapter.Monster().Position(), Route.Points[RouteCursor]) || CELSharedDeaths::Crosses(Adapter.Monster().ID(), Adapter.Monster().Position(), Route.Points[RouteCursor]))
	{
		Movement.Stop();
		// The next attempt needs a fresh hazard snapshot rather than the same rejected cached goal.
		InvalidateRoute();
		RetryAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
		Validation = {EELBehaviorState::Blocked, "Опасная зона на пути: перепланирование обхода"};
		return false;
	}
	if (CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Route;
		Event.OwnerIdentifier = Adapter.Monster().ID();
		Event.Start = Adapter.Monster().Position();
		Event.Finish = Route.Points.back();
		Event.Points = Route.Points;
		Event.Radius = .25f;
		Event.Label = "Исполняемый маршрут";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
	HasMoveCommand = Movement.MoveTo(Route.Points[RouteCursor], Route.Vertices[RouteCursor], IsRunning);
	Validation.State = EELBehaviorState::Running;
	return HasMoveCommand;
}

// Сопровождает текущий путь и параллельно принимает либо готовит его замену.
bool CELGroundBehaviorBase::Move(const Fvector& GoalPosition, u32 GoalVertex, bool IsRunning)
{
	HasMoveCommand = false;
	TrackProgress();
	TakePreparedRoute();
	AdvanceRoute();
	if (Route.HasPath && s32(Device.dwTimeGlobal - DirectCheckAfter) >= 0)
	{
		DirectCheckAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
		Fvector DirectPoint;
		u32 DirectVertex;
		if (CELGroundLocalSearch::Resolve(Adapter, GoalPosition, DirectPoint, DirectVertex) &&
			CELGroundLocalSearch::MoveDirect(Adapter, Movement, DirectPoint, IsRunning))
		{
			// Replace the obsolete detour only after the current destination has a safe direct corridor.
			Navigation.Reset();
			Route = SELNavigationResult();
			Route.HasPath = true;
			Route.Points.push_back(DirectPoint);
			Route.Vertices.push_back(DirectVertex);
			RouteCursor = 0;
			RequestedGoal = DirectPoint;
			HasRequestedGoal = HasMoveCommand = true;
			LocalSearch.Reset();
			Validation = {EELBehaviorState::Running, "Прямой проход освободился: сокращаю обход"};
			return true;
		}
	}
	PrepareContinuation(GoalPosition, GoalVertex);
	if (Route.HasPath && RouteCursor < Route.Points.size() && Device.dwTimeGlobal - ProgressTime < Profile.GroundStallTime)
	{
		LocalSearch.Reset();
		return FollowRoute(IsRunning);
	}
	if (Adapter.Monster().Position().distance_to_sqr(GoalPosition) >= _sqr(MonsterScale::Arrival(Adapter.Monster().GetVisualScale())) && LocalSearch.Move(Adapter, Movement, Profile, GoalPosition, IsRunning))
	{
		HasMoveCommand = true;
		Validation = {EELBehaviorState::Running, "Локальный обход: основной поиск продолжается"};
		return true;
	}
	if (Adapter.Monster().Position().distance_to_sqr(GoalPosition) < _sqr(MonsterScale::Arrival(Adapter.Monster().GetVisualScale())))
	{
		Validation = {EELBehaviorState::Running, "Точка достигнута"};
		ProgressTime = Device.dwTimeGlobal;
	}
	else
	{
		Validation = {Navigation.IsPending() ? EELBehaviorState::WaitingForResult : EELBehaviorState::WaitingForBudget, "Нет готового безопасного продолжения"};
	}
	Movement.Stop();
	return true;
}

// Возвращает состояние выполнения схемы, включая ожидание, завершение и сбои.
SELBehaviorValidation CELGroundBehaviorBase::Validate() const
{
	if (!Adapter.CanControl())
	{
		return {EELBehaviorState::InvalidTarget, "Внешнее управление"};
	}
	if (Device.dwTimeGlobal - ProgressTime >= 3000 && HasMoveCommand)
	{
		return {EELBehaviorState::NoProgress, "Нет движения"};
	}
	if (!HasMoveCommand && Device.dwTimeGlobal - WaitingSince >= CELSharedSettings::Work().GroundCaptureTimeout + 500 &&
		(Validation.State == EELBehaviorState::WaitingForResult || Validation.State == EELBehaviorState::WaitingForBudget))
	{
		return {EELBehaviorState::NoProgress, "Истёк срок подготовки пути"};
	}
	SELBehaviorValidation Result = Validation;
	Result.StartedAt = StartedAt;
	Result.Deadline = ActionUntil;
	Result.WaitingSince = WaitingSince;
	return Result;
}

// Выбирает ограниченное восстановление после сбоя либо освобождает схему.
EELRecovery CELGroundBehaviorBase::Recover(const SELBehaviorValidation& Failure)
{
	if (Failure.State == EELBehaviorState::InvalidTarget)
	{
		return EELRecovery::Release;
	}
	if (++RecoveryCount > 3)
	{
		return EELRecovery::Release;
	}
	ProgressTime = WaitingSince = Device.dwTimeGlobal;
	Navigation.Reset();
	Route = SELNavigationResult();
	HasRequestedGoal = false;
	RetryAfter = Device.dwTimeGlobal + CELSharedSettings::Work().GroundRouteRetryDelay;
	return EELRecovery::Continue;
}
