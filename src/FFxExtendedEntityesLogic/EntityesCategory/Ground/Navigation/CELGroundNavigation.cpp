#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundNavigation.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELSharedGroundGraph.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"

// Связывает подготовку наземного обхода с необходимыми службами и состоянием владельца.
CELGroundNavigation::CELGroundNavigation(IELAdapter& OwnerAdapter) : Adapter(OwnerAdapter) {}
// Завершает подготовку наземного обхода и освобождает связанные ресурсы.
CELGroundNavigation::~CELGroundNavigation()
{
	Cancel();
}

// Начинает захват наземного графа для конкретного назначения.
bool CELGroundNavigation::Prepare(const Fvector& GoalPosition, u32 GoalVertex, u32 PlanGeneration)
{
	if (IsPending() || !Adapter.CanReach(GoalPosition, GoalVertex))
	{
		return false;
	}
	CBaseMonster& OwnerMonster = Adapter.Monster();
	const u32 StartVertex = OwnerMonster.ai_location().level_vertex_id();
	if (!ai().level_graph().valid_vertex_id(StartVertex))
	{
		return false;
	}
	SELNavigationRequest Request;
	Request.Identity = {ExtendedEntityes::Navigation().GetGeneration(), ExtendedEntityes::OwnerGeneration(OwnerMonster.ID()), PlanGeneration, u16(OwnerMonster.ID())};
	Request.StartPosition = OwnerMonster.Position();
	CELSharedAnomalies::Snapshot(OwnerMonster.ID(), Request.Obstacles);
	CELSharedDeaths::AppendObstacles(OwnerMonster.ID(), Request.Obstacles);
	Request.GoalPosition = GoalPosition;
	SELNavigationNode StartNode;
	StartNode.VertexIdentifier = StartVertex;
	StartNode.Position = ai().level_graph().vertex_position(StartVertex);
	Request.Graph.push_back(StartNode);
	PendingIdentity = Request.Identity;
	HasCapture = CELSharedGroundGraph::Submit(std::move(Request), GoalVertex);
	HasPendingRequest = HasCapture;
	return HasCapture;
}

// Забирает готовый результат либо продолжает подготовку маршрута.
bool CELGroundNavigation::Poll(SELNavigationResult& Result)
{
	if (HasCapture)
	{
		const EELGraphCaptureState State = CELSharedGroundGraph::TakeState(PendingIdentity);
		if (State == EELGraphCaptureState::Capturing)
		{
			return false;
		}
		HasCapture = false;
		if (State == EELGraphCaptureState::Failed)
		{
			HasPendingRequest = false;
			Result = SELNavigationResult();
			Result.Identity = PendingIdentity;
			return true;
		}
	}
	return CELNavigationBase::Poll(Result);
}

// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELGroundNavigation::Cancel()
{
	if (HasCapture)
	{
		CELSharedGroundGraph::Cancel(PendingIdentity);
	}
	HasCapture = false;
	CELNavigationBase::Cancel();
}

// Сбрасывает временное состояние перед новым действием.
void CELGroundNavigation::Reset()
{
	Cancel();
}
