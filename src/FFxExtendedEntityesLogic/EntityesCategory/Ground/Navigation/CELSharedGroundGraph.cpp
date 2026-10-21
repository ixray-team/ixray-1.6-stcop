#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELSharedGroundGraph.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundAdapter.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"

namespace ExtendedEntityesGroundGraph
{
struct SFrontier
{
	u32 NodeIndex = 0;
	float Distance = 0.f;
	// Упорядочивает координатные ключи для поиска в общем реестре.
	bool operator<(const SFrontier& Other) const { return Distance > Other.Distance; }
};
struct SCapture
{
	SELNavigationRequest Request;
	SELNavigationIdentity Identity;
	xr_vector<SFrontier> Frontier;
	xr_map<u32, u32> NodeIndices;
	u32 Cursor = 0;
	u32 GoalVertex = u32(-1);
	u32 CreatedAt = 0;
	EELGraphCaptureState State = EELGraphCaptureState::Capturing;
};
xrCriticalSection CaptureLock;
xr_vector<SCapture> Captures;
u32 NextCapture = 0;

// Выполняет один ограниченный шаг расширения поисковых данных.
void Expand(SCapture& Capture, CELGroundAdapter& Adapter)
{
	ILevelGraph& Graph = ai().level_graph();
	std::pop_heap(Capture.Frontier.begin(), Capture.Frontier.end());
	Capture.Cursor = Capture.Frontier.back().NodeIndex;
	Capture.Frontier.pop_back();
	const u32 CurrentVertex = Capture.Request.Graph[Capture.Cursor].VertexIdentifier;
	ILevelGraph::const_iterator NeighborIterator;
	ILevelGraph::const_iterator NeighborEnd;
	u32 NeighborIndex = 0;
	u32 NeighborVertex;
	u32 NewIndex;
	SELNavigationNode NewNode;
	xr_map<u32, u32>::iterator ExistingNode;
	Graph.begin(CurrentVertex, NeighborIterator, NeighborEnd);
	for (; NeighborIterator != NeighborEnd && NeighborIndex < 4; ++NeighborIterator, ++NeighborIndex)
	{
		NeighborVertex = Graph.value(CurrentVertex, NeighborIterator);
		if (!Graph.valid_vertex_id(NeighborVertex))
		{
			continue;
		}
		ExistingNode = Capture.NodeIndices.find(NeighborVertex);
		if (ExistingNode == Capture.NodeIndices.end())
		{
			if (Capture.Request.Graph.size() >= CELSharedSettings::Work().GroundCaptureNodeLimit)
			{
				continue;
			}
			NewNode = SELNavigationNode();
			NewNode.Position = Graph.vertex_position(NeighborVertex);
			NewNode.VertexIdentifier = NeighborVertex;
			if (!Adapter.CanReach(NewNode.Position, NeighborVertex))
			{
				continue;
			}
			NewIndex = u32(Capture.Request.Graph.size());
			Capture.NodeIndices.emplace(NeighborVertex, NewIndex);
			Capture.Request.Graph.push_back(NewNode);
			Capture.Frontier.push_back({NewIndex, NewNode.Position.distance_to_sqr(Capture.Request.GoalPosition)});
			std::push_heap(Capture.Frontier.begin(), Capture.Frontier.end());
		}
		else
		{
			NewIndex = ExistingNode->second;
		}
		Capture.Request.Graph[Capture.Cursor].Neighbors[NeighborIndex] = NewIndex;
	}
	ExistingNode = Capture.NodeIndices.find(Capture.GoalVertex);
	if (ExistingNode != Capture.NodeIndices.end())
	{
		Capture.Request.GoalNode = ExistingNode->second;
		Capture.State = ExtendedEntityes::Navigation().Submit(std::move(Capture.Request)) ? EELGraphCaptureState::Submitted : EELGraphCaptureState::Failed;
		Capture.NodeIndices.clear();
	}
	else if (Capture.Frontier.empty() || Capture.Request.Graph.size() >= CELSharedSettings::Work().GroundCaptureNodeLimit)
	{
		Capture.State = EELGraphCaptureState::Failed;
	}
}

// Обрабатывает очередную ограниченную порцию накопленных задач.
void Process(SCapture& Capture)
{
	CEntityAlive* OwnerEntity = ExtendedEntityes::FindEntity(Capture.Identity.OwnerIdentifier);
	CBaseMonster* OwnerMonster = OwnerEntity ? smart_cast<CBaseMonster*>(OwnerEntity) : nullptr;
	if (!OwnerMonster || !OwnerMonster->g_Alive() ||
		Capture.Identity.OwnerGeneration != ExtendedEntityes::OwnerGeneration(OwnerMonster->ID()) ||
		Device.dwTimeGlobal - Capture.CreatedAt > CELSharedSettings::Work().GroundCaptureTimeout || !ai().get_level_graph())
	{
		Capture.State = EELGraphCaptureState::Failed;
		return;
	}
	CELGroundAdapter Adapter(*OwnerMonster);
	Expand(Capture, Adapter);
}
} // namespace ExtendedEntityesGroundGraph

// Помещает заявку в очередь с проверкой допустимости и доступного места.
bool CELSharedGroundGraph::Submit(SELNavigationRequest Request, u32 GoalVertex)
{
	using namespace ExtendedEntityesGroundGraph;
	xrCriticalSectionGuard Guard(CaptureLock);
	if (Captures.size() >= CELSharedSettings::Work().GroundCaptureTaskLimit || Request.Graph.size() != 1)
	{
		return false;
	}
	SCapture Capture;
	Capture.GoalVertex = GoalVertex;
	Capture.CreatedAt = Device.dwTimeGlobal;
	Capture.Identity = Request.Identity;
	Capture.Frontier.push_back({0, Request.Graph.front().Position.distance_to_sqr(Request.GoalPosition)});
	Capture.NodeIndices.emplace(Request.Graph.front().VertexIdentifier, 0);
	Capture.Request = std::move(Request);
	Captures.push_back(std::move(Capture));
	return true;
}

// Забирает состояние и готовый снимок наземного графа.
EELGraphCaptureState CELSharedGroundGraph::TakeState(const SELNavigationIdentity& Identity)
{
	using namespace ExtendedEntityesGroundGraph;
	xrCriticalSectionGuard Guard(CaptureLock);
	xr_vector<SCapture>::iterator CaptureIterator;
	EELGraphCaptureState State;
	for (CaptureIterator = Captures.begin(); CaptureIterator != Captures.end(); ++CaptureIterator)
	{
		if (CaptureIterator->Identity != Identity)
		{
			continue;
		}
		State = CaptureIterator->State;
		if (State != EELGraphCaptureState::Capturing)
		{
			Captures.erase(CaptureIterator);
		}
		return State;
	}
	return EELGraphCaptureState::Failed;
}

// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELSharedGroundGraph::Cancel(const SELNavigationIdentity& Identity)
{
	using namespace ExtendedEntityesGroundGraph;
	xrCriticalSectionGuard Guard(CaptureLock);
	xr_vector<SCapture>::iterator CaptureIterator = Captures.begin();
	while (CaptureIterator != Captures.end())
	{
		if (CaptureIterator->Identity == Identity)
		{
			CaptureIterator = Captures.erase(CaptureIterator);
		}
		else
		{
			++CaptureIterator;
		}
	}
}

// Порционно захватывает штатный наземный граф с сохранением курсора очереди.
void CELSharedGroundGraph::BeginFrame()
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Ground, EELDebugStage::Graph);
	using namespace ExtendedEntityesGroundGraph;
	xrCriticalSectionGuard Guard(CaptureLock);
	u32 Skipped = 0;
	SCapture* Capture;
	while (!Captures.empty() && Skipped < Captures.size())
	{
		NextCapture %= u32(Captures.size());
		Capture = &Captures[NextCapture++];
		if (Capture->State != EELGraphCaptureState::Capturing)
		{
			++Skipped;
			continue;
		}
		Skipped = 0;
		if (!ExtendedEntityes::Navigation().TryCaptureNode())
		{
			--NextCapture;
			break;
		}
		Process(*Capture);
	}
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedGroundGraph::Clear()
{
	xrCriticalSectionGuard Guard(ExtendedEntityesGroundGraph::CaptureLock);
	ExtendedEntityesGroundGraph::Captures.clear();
	ExtendedEntityesGroundGraph::NextCapture = 0;
}

// Возвращает число заявок, ожидающих обработки или получения результата.
u32 CELSharedGroundGraph::OutstandingRequests()
{
	xrCriticalSectionGuard Guard(ExtendedEntityesGroundGraph::CaptureLock);
	return u32(ExtendedEntityesGroundGraph::Captures.size());
}
