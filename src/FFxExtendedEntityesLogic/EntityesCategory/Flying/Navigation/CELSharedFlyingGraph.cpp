#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include <cmath>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingGraph.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingSpaceCache.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "xrGame/entity_alive.h"

namespace ExtendedEntityesAirGraph
{
struct SCell
{
	s32 Horizontal = 0;
	s32 Vertical = 0;
	s32 Lateral = 0;
	// Упорядочивает координатные ключи для поиска в общем реестре.
	bool operator<(const SCell& Other) const
	{
		if (Horizontal != Other.Horizontal)
		{
			return Horizontal < Other.Horizontal;
		}
		if (Vertical != Other.Vertical)
		{
			return Vertical < Other.Vertical;
		}
		return Lateral < Other.Lateral;
	}
};
struct SNode
{
	SCell Cell;
	float Cost = FLT_MAX;
	u32 NextEdge = 0;
	bool Expanded = false;
};
struct SCapture
{
	SELNavigationRequest Request;
	xr_vector<SNode> Nodes;
	xr_map<SCell, u32> CellNodes;
	Fbox Bounds;
	SCell NeighborDirections[6];
	SCell CandidateCell;
	Fvector CandidatePosition = {0.f, 0.f, 0.f};
	float Radius = 0.f;
	float CenterHeight = 0.f;
	float CellSize = 0.f;
	u32 NodeLimit = 0;
	u32 Deadline = 0;
	u32 OwnerCheckedFrame = u32(-1);
	u32 CurrentNode = 0;
	u32 Edge = 0;
	bool HasEdge = false;
	bool IsCheckingGoal = false;
	EELGeometryResult State = EELGeometryResult::Pending;
};
const SCell Directions[] = {{0, 1, 0}, {1, 0, 0}, {0, 0, 1}, {0, 0, -1}, {-1, 0, 0}, {0, -1, 0}};

// Выбирает следующий перспективный узел локального графа.
bool SelectNode(SCapture& Capture)
{
	u32 NodeIndex;
	u32 BestNode = u32(-1);
	float BestScore = FLT_MAX;
	float Score;
	for (NodeIndex = 0; NodeIndex < Capture.Nodes.size(); ++NodeIndex)
	{
		if (NodeIndex == Capture.Request.GoalNode || Capture.Nodes[NodeIndex].Expanded)
		{
			continue;
		}
		// Capture a useful corridor first; the worker still finds a path on the certified graph.
		Score = Capture.Request.Graph[NodeIndex].Position.distance_to(Capture.Request.GoalPosition) + Capture.Nodes[NodeIndex].Cost * .01f;
		if (Score < BestScore)
		{
			BestScore = Score;
			BestNode = NodeIndex;
		}
	}
	if (BestNode == u32(-1))
	{
		Capture.State = EELGeometryResult::Blocked;
		return false;
	}
	Capture.CurrentNode = BestNode;
	Capture.Edge = Capture.Nodes[BestNode].NextEdge;
	return true;
}

// Retain unfinished branches, but allow a promising new node to be probed immediately.
void AdvanceEdge(SCapture& Capture)
{
	SNode& Node = Capture.Nodes[Capture.CurrentNode];
	Node.NextEdge = Capture.Edge + 1u;
	Node.Expanded = Node.NextEdge >= 7u;
	Capture.Edge = 7u;
}

// Добавляет проверенное ребро и при необходимости соседний узел в снимок.
void AcceptEdge(SCapture& Capture)
{
	if (Capture.Edge == 0)
	{
		Capture.Request.Graph[Capture.CurrentNode].Neighbors[6] = Capture.Request.GoalNode;
		Capture.State = EELGeometryResult::Clear;
		return;
	}
	xr_map<SCell, u32>::iterator Existing = Capture.CellNodes.find(Capture.CandidateCell);
	u32 NodeIndex;
	const float Cost = Capture.Nodes[Capture.CurrentNode].Cost + Capture.Request.Graph[Capture.CurrentNode].Position.distance_to(Capture.CandidatePosition);
	if (Existing == Capture.CellNodes.end())
	{
		if (Capture.Nodes.size() >= Capture.NodeLimit)
		{
			return;
		}
		NodeIndex = u32(Capture.Nodes.size());
		SNode Node;
		Node.Cell = Capture.CandidateCell;
		Node.Cost = Cost;
		Capture.Nodes.push_back(Node);
		SELNavigationNode GraphNode;
		GraphNode.Position = Capture.CandidatePosition;
		Capture.Request.Graph.push_back(GraphNode);
		Capture.CellNodes.emplace(Capture.CandidateCell, NodeIndex);
	}
	else
	{
		NodeIndex = Existing->second;
		Capture.Nodes[NodeIndex].Cost = std::min(Capture.Nodes[NodeIndex].Cost, Cost);
	}
	Capture.Request.Graph[Capture.CurrentNode].Neighbors[Capture.Edge - 1] = NodeIndex;
}

// Принимает результат геометрической проверки текущего ребра.
void FinishEdge(SCapture& Capture)
{
	SELNavigationResult CheckedEdge;
	const EELGeometryResult State = CELFlyingGeometry::Take(Capture.Request.Identity, CheckedEdge);
	if (State == EELGeometryResult::Pending)
	{
		return;
	}
	Capture.HasEdge = false;
	const Fvector& EdgeStart = Capture.IsCheckingGoal ? Capture.Request.GoalPosition : Capture.Request.Graph[Capture.CurrentNode].Position;
	const Fvector& EdgeEnd = Capture.IsCheckingGoal ? Capture.Request.GoalPosition : Capture.CandidatePosition;
	CELSharedFlyingSpaceCache::Remember(EdgeStart, EdgeEnd, Capture.Radius, Capture.CenterHeight, State);
	if (Capture.IsCheckingGoal)
	{
		Capture.IsCheckingGoal = false;
		if (State == EELGeometryResult::Blocked)
		{
			Capture.State = EELGeometryResult::Blocked;
		}
		return;
	}
	if (State == EELGeometryResult::Clear)
	{
		AcceptEdge(Capture);
	}
	AdvanceEdge(Capture);
}

// Начинает бюджетную проверку следующего ребра воздушного графа.
bool StartEdge(SCapture& Capture)
{
	if (Capture.Edge == 7 && !SelectNode(Capture))
	{
		return true;
	}
	Capture.CandidatePosition = Capture.Request.GoalPosition;
	if (Capture.Edge != 0)
	{
		Capture.CandidateCell = Capture.Nodes[Capture.CurrentNode].Cell;
		const SCell& Direction = Capture.NeighborDirections[Capture.Edge - 1];
		Capture.CandidateCell.Horizontal += Direction.Horizontal;
		Capture.CandidateCell.Vertical += Direction.Vertical;
		Capture.CandidateCell.Lateral += Direction.Lateral;
		Capture.CandidatePosition.set(float(Capture.CandidateCell.Horizontal) * Capture.CellSize, float(Capture.CandidateCell.Vertical) * Capture.CellSize, float(Capture.CandidateCell.Lateral) * Capture.CellSize);
		const auto Existing = Capture.CellNodes.find(Capture.CandidateCell);
		if (Existing != Capture.CellNodes.end())
		{
			Capture.CandidatePosition = Capture.Request.Graph[Existing->second].Position;
		}
	}
	const Fvector& Start = Capture.Request.Graph[Capture.CurrentNode].Position;
	// Прямая кривая уже отклонена: та же хорда от старта создаст её повторно вместо обхода.
	if (Capture.Edge == 0 && Capture.CurrentNode == Capture.Request.StartNode && !Capture.Request.CanConnectStartToGoal)
	{
		AdvanceEdge(Capture);
		return true;
	}
	if (!Capture.Bounds.contains(Capture.CandidatePosition) || CELSharedAnomalies::Crosses(Capture.Request.Obstacles, Start, Capture.CandidatePosition))
	{
		AdvanceEdge(Capture);
		return true;
	}
	EELGeometryResult Cached;
	if (CELSharedFlyingSpaceCache::Find(Start, Capture.CandidatePosition, Capture.Radius, Capture.CenterHeight, Cached))
	{
		if (Cached == EELGeometryResult::Clear)
		{
			AcceptEdge(Capture);
		}
		AdvanceEdge(Capture);
		return true;
	}
	// Кэш и отбрасывание по границам не расходуют квоту новых геометрических проб.
	if (!ExtendedEntityes::Navigation().TryCaptureNode())
	{
		return false;
	}
	SELNavigationResult EdgeRoute;
	EdgeRoute.Identity = Capture.Request.Identity;
	EdgeRoute.HasPath = true;
	EdgeRoute.Points.push_back(Start);
	EdgeRoute.Points.push_back(Capture.CandidatePosition);
	Capture.HasEdge = CELFlyingGeometry::Submit(std::move(EdgeRoute), Capture.Radius, Capture.CenterHeight);
	return true;
}
} // namespace ExtendedEntityesAirGraph
struct CELSharedFlyingGraph::SState
{
	xr_vector<ExtendedEntityesAirGraph::SCapture> Captures;
	u32 NextCapture = 0;
	u32 SliceFrame = u32(-1);
	u32 FrameMicroseconds = 0;
};
// Возвращает общее хранилище состояния очереди.
CELSharedFlyingGraph::SState& CELSharedFlyingGraph::State()
{
	static SState Runtime;
	return Runtime;
}


// Помещает заявку в очередь с проверкой допустимости и доступного места.
bool CELSharedFlyingGraph::Submit(SELNavigationRequest Request, float Radius, float CenterHeight, float CellSize, u32 NodeLimit, u32 Timeout)
{
	using namespace ExtendedEntityesAirGraph;
	if (State().Captures.size() >= CELSharedSettings::Work().NavigationTaskLimit || !Request.IsFlying || NodeLimit < 2 || !_valid(CellSize) || CellSize <= EPS_L)
	{
		return false;
	}
	SCapture Capture;
	Capture.Request = std::move(Request);
	Capture.Request.Graph.clear();
	Capture.Request.StartNode = 0;
	Capture.Request.GoalNode = 1;
	Capture.Bounds = CELFlyingGeometry::AirBounds(Radius, Radius, CenterHeight);
	// Поиск не сможет соединить цель за границами допустимого воздушного объёма.
	if (!Capture.Bounds.contains(Capture.Request.StartPosition) || !Capture.Bounds.contains(Capture.Request.GoalPosition))
	{
		return false;
	}
	xr_vector<SELNavigationObstacle>::const_iterator Obstacle;
	for (Obstacle = Capture.Request.Obstacles.begin(); Obstacle != Capture.Request.Obstacles.end(); ++Obstacle)
	{
		// Снаружи нельзя закончить обход внутри запретной сферы; выход из уже занятой области сохраняется.
		if (Capture.Request.GoalPosition.distance_to_sqr(Obstacle->Position) < _sqr(Obstacle->Radius) && Capture.Request.StartPosition.distance_to_sqr(Obstacle->Position) >= _sqr(Obstacle->Radius))
		{
			return false;
		}
	}
	Capture.Radius = Radius;
	Capture.CenterHeight = CenterHeight;
	Capture.CellSize = CellSize;
	std::copy(std::begin(Directions), std::end(Directions), std::begin(Capture.NeighborDirections));
	const Fvector TowardGoal = Fvector().sub(Capture.Request.GoalPosition, Capture.Request.StartPosition);
	std::sort(std::begin(Capture.NeighborDirections), std::end(Capture.NeighborDirections), [&TowardGoal](const SCell& Left, const SCell& Right)
			  { return float(Left.Horizontal - Right.Horizontal) * TowardGoal.x + float(Left.Vertical - Right.Vertical) * TowardGoal.y + float(Left.Lateral - Right.Lateral) * TowardGoal.z > 0.f; });
	Capture.NodeLimit = std::min(NodeLimit, CELSharedSettings::Work().NavigationGraphNodeLimit);
	Capture.Deadline = Device.dwTimeGlobal + Timeout;
	SELNavigationNode StartNode;
	StartNode.Position = Capture.Request.StartPosition;
	SELNavigationNode GoalNode;
	GoalNode.Position = Capture.Request.GoalPosition;
	Capture.Request.Graph.push_back(StartNode);
	Capture.Request.Graph.push_back(GoalNode);
	Capture.Nodes.resize(2);
	Capture.Nodes[0].Cost = 0.f;
	Capture.Nodes[1].Expanded = true;
	Capture.Nodes[0].Cell = {s32(std::round(Capture.Request.StartPosition.x / CellSize)), s32(std::round(Capture.Request.StartPosition.y / CellSize)), s32(std::round(Capture.Request.StartPosition.z / CellSize))};
	Capture.CellNodes.emplace(Capture.Nodes[0].Cell, 0);
	// Недопустимый зазор под телом в самой цели не исправить расширением сотен соседей.
	EELGeometryResult CachedGoal;
	if (CELSharedFlyingSpaceCache::Find(Capture.Request.GoalPosition, Capture.Request.GoalPosition, Radius, CenterHeight, CachedGoal))
	{
		if (CachedGoal == EELGeometryResult::Blocked)
		{
			return false;
		}
		State().Captures.push_back(std::move(Capture));
		return true;
	}
	SELNavigationResult GoalProbe;
	GoalProbe.Identity = Capture.Request.Identity;
	GoalProbe.HasPath = true;
	GoalProbe.Points.push_back(Capture.Request.GoalPosition);
	GoalProbe.Points.push_back(Capture.Request.GoalPosition);
	Capture.HasEdge = CELFlyingGeometry::Submit(std::move(GoalProbe), Radius, CenterHeight);
	if (!Capture.HasEdge)
	{
		return false;
	}
	Capture.IsCheckingGoal = true;
	State().Captures.push_back(std::move(Capture));
	return true;
}

// Порционно захватывает проверенные воздушные рёбра с сохранением курсора очереди.
bool CELSharedFlyingGraph::BeginFrame()
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Flying, EELDebugStage::Graph);
	using namespace ExtendedEntityesAirGraph;
	if (State().SliceFrame != Device.dwFrame)
	{
		State().SliceFrame = Device.dwFrame;
		State().FrameMicroseconds = 0;
	}
	if (!HasFrameBudget())
	{
		return false;
	}
	CTimer SliceTimer;
	SliceTimer.Start();
	bool Advanced = false;
	CELSharedFlyingSpaceCache::BeginFrame();
	u32 Remaining = u32(State().Captures.size());
	SCapture* Capture;
	while (Remaining > 0 && !State().Captures.empty())
	{
		if (State().FrameMicroseconds + SliceTimer.GetElapsed_mcs() >= std::max(1u, CELSharedSettings::Work().NavigationSliceMicroseconds / 3))
		{
			break;
		}
		--Remaining;
		State().NextCapture %= u32(State().Captures.size());
		Capture = &State().Captures[State().NextCapture++];
		if (Capture->OwnerCheckedFrame != Device.dwFrame)
		{
			Capture->OwnerCheckedFrame = Device.dwFrame;
			const SELNavigationIdentity& Identity = Capture->Request.Identity;
			const CEntityAlive* Owner = ExtendedEntityes::FindEntity(Identity.OwnerIdentifier);
			if (!Owner || !Owner->g_Alive() || Identity.OwnerGeneration != ExtendedEntityes::OwnerGeneration(Identity.OwnerIdentifier))
			{
				if (Capture->HasEdge)
				{
					CELFlyingGeometry::Cancel(Identity);
				}
				State().Captures.erase(State().Captures.begin() + (State().NextCapture - 1));
				--State().NextCapture;
				Advanced = true;
				continue;
			}
		}
		if (Capture->State != EELGeometryResult::Pending)
		{
			continue;
		}
		if (s32(Device.dwTimeGlobal - Capture->Deadline) >= 0)
		{
			CELFlyingGeometry::Cancel(Capture->Request.Identity);
			Capture->HasEdge = false;
			Capture->State = EELGeometryResult::Blocked;
			Advanced = true;
			continue;
		}
		if (Capture->HasEdge)
		{
			FinishEdge(*Capture);
			Advanced = Advanced || !Capture->HasEdge;
		}
		if (!Capture->HasEdge && Capture->State == EELGeometryResult::Pending)
		{
			if (!StartEdge(*Capture))
			{
				--State().NextCapture;
				break;
			}
			Advanced = true;
		}
	}
	State().FrameMicroseconds += u32(SliceTimer.GetElapsed_mcs());
	return Advanced;
}

// Ограничивает дешёвый обход уже известной карты, не привязывая его к квоте новых лучей.
bool CELSharedFlyingGraph::HasFrameBudget()
{
	return State().FrameMicroseconds < std::max(1u, CELSharedSettings::Work().NavigationSliceMicroseconds / 3);
}

// Возвращает состояние заявки и передаёт готовые данные вызывающему коду.
EELGeometryResult CELSharedFlyingGraph::Take(const SELNavigationIdentity& Identity, SELNavigationRequest& Request)
{
	using namespace ExtendedEntityesAirGraph;
	xr_vector<SCapture>::iterator CaptureIterator;
	EELGeometryResult ResultState;
	for (CaptureIterator = State().Captures.begin(); CaptureIterator != State().Captures.end(); ++CaptureIterator)
	{
		if (CaptureIterator->Request.Identity != Identity)
		{
			continue;
		}
		ResultState = CaptureIterator->State;
		if (ResultState != EELGeometryResult::Pending)
		{
			Request = std::move(CaptureIterator->Request);
			State().Captures.erase(CaptureIterator);
		}
		return ResultState;
	}
	return EELGeometryResult::Blocked;
}

// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELSharedFlyingGraph::Cancel(const SELNavigationIdentity& Identity)
{
	using namespace ExtendedEntityesAirGraph;
	xr_vector<SCapture>::iterator CaptureIterator = State().Captures.begin();
	while (CaptureIterator != State().Captures.end())
	{
		if (CaptureIterator->Request.Identity == Identity)
		{
			if (CaptureIterator->HasEdge)
			{
				CELFlyingGeometry::Cancel(Identity);
			}
			CaptureIterator = State().Captures.erase(CaptureIterator);
		}
		else
		{
			++CaptureIterator;
		}
	}
}
// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedFlyingGraph::Clear()
{
	using namespace ExtendedEntityesAirGraph;
	xr_vector<SCapture>::const_iterator CaptureIterator;
	for (CaptureIterator = State().Captures.begin(); CaptureIterator != State().Captures.end(); ++CaptureIterator)
	{
		if (CaptureIterator->HasEdge)
		{
			CELFlyingGeometry::Cancel(CaptureIterator->Request.Identity);
		}
	}
	State().Captures.clear();
	State().NextCapture = 0;
	State().SliceFrame = u32(-1);
	State().FrameMicroseconds = 0;
	CELSharedFlyingSpaceCache::Clear();
}
// Возвращает число заявок, ожидающих обработки или получения результата.
u32 CELSharedFlyingGraph::OutstandingRequests()
{
	return u32(State().Captures.size());
}
