#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingRouteSmoothing.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/entity_alive.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"

// Помещает заявку в очередь с проверкой допустимости и доступного места.
bool CELSharedNavigationScheduler::Submit(SELNavigationRequest Request)
{
	xrCriticalSectionGuard Guard(QueueLock);
	if (Request.Identity.WorldGeneration != WorldGeneration || Tasks.size() >= CELSharedSettings::Work().NavigationTaskLimit || Request.Graph.size() > CELSharedSettings::Work().NavigationGraphNodeLimit || !_valid(Request.StartPosition) || !_valid(Request.GoalPosition) || !_valid(Request.StartDirection) || !_valid(Request.GoalDirection) || !_valid(Request.CurveDistance) || Request.CurveDistance < 0.f || !_valid(Request.InvertedBodyRadius) || Request.InvertedBodyRadius < 0.f)
	{
		return false;
	}
	xr_vector<STask>::const_iterator TaskIterator;
	for (TaskIterator = Tasks.begin(); TaskIterator != Tasks.end(); ++TaskIterator)
	{
		if (TaskIterator->Request.Identity == Request.Identity)
		{
			return true;
		}
	}
	if (!Request.GuideDirections.empty())
	{
		if (Request.GuideDirections.size() != Request.GuidePoints.size() || !Request.Graph.empty())
		{
			return false;
		}
		xr_vector<Fvector>::iterator Direction;
		for (Direction = Request.GuideDirections.begin(); Direction != Request.GuideDirections.end(); ++Direction)
		{
			if (!_valid(*Direction) || Direction->square_magnitude() <= EPS_L)
			{
				return false;
			}
			Direction->normalize_safe();
		}
	}
	STask Task;
	Task.Result.Identity = Request.Identity;
	Task.Result.InvertedBodyRadius = Request.InvertedBodyRadius;
	Task.Result.CanInvert = Request.CanInvert;
	if (!Request.GuidePoints.empty())
	{
		if (!Request.IsFlying || Request.GuidePoints.size() < 2 || Request.GuidePoints.size() > CELSharedSettings::Work().NavigationGraphNodeLimit || !Request.GuidePoints.front().similar(Request.StartPosition) || !Request.GuidePoints.back().similar(Request.GoalPosition))
		{
			return false;
		}
		xr_vector<Fvector>::const_iterator GuidePoint;
		for (GuidePoint = Request.GuidePoints.begin(); GuidePoint != Request.GuidePoints.end(); ++GuidePoint)
		{
			if (!_valid(*GuidePoint))
			{
				return false;
			}
		}
		if (Request.Graph.empty())
		{
			Task.CurvePath = Request.GuidePoints;
		}
	}
	Task.Request = std::move(Request);
	if (!Task.Request.IsFlying || !Task.Request.Graph.empty())
	{
		if (Task.Request.StartNode >= Task.Request.Graph.size() || Task.Request.GoalNode >= Task.Request.Graph.size())
		{
			return false;
		}
		Task.Costs.assign(Task.Request.Graph.size(), FLT_MAX);
		Task.Parents.assign(Task.Request.Graph.size(), u32(-1));
		Task.States.assign(Task.Request.Graph.size(), 0);
		Task.Costs[Task.Request.StartNode] = 0.f;
		Task.States[Task.Request.StartNode] = 1;
		Task.OpenNodes.push_back({Task.Request.StartNode, 0.f, Task.Request.Graph[Task.Request.StartNode].Position.distance_to(Task.Request.Graph[Task.Request.GoalNode].Position)});
	}
	Tasks.push_back(std::move(Task));
	return true;
}

// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELSharedNavigationScheduler::Cancel(const SELNavigationIdentity& Identity)
{
	xrCriticalSectionGuard Guard(QueueLock);
	xr_vector<STask>::iterator TaskIterator = Tasks.begin();
	while (TaskIterator != Tasks.end())
	{
		if (TaskIterator->Request.Identity == Identity)
		{
			TaskIterator = Tasks.erase(TaskIterator);
		}
		else
		{
			++TaskIterator;
		}
	}
}

// Возвращает состояние заявки и передаёт готовые данные вызывающему коду.
bool CELSharedNavigationScheduler::Take(const SELNavigationIdentity& Identity, SELNavigationResult& Result)
{
	xrCriticalSectionGuard Guard(QueueLock);
	xr_vector<STask>::iterator TaskIterator;
	for (TaskIterator = Tasks.begin(); TaskIterator != Tasks.end(); ++TaskIterator)
	{
		if (TaskIterator->Request.Identity == Identity && TaskIterator->IsComplete)
		{
			Result = std::move(TaskIterator->Result);
			Tasks.erase(TaskIterator);
			return true;
		}
	}
	return false;
}

// Выполняет ограниченную порцию фоновых расчётов только по собственным снимкам.
void CELSharedNavigationScheduler::Process()
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Shared, EELDebugStage::Worker);
	// One bounded delegate on the existing game thread. Inputs are owned snapshots.
	xrCriticalSectionGuard Guard(QueueLock);
	Budget.BeginFrame(Device.dwFrame);
	u32 VisitedWithoutWork = 0;
	STask* Task;
	CTimer SliceTimer;
	SliceTimer.Start();
	while (!Tasks.empty() && VisitedWithoutWork < Tasks.size() && SliceTimer.GetElapsed_mcs() < CELSharedSettings::Work().NavigationSliceMicroseconds)
	{
		NextTask %= u32(Tasks.size());
		Task = &Tasks[NextTask];
		++NextTask;
		if (Task->IsComplete)
		{
			++VisitedWithoutWork;
			continue;
		}
		VisitedWithoutWork = 0;
		if (!Budget.TryConsume())
		{
			--NextTask;
			break;
		}
		Expand(*Task);
	}
	SliceMicroseconds = u32(SliceTimer.GetElapsed_mcs());
}

// Выполняет один ограниченный шаг расширения поисковых данных.
void CELSharedNavigationScheduler::Expand(STask& Task)
{
	CELSharedDebugScope ProfileScope(Task.Request.IsFlying ? EELDebugGroup::Flying : EELDebugGroup::Ground, EELDebugStage::Worker);
	if (Task.Request.IsFlying && (Task.Request.Graph.empty() || !Task.CurvePath.empty()))
	{
		ExpandCurve(Task);
	}
	else
	{
		ExpandGraph(Task);
	}
}

// Порционно рассчитывает точки сглаженной кривой по собственному снимку.
void CELSharedNavigationScheduler::ExpandCurve(STask& Task)
{
	const SELNavigationRequest& Request = Task.Request;

	// Шесть шагов сохраняют обязательные стыки, не создавая шестнадцать хорд на каждом метре обхода.
	constexpr u32 SamplesPerSegment = 6;
	const u32 Segment = Task.CurveStep / SamplesPerSegment;
	const u32 Sample = Task.CurveStep % SamplesPerSegment;
	const bool HasGraphPath = !Task.CurvePath.empty();
	const u32 SegmentCount = HasGraphPath ? u32(Task.CurvePath.size() - 1) : 1;
	const bool IsLastPoint = Segment >= SegmentCount;
	const u32 CurrentSegment = IsLastPoint ? SegmentCount - 1 : Segment;
	const float Fraction = IsLastPoint ? 1.f : float(Sample) / float(SamplesPerSegment);
	const float Remaining = 1.f - Fraction;
	const Fvector Start = HasGraphPath ? Task.CurvePath[CurrentSegment] : Request.StartPosition;
	const Fvector Goal = HasGraphPath ? Task.CurvePath[CurrentSegment + 1] : Request.GoalPosition;
	Fvector StartDirection = Request.StartDirection;
	Fvector GoalDirection = Request.GoalDirection;
	float CurveDistance = Request.CurveDistance;
	if (HasGraphPath)
	{
		if (CurrentSegment > 0)
		{
			StartDirection.sub(Goal, Task.CurvePath[CurrentSegment - 1]).normalize_safe();
		}
		if (CurrentSegment + 2 < Task.CurvePath.size())
		{
			GoalDirection.sub(Task.CurvePath[CurrentSegment + 2], Start).normalize_safe();
		}
		if (Request.GuideDirections.empty())
		{
			CurveDistance = std::min(CurveDistance, Start.distance_to(Goal) * .25f);
		}
	}
	if (!Request.GuideDirections.empty())
	{
		StartDirection = Request.GuideDirections[CurrentSegment];
		GoalDirection = Request.GuideDirections[CurrentSegment + 1];
	}
	Fvector Displacement;
	Displacement.sub(Goal, Start);
	CurveDistance = std::min(CurveDistance, Displacement.magnitude() * .5f);
	const float StartProjection = Displacement.dotproduct(StartDirection);
	const float EndProjection = Displacement.dotproduct(GoalDirection);
	if (StartProjection > EPS_L && EndProjection > EPS_L)
	{
		// Keep control points ordered along the chord instead of folding short/climbing legs backwards.
		CurveDistance = std::min(CurveDistance, std::min(StartProjection, EndProjection) * .5f);
	}
	Fvector FirstControl;
	Fvector SecondControl;
	Fvector Point;
	FirstControl.mad(Start, StartDirection, CurveDistance);
	SecondControl.mad(Goal, GoalDirection, -CurveDistance);
	Point.mul(Start, Remaining * Remaining * Remaining);
	Point.mad(FirstControl, 3.f * Remaining * Remaining * Fraction);
	Point.mad(SecondControl, 3.f * Remaining * Fraction * Fraction);
	Point.mad(Goal, Fraction * Fraction * Fraction);
	if (!Task.Result.Points.empty() && CELSharedAnomalies::Crosses(Request.Obstacles, Task.Result.Points.back(), Point))
	{
		Task.IsComplete = true;
		return;
	}
	Task.Result.Points.push_back(Point);
	++Task.CurveStep;
	if (IsLastPoint)
	{
		Task.Result.HasPath = true;
		Task.IsComplete = true;
	}
}

// Выполняет очередное расширение A* по собственному снимку графа.
void CELSharedNavigationScheduler::ExpandGraph(STask& Task)
{
	if (Task.OpenNodes.empty())
	{
		Task.IsComplete = true;
		return;
	}
	// Consume at most one heap entry per budget operation, including stale entries.
	std::pop_heap(Task.OpenNodes.begin(), Task.OpenNodes.end());
	const SOpenNode Current = Task.OpenNodes.back();
	Task.OpenNodes.pop_back();
	const u32 BestNode = Current.Index;
	if (Task.States[BestNode] == 2 || Current.Cost > Task.Costs[BestNode])
	{
		return;
	}
	if (BestNode == Task.Request.GoalNode)
	{
		FinishGraph(Task, BestNode);
		return;
	}
	Task.States[BestNode] = 2;
	u32 NeighborIndex;
	u32 NeighborNode;
	float NewCost;
	for (NeighborIndex = 0; NeighborIndex < std::size(Task.Request.Graph[BestNode].Neighbors); ++NeighborIndex)
	{
		NeighborNode = Task.Request.Graph[BestNode].Neighbors[NeighborIndex];
		if (NeighborNode >= Task.Request.Graph.size() || Task.States[NeighborNode] == 2)
		{
			continue;
		}
		if (CELSharedAnomalies::Crosses(Task.Request.Obstacles, Task.Request.Graph[BestNode].Position, Task.Request.Graph[NeighborNode].Position))
		{
			continue;
		}
		NewCost = Task.Costs[BestNode] + Task.Request.Graph[BestNode].Position.distance_to(Task.Request.Graph[NeighborNode].Position);
		if (NewCost < Task.Costs[NeighborNode])
		{
			Task.Costs[NeighborNode] = NewCost;
			Task.Parents[NeighborNode] = BestNode;
			Task.States[NeighborNode] = 1;
			Task.OpenNodes.push_back({NeighborNode, NewCost, NewCost + Task.Request.Graph[NeighborNode].Position.distance_to(Task.Request.Graph[Task.Request.GoalNode].Position)});
			std::push_heap(Task.OpenNodes.begin(), Task.OpenNodes.end());
		}
	}
}

// Восстанавливает найденный путь и подготавливает его порционное сглаживание.
void CELSharedNavigationScheduler::FinishGraph(STask& Task, u32 NodeIndex)
{
	while (NodeIndex != u32(-1))
	{
		Task.Result.Points.push_back(Task.Request.Graph[NodeIndex].Position);
		Task.Result.Vertices.push_back(Task.Request.Graph[NodeIndex].VertexIdentifier);
		NodeIndex = Task.Parents[NodeIndex];
	}
	std::reverse(Task.Result.Points.begin(), Task.Result.Points.end());
	std::reverse(Task.Result.Vertices.begin(), Task.Result.Vertices.end());
	if (Task.Request.IsFlying && Task.Result.Points.size() > 1)
	{
		CELFlyingRouteSmoothing::Compact(Task.Request, Task.Result.Points);
		Task.CurvePath = std::move(Task.Result.Points);
		Task.Result.Points.clear();
		Task.Result.Vertices.clear();
		Task.CurveStep = 0;
		return;
	}
	Task.Result.HasPath = true;
	Task.IsComplete = true;
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedNavigationScheduler::Clear()
{
	xrCriticalSectionGuard Guard(QueueLock);
	Tasks.clear();
	++WorldGeneration;
	NextTask = 0;
}

// Main/game phase only: worker tasks never inspect live world objects themselves.
void CELSharedNavigationScheduler::DiscardAbandoned()
{
	xrCriticalSectionGuard Guard(QueueLock);
	for (auto Task = Tasks.begin(); Task != Tasks.end();)
	{
		const SELNavigationIdentity& Identity = Task->Request.Identity;
		const CEntityAlive* Owner = ExtendedEntityes::FindEntity(Identity.OwnerIdentifier);
		if (!Owner || !Owner->g_Alive() || Identity.WorldGeneration != WorldGeneration || Identity.OwnerGeneration != ExtendedEntityes::OwnerGeneration(Identity.OwnerIdentifier))
		{
			Task = Tasks.erase(Task);
		}
		else
		{
			++Task;
		}
	}
	if (Tasks.empty())
	{
		NextTask = 0;
	}
	else
	{
		NextTask %= u32(Tasks.size());
	}
}

// Возвращает поколение мира для отклонения устаревших заявок.
u32 CELSharedNavigationScheduler::GetGeneration() const
{
	xrCriticalSectionGuard Guard(QueueLock);
	return WorldGeneration;
}

// Резервирует квоту захвата одного узла штатного графа.
bool CELSharedNavigationScheduler::TryCaptureNode()
{
	xrCriticalSectionGuard Guard(QueueLock);
	Budget.BeginFrame(Device.dwFrame);
	return Budget.TryCaptureNode();
}

// Атомарно резервирует необходимое число лучей проверки геометрии.
bool CELSharedNavigationScheduler::TryGeometryRays(u32 RayCount)
{
	xrCriticalSectionGuard Guard(QueueLock);
	Budget.BeginFrame(Device.dwFrame);
	return Budget.TryGeometryRays(RayCount);
}

bool CELSharedNavigationScheduler::TryEmergencyGeometryRays(u32 RayCount)
{
	xrCriticalSectionGuard Guard(QueueLock);
	Budget.BeginFrame(Device.dwFrame);
	return Budget.TryEmergencyGeometryRays(RayCount);
}

// Возвращает число заявок, ожидающих обработки или получения результата.
u32 CELSharedNavigationScheduler::OutstandingRequests() const
{
	xrCriticalSectionGuard Guard(QueueLock);
	return u32(Tasks.size());
}

// Возвращает длительность последней фоновой порции в микросекундах.
u32 CELSharedNavigationScheduler::LastSliceMicroseconds() const
{
	xrCriticalSectionGuard Guard(QueueLock);
	return SliceMicroseconds;
}

// Возвращает остаток уже начатой кадровой квоты для диагностики ожидания полёта.
u32 CELSharedNavigationScheduler::GeometryRaysRemaining() const
{
	xrCriticalSectionGuard Guard(QueueLock);
	return Budget.GeometryRaysRemaining();
}

u32 CELSharedNavigationScheduler::EmergencyRaysRemaining() const
{
	xrCriticalSectionGuard Guard(QueueLock);
	return Budget.EmergencyRaysRemaining();
}
