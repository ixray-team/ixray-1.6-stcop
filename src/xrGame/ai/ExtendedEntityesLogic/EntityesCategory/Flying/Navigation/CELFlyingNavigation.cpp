#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingNavigation.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingGraph.h"

// Завершает подготовку и сертификацию воздушного маршрута и освобождает связанные ресурсы.
CELFlyingNavigation::~CELFlyingNavigation()
{
	Cancel();
}
// Задаёт размеры тела для проверки проходимости маршрута.
void CELFlyingNavigation::SetClearance(float CollisionRadius, float BodyCenterHeight)
{
	Radius = CollisionRadius;
	CenterHeight = BodyCenterHeight;
}
// Устанавливает ограничения размера и времени локального поиска.
void CELFlyingNavigation::SetSearch(float CellSize, u32 NodeLimit, u32 Timeout)
{
	SearchCellSize = std::max(CellSize, Radius * 2.f);
	SearchNodeLimit = std::max(NodeLimit, 2u);
	SearchTimeout = Timeout;
}
// Сообщает, ожидается ли ещё подготовка или проверка маршрута.
bool CELFlyingNavigation::IsPending() const
{
	return HasPendingRequest || HasGeometry || HasCapture;
}
// Помещает заявку в очередь с проверкой допустимости и доступного места.
bool CELFlyingNavigation::Submit(SELNavigationRequest Request)
{
	if (IsPending())
	{
		return OriginalRequest.Identity == Request.Identity;
	}
	OriginalRequest = Request;
	HasDeliveredPrefix = false;
	HasTriedDetour = false;
	HasTriedShorterCurve = false;
	HasBlocked = false;
	return CELNavigationBase::Submit(std::move(Request));
}
// Один раз проверяет более короткие плечи той же гладкой кривой до дорогого объёмного поиска.
bool CELFlyingNavigation::RetryShorterCurve()
{
	if (HasTriedShorterCurve || HasTriedDetour || OriginalRequest.CanInvert || (!OriginalRequest.GuidePoints.empty() && OriginalRequest.GuideDirections.empty()) || OriginalRequest.CurveDistance <= EPS_L)
	{
		return false;
	}
	HasTriedShorterCurve = true;
	SELNavigationRequest Request = OriginalRequest;
	Request.CurveDistance *= .5f;
	return CELNavigationBase::Submit(std::move(Request));
}

// Запускает ограниченный трёхмерный обход после отказа прямой кривой.
bool CELFlyingNavigation::StartDetour()
{
	if (HasTriedDetour || OriginalRequest.CanInvert)
	{
		return false;
	}
	HasTriedDetour = true;
	if (!OriginalRequest.GuideDirections.empty())
	{
		IsGuidedDetour = true;
		DetourLeg = 0;
		GuidedNodes = 0;
		GuidedDeadline = Device.dwTimeGlobal + SearchTimeout;
		GuidedPath.clear();
		return SubmitGuidedLeg();
	}
	HasCapture = CELSharedFlyingGraph::Submit(OriginalRequest, Radius, CenterHeight, SearchCellSize, SearchNodeLimit, SearchTimeout);
	return HasCapture;
}
// Строит отдельное плечо обхода, сохраняя обязательный контакт как общий узел двух кривых.
bool CELFlyingNavigation::SubmitGuidedLeg()
{
	if (DetourLeg + 1 >= OriginalRequest.GuidePoints.size() || OriginalRequest.GuideDirections.size() != OriginalRequest.GuidePoints.size())
	{
		return false;
	}
	const s32 TimeLeft = s32(GuidedDeadline - Device.dwTimeGlobal);
	if (TimeLeft <= 0 || GuidedNodes + 2 > SearchNodeLimit)
	{
		return false;
	}
	SELNavigationRequest Leg = OriginalRequest;
	Leg.StartPosition = OriginalRequest.GuidePoints[DetourLeg];
	Leg.GoalPosition = OriginalRequest.GuidePoints[DetourLeg + 1];
	Leg.StartDirection = OriginalRequest.GuideDirections[DetourLeg];
	Leg.GoalDirection = OriginalRequest.GuideDirections[DetourLeg + 1];
	Leg.GuidePoints.clear();
	Leg.GuideDirections.clear();
	Leg.Graph.clear();
	Leg.CanConnectStartToGoal = true;
	HasCapture = CELSharedFlyingGraph::Submit(std::move(Leg), Radius, CenterHeight, SearchCellSize, SearchNodeLimit - GuidedNodes, u32(TimeLeft));
	return HasCapture;
}

// Забирает готовый результат либо продолжает подготовку маршрута.
bool CELFlyingNavigation::Poll(SELNavigationResult& Result)
{
	SELNavigationRequest CapturedRequest;
	if (HasCapture)
	{
		const EELGeometryResult CaptureState = CELSharedFlyingGraph::Take(OriginalRequest.Identity, CapturedRequest);
		if (CaptureState == EELGeometryResult::Pending)
		{
			return false;
		}
		HasCapture = false;
		if (IsGuidedDetour)
		{
			GuidedNodes += u32(CapturedRequest.Graph.size());
		}
		if (CaptureState == EELGeometryResult::Blocked || !CELNavigationBase::Submit(std::move(CapturedRequest)))
		{
			HasBlocked = true;
			return false;
		}
	}
	SELNavigationResult Candidate;
	if (CELNavigationBase::Poll(Candidate))
	{
		if (IsGuidedDetour && Candidate.HasPath)
		{
			if (Candidate.Points.size() < 2 || (!GuidedPath.empty() && !GuidedPath.back().similar(Candidate.Points.front(), EPS_L)))
			{
				HasBlocked = true;
				return false;
			}
			GuidedPath.insert(GuidedPath.end(), Candidate.Points.begin() + (GuidedPath.empty() ? 0 : 1), Candidate.Points.end());
			++DetourLeg;
			if (DetourLeg + 1 < OriginalRequest.GuidePoints.size())
			{
				HasBlocked = !SubmitGuidedLeg();
				return false;
			}
			Candidate.Points = std::move(GuidedPath);
			IsGuidedDetour = false;
		}
		GeometryIdentity = Candidate.Identity;
		HasGeometry = Candidate.HasPath && CELFlyingGeometry::Submit(std::move(Candidate), Radius, CenterHeight, StreamsRoute);
		if (!HasGeometry)
		{
			HasBlocked = !(RetryShorterCurve() || StartDetour());
		}
	}
	if (HasGeometry)
	{
		Result = SELNavigationResult();
		const EELGeometryResult State = CELFlyingGeometry::Take(GeometryIdentity, Result);
		if (State == EELGeometryResult::Pending && Result.HasPath)
		{
			HasDeliveredPrefix = true;
			return true;
		}
		if (State != EELGeometryResult::Pending)
		{
			HasGeometry = false;
			HasBlocked = State == EELGeometryResult::Blocked && (HasDeliveredPrefix || !(RetryShorterCurve() || StartDetour()));
			return State == EELGeometryResult::Clear;
		}
	}
	return false;
}
// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELFlyingNavigation::Cancel()
{
	CELNavigationBase::Cancel();
	if (HasGeometry)
	{
		CELFlyingGeometry::Cancel(GeometryIdentity);
	}
	if (HasCapture)
	{
		CELSharedFlyingGraph::Cancel(OriginalRequest.Identity);
	}
	HasGeometry = false;
	HasDeliveredPrefix = false;
	HasCapture = false;
	IsGuidedDetour = false;
	DetourLeg = 0;
	GuidedPath.clear();
	HasBlocked = false;
	HasTriedDetour = false;
	HasTriedShorterCurve = false;
	OriginalRequest = SELNavigationRequest();
}

// Возвращает краткое объяснение текущего этапа подготовки маршрута.
const char* CELFlyingNavigation::Status() const
{
	if (HasCapture)
	{
		return "Поиск объёмного воздушного коридора";
	}
	if (HasGeometry)
	{
		return "Проверка прохода для тела";
	}
	if (HasPendingRequest)
	{
		if (HasTriedShorterCurve && !HasTriedDetour)
		{
			return "Расчёт более короткого виража";
		}
		return "Ожидание расчёта полёта";
	}
	return HasBlocked ? "Нет проверенного воздушного коридора" : "Запрос полёта не поставлен в очередь";
}
