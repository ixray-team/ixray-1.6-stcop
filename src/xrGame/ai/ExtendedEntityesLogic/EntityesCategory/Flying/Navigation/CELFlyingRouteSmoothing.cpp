#include "stdafx.h"
#include <algorithm>
#include "CELFlyingRouteSmoothing.h"

// Сохраняет повороты грубого пути и ограничивает плечо сглаживания до удаления лишних прямых узлов.
void CELFlyingRouteSmoothing::Compact(SELNavigationRequest& Request, xr_vector<Fvector>& Points)
{
	if (Points.size() < 3 || !Request.GuideDirections.empty())
	{
		return;
	}
	for (u32 Index = 1; Index < Points.size(); ++Index)
	{
		const float Distance = Points[Index - 1].distance_to(Points[Index]);
		if (Distance > EPS_L)
		{
			Request.CurveDistance = std::min(Request.CurveDistance, Distance * .25f);
		}
	}
	u32 Written = 1;
	for (u32 Index = 1; Index + 1 < Points.size(); ++Index)
	{
		const Fvector Incoming = Fvector().sub(Points[Index], Points[Written - 1]);
		const Fvector Outgoing = Fvector().sub(Points[Index + 1], Points[Index]);
		// Не удаляем вершину разворота или изгиба и не соединяем произвольные видимые точки сквозь стену.
		if (Incoming.dotproduct(Outgoing) < 0.f || Fvector().crossproduct(Incoming, Outgoing).square_magnitude() > EPS_S * EPS_S)
		{
			Points[Written++] = Points[Index];
		}
	}
	Points[Written++] = Points.back();
	Points.resize(Written);
}
