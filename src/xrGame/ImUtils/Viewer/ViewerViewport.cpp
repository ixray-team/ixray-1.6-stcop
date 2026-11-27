#include "StdAfx.h"
#include "ViewerViewport.h"

namespace ImUtils::Viewer::ViewerViewport
{
SCenteredCrop CenteredCoverCrop(float sourceWidth, float sourceHeight, float destinationWidth, float destinationHeight)
{
	const float SourceAspect = std::max(sourceWidth, 1.f) / std::max(sourceHeight, 1.f);
	const float DestinationAspect = std::max(destinationWidth, 1.f) / std::max(destinationHeight, 1.f);
	SCenteredCrop Result = { { 0.f, 0.f }, { 1.f, 1.f } };
	if (SourceAspect > DestinationAspect)
	{
		const float VisibleWidth = DestinationAspect / SourceAspect;
		Result.Min.x = (1.f - VisibleWidth) * 0.5f;
		Result.Max.x = 1.f - Result.Min.x;
	}
	else
	{
		const float VisibleHeight = SourceAspect / DestinationAspect;
		Result.Min.y = (1.f - VisibleHeight) * 0.5f;
		Result.Max.y = 1.f - Result.Min.y;
	}
	return Result;
}

Fvector2 FitToSquare(const Fvector2& point, float sourceWidth, float sourceHeight)
{
	if (sourceWidth <= 0.f || sourceHeight <= 0.f)
	{
		return point;
	}
	const Fvector2 Fit = { std::min(1.f, sourceWidth / sourceHeight), std::min(1.f, sourceHeight / sourceWidth) };
	return { (point.x - 0.5f) * Fit.x + 0.5f, (point.y - 0.5f) * Fit.y + 0.5f };
}

Fvector2 ApplyCrop(const SCenteredCrop& crop, const Fvector2& point)
{
	return { (point.x - crop.Min.x) / (crop.Max.x - crop.Min.x), (point.y - crop.Min.y) / (crop.Max.y - crop.Min.y) };
}
}
