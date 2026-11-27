#pragma once

namespace ImUtils::Viewer::ViewerViewport
{
	struct SCenteredCrop
	{
		Fvector2 Min;
		Fvector2 Max;
	};

	SCenteredCrop CenteredCoverCrop(float sourceWidth, float sourceHeight, float destinationWidth, float destinationHeight);
	Fvector2 FitToSquare(const Fvector2& point, float sourceWidth, float sourceHeight);
	Fvector2 ApplyCrop(const SCenteredCrop& crop, const Fvector2& point);
}
