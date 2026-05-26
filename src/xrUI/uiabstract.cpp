#include "stdafx.h"
#include "uiabstract.h"

void CUISimpleWindow::ResolveAutoSize()
{
	const bool widthAuto = (m_sizeModeWidth == UI_SIZE_MODE_AUTO);
	const bool heightAuto = (m_sizeModeHeight == UI_SIZE_MODE_AUTO);
	if (!widthAuto && !heightAuto)
	{
		return;
	}

	Fvector2 preferred = GetPreferredSize();
	float w = widthAuto ? preferred.x : m_wndSize.x;
	float h = heightAuto ? preferred.y : m_wndSize.y;
	w = (w < m_minWidth) ? m_minWidth : w;
	h = (h < m_minHeight) ? m_minHeight : h;
	SetWndSize(Fvector2().set(w, h));
}

namespace
{
float ClampAnchorNormalizedCoord(float value)
{
	if (value < 0.0f)
	{
		return 0.0f;
	}
	if (value > 1.0f)
	{
		return 1.0f;
	}
	return value;
}

void LogInvalidAnchorCoordsOnce()
{
	static bool hasLogged = false;
	if (hasLogged)
	{
		return;
	}
	hasLogged = true;
	Msg("! UI anchor: anchor_min/anchor_max values were clamped to [0, 1]");
}
} // namespace

void SyncAnchorOffsetsFromSize(SAnchorData& anchorData, float width, float height)
{
	if (!anchorData.useAnchors)
	{
		return;
	}

	const bool stretchH = (anchorData.anchorMin.x != anchorData.anchorMax.x);
	const bool stretchV = (anchorData.anchorMin.y != anchorData.anchorMax.y);

	if (stretchH && stretchV)
	{
		return;
	}

	if (!stretchH && !stretchV)
	{
		anchorData.offsetMax.x = anchorData.offsetMin.x + width;
		anchorData.offsetMax.y = anchorData.offsetMin.y + height;
	}
	else if (stretchH)
	{
		const float halfH = height * 0.5f;
		anchorData.offsetMin.y = -halfH;
		anchorData.offsetMax.y = halfH;
	}
	else
	{
		const float halfW = width * 0.5f;
		anchorData.offsetMin.x = -halfW;
		anchorData.offsetMax.x = halfW;
	}
}

void ComputeAnchoredRect(const Frect& parentRect, const SAnchorData& anchor, Frect& result)
{
	if (!anchor.useAnchors)
	{
		result.set(parentRect.x1, parentRect.y1, parentRect.x2, parentRect.y2);
		return;
	}

	Frect parentWork = parentRect;
	if (parentWork.x2 < parentWork.x1)
	{
		const float tmp = parentWork.x1;
		parentWork.x1 = parentWork.x2;
		parentWork.x2 = tmp;
	}
	if (parentWork.y2 < parentWork.y1)
	{
		const float tmp = parentWork.y1;
		parentWork.y1 = parentWork.y2;
		parentWork.y2 = tmp;
	}

	if (!_valid(parentWork) || !_valid(anchor.anchorMin) || !_valid(anchor.anchorMax) || !_valid(anchor.offsetMin) || !_valid(anchor.offsetMax))
	{
		result.set(parentWork.x1, parentWork.y1, parentWork.x2, parentWork.y2);
		return;
	}

	Fvector2 anchorMin = anchor.anchorMin;
	Fvector2 anchorMax = anchor.anchorMax;
	const bool needsClamp =
		anchorMin.x < 0.0f || anchorMin.x > 1.0f ||
		anchorMin.y < 0.0f || anchorMin.y > 1.0f ||
		anchorMax.x < 0.0f || anchorMax.x > 1.0f ||
		anchorMax.y < 0.0f || anchorMax.y > 1.0f;
	if (needsClamp)
	{
		LogInvalidAnchorCoordsOnce();
		anchorMin.x = ClampAnchorNormalizedCoord(anchorMin.x);
		anchorMin.y = ClampAnchorNormalizedCoord(anchorMin.y);
		anchorMax.x = ClampAnchorNormalizedCoord(anchorMax.x);
		anchorMax.y = ClampAnchorNormalizedCoord(anchorMax.y);
	}

	const float parentWidth = parentWork.width();
	const float parentHeight = parentWork.height();

	result.x1 = parentWork.x1 + anchorMin.x * parentWidth + anchor.offsetMin.x;
	result.y1 = parentWork.y1 + anchorMin.y * parentHeight + anchor.offsetMin.y;
	result.x2 = parentWork.x1 + anchorMax.x * parentWidth + anchor.offsetMax.x;
	result.y2 = parentWork.y1 + anchorMax.y * parentHeight + anchor.offsetMax.y;

	if (!_valid(result))
	{
		result.set(parentWork.x1, parentWork.y1, parentWork.x2, parentWork.y2);
		return;
	}

	if (result.x2 < result.x1)
	{
		const float tmp = result.x1;
		result.x1 = result.x2;
		result.x2 = tmp;
	}
	if (result.y2 < result.y1)
	{
		const float tmp = result.y1;
		result.y1 = result.y2;
		result.y2 = tmp;
	}
}
