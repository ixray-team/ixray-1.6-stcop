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

void ComputeAnchoredRect(const Frect& parentRect, const SAnchorData& anchor, Frect& result)
{
	if (!anchor.useAnchors)
	{
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

	const float parentWidth = parentWork.width();
	const float parentHeight = parentWork.height();

	result.x1 = parentWork.x1 + anchor.anchorMin.x * parentWidth + anchor.offsetMin.x;
	result.y1 = parentWork.y1 + anchor.anchorMin.y * parentHeight + anchor.offsetMin.y;
	result.x2 = parentWork.x1 + anchor.anchorMax.x * parentWidth + anchor.offsetMax.x;
	result.y2 = parentWork.y1 + anchor.anchorMax.y * parentHeight + anchor.offsetMax.y;

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
