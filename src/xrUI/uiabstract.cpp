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

	const float parentWidth = parentRect.width();
	const float parentHeight = parentRect.height();

	result.x1 = parentRect.x1 + anchor.anchorMin.x * parentWidth + anchor.offsetMin.x;
	result.y1 = parentRect.y1 + anchor.anchorMin.y * parentHeight + anchor.offsetMin.y;
	result.x2 = parentRect.x1 + anchor.anchorMax.x * parentWidth + anchor.offsetMax.x;
	result.y2 = parentRect.y1 + anchor.anchorMax.y * parentHeight + anchor.offsetMax.y;

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
