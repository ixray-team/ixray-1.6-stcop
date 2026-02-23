#include "stdafx.h"
#include "UILayout.h"
#include "Widgets/UIWindow.h"

CUIStackLayout::CUIStackLayout(EUIStackLayoutDir direction, float spacing,
	float paddingLeft, float paddingTop, float paddingRight, float paddingBottom,
	bool reverse)
	: m_direction(direction)
	, m_spacing(spacing)
	, m_paddingLeft(paddingLeft)
	, m_paddingTop(paddingTop)
	, m_paddingRight(paddingRight)
	, m_paddingBottom(paddingBottom)
	, m_reverse(reverse)
{
}

void CUIStackLayout::SetPadding(float left, float top, float right, float bottom)
{
	m_paddingLeft = left;
	m_paddingTop = top;
	m_paddingRight = right;
	m_paddingBottom = bottom;
}

void CUIStackLayout::LayoutChildren(CUIWindow* pParent)
{
	if (!pParent)
	{
		return;
	}

	auto& children = pParent->GetChildWndList();
	if (children.empty())
	{
		return;
	}

	for (CUIWindow* child : children)
	{
		if (child && child->IsShown() && !child->GetCustomDraw())
		{
			child->ResolveAutoSize();
		}
	}

	if (m_direction == EUIStackLayoutDir::Horizontal)
	{
		float x = m_paddingLeft;

		if (m_reverse)
		{
			float parentWidth = pParent->GetWidth();
			x = parentWidth - m_paddingRight;

			for (int i = (int)children.size() - 1; i >= 0; --i)
			{
				CUIWindow* child = children[i];
				if (!child || !child->IsShown() || child->GetCustomDraw())
				{
					continue;
				}
				float w = child->GetWidth();
				x -= w;
				child->SetWndPos(Fvector2().set(x, m_paddingTop));
				x -= m_spacing;
			}
		}
		else
		{
			for (CUIWindow* child : children)
			{
				if (!child || !child->IsShown() || child->GetCustomDraw())
				{
					continue;
				}
				child->SetWndPos(Fvector2().set(x, m_paddingTop));
				x += child->GetWidth() + m_spacing;
			}
		}
	}
	else
	{
		float y = m_paddingTop;

		if (m_reverse)
		{
			float parentHeight = pParent->GetHeight();
			y = parentHeight - m_paddingBottom;

			for (int i = (int)children.size() - 1; i >= 0; --i)
			{
				CUIWindow* child = children[i];
				if (!child || !child->IsShown() || child->GetCustomDraw())
				{
					continue;
				}
				float h = child->GetHeight();
				y -= h;
				child->SetWndPos(Fvector2().set(m_paddingLeft, y));
				y -= m_spacing;
			}
		}
		else
		{
			for (CUIWindow* child : children)
			{
				if (!child || !child->IsShown() || child->GetCustomDraw())
				{
					continue;
				}
				child->SetWndPos(Fvector2().set(m_paddingLeft, y));
				y += child->GetHeight() + m_spacing;
			}
		}
	}
}

CUIGridLayout::CUIGridLayout(int cols, int rows,
	float cellSpacingX, float cellSpacingY,
	float cellWidth, float cellHeight,
	float paddingLeft, float paddingTop, float paddingRight, float paddingBottom)
	: m_cols(cols)
	, m_rows(rows)
	, m_cellSpacingX(cellSpacingX)
	, m_cellSpacingY(cellSpacingY)
	, m_cellWidth(cellWidth)
	, m_cellHeight(cellHeight)
	, m_paddingLeft(paddingLeft)
	, m_paddingTop(paddingTop)
	, m_paddingRight(paddingRight)
	, m_paddingBottom(paddingBottom)
{
}

void CUIGridLayout::LayoutChildren(CUIWindow* pParent)
{
	if (!pParent || m_cols <= 0)
	{
		return;
	}

	auto& children = pParent->GetChildWndList();
	if (children.empty())
	{
		return;
	}

	for (CUIWindow* child : children)
	{
		if (child && child->IsShown() && !child->GetCustomDraw())
		{
			child->ResolveAutoSize();
		}
	}

	int childIndex = 0;
	for (CUIWindow* child : children)
	{
		if (!child || !child->IsShown() || child->GetCustomDraw())
		{
			continue;
		}

		int row = childIndex / m_cols;
		int col = childIndex % m_cols;

		if (m_rows > 0 && row >= m_rows)
		{
			break;
		}

		float cellW = m_cellWidth > 0.0f ? m_cellWidth : child->GetWidth();
		float cellH = m_cellHeight > 0.0f ? m_cellHeight : child->GetHeight();

		float x = m_paddingLeft + col * (cellW + m_cellSpacingX);
		float y = m_paddingTop + row * (cellH + m_cellSpacingY);

		child->SetWndPos(Fvector2().set(x, y));

		++childIndex;
	}
}
