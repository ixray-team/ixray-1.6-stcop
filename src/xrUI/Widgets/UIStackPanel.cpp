#include "stdafx.h"
#include "UIStackPanel.h"

void CUIStackPanel::Draw()
{
	xrCriticalSectionGuard guard(csUi);
	const bool Ordered = HasCustomChildDrawOrder();

	switch (SPAlignment)
	{
	case eLeft:
		{
			float x = 0.0f;
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown() || W->GetCustomDraw())
				{
					continue;
				}

				W->SetWndPos(Fvector2().set(x, 0.f));
				x += W->GetWndSize().x;
				x += Spacing;

				if (!Ordered)
				{
					W->DrawWithTransform();
				}
			}
		}
		break;
	case eRight:
		{
			float totalWidth = 0.0f;
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W)
				{
					continue;
				}

				totalWidth += W->GetWndSize().x;
			}

			float x = totalWidth;
			SetWidth(totalWidth);

			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown() || W->GetCustomDraw())
				{
					continue;
				}

				W->SetWndPos(Fvector2().set(x, 0.f));
				x -= W->GetWndSize().x;
				x -= Spacing;

				if (!Ordered)
				{
					W->DrawWithTransform();
				}
			}
		}
		break;
	case eTop:
		{
			float y = 0.0f;
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown() || W->GetCustomDraw())
				{
					continue;
				}

				W->SetWndPos(Fvector2().set(0.f, y));
				y += W->GetWndSize().y;
				y += Spacing;

				if (!Ordered)
				{
					W->DrawWithTransform();
				}
			}
		}
		break;
	case eBottom:
		{
			float totalHeight = 0.0f;
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W)
				{
					continue;
				}

				totalHeight += W->GetWndSize().y;
			}

			float y = totalHeight;
			SetHeight(totalHeight);

			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown() || W->GetCustomDraw())
				{
					continue;
				}

				W->SetWndPos(Fvector2().set(0.f, y));
				y -= W->GetWndSize().y;
				y -= Spacing;

				if (!Ordered)
				{
					W->DrawWithTransform();
				}
			}
		}
		break;
	case eCenterHorz:
		{
			float totalWidth = 0.0f;
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown())
				{
					continue;
				}

				totalWidth += W->GetWndSize().x;
				totalWidth += Spacing;
			}

			float x = (GetWidth() / 2.0f) - (totalWidth / 2.0f);
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown() || W->GetCustomDraw())
				{
					continue;
				}

				W->SetWndPos(Fvector2().set(x, 0.f));
				x += W->GetWndSize().x;
				x += Spacing;

				if (!Ordered)
				{
					W->DrawWithTransform();
				}
			}
		}
		break;
	case eCenterVert:
		{
			float totalHeight = 0.0f;
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown())
				{
					continue;
				}

				totalHeight += W->GetWndSize().y;
				totalHeight += Spacing;
			}

			float y = (GetHeight() / 2.0f) - (totalHeight / 2.0f);
			for (CUIWindow* W : m_ChildWndList)
			{
				if (!W || !W->IsShown() || W->GetCustomDraw())
				{
					continue;
				}

				W->SetWndPos(Fvector2().set(0.f, y));
				y += W->GetWndSize().y;
				y += Spacing;

				if (!Ordered)
				{
					W->DrawWithTransform();
				}
			}
		}
		break;
	default:
		R_ASSERT2(false, "not implemented :(");
		break;
	}
	if (Ordered)
	{
		CUIWindow::Draw();
	}
}
