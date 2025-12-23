#include "stdafx.h"
#include "UIStackPanel.h"

void CUIStackPanel::Draw()
{
	xrCriticalSectionGuard guard(csUi);

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

				W->Draw();
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

				W->Draw();
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

				W->Draw();
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

				W->Draw();
			}
		}
		break;
	default:
		R_ASSERT2(false, "not implemented :(");
		break;
	}
}
