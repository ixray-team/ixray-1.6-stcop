#include "stdafx.h"
#include "UIStackPanel.h"

void CUIStackPanel::Draw()
{
	xrCriticalSectionGuard guard(csUi);

	if (AlignLeft)
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

			W->Draw();
		}
	}
	else
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

			W->Draw();
		}
	}
}
