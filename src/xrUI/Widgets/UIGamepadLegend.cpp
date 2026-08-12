#include "stdafx.h"
#include "UIGamepadLegend.h"
#include "UIStatic.h"
#include "../../xrEngine/xr_input.h"

void CUIGamepadLegend::Draw()
{
	if (!pInput->GetControllerMode())
		return;

	xrCriticalSectionGuard guard(csUi);

	float totalWidth = 0.0f;
	for (CUIWindow* W : m_ChildWndList)
	{
		if (!W || !W->IsShown())
		{
			continue;
		}
		if (W->ui_cast_static())
		{
			W->ui_cast_static()->AdjustWidthToText();
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
		if (W->ui_cast_static())
		{
			W->ui_cast_static()->SetTextShadow(true, 0.5f, color_argb(255, 0, 0, 0));
		}
		W->SetWndPos(Fvector2().set(x, 0.f));
		x += W->GetWndSize().x;
		x += Spacing;
		W->Draw();
	}
}

void CUIGamepadLegend::ReloadLegend() 
{
	for (CUIWindow* W : m_ChildWndList)
	{
		if (!W || !W->ui_cast_static())
		{
			continue;
		}

		W->ui_cast_static()->ReloadText();
	}
}