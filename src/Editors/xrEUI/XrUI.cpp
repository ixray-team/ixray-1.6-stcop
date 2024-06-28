#include "stdafx.h"
#include "xrUITheme.h"

XrUI::~XrUI()
{
}

void XrUI::ResetBegin()
{

}

void XrUI::ResetEnd()
{

}

void XrUI::BeginDraw() const
{
	if (!IsFocused && !IsDocked)
	{
		ImGui::SetNextWindowBgAlpha(CUIThemeManager::Get().TransparentUnfocused);
	}
}

void XrUI::EndDraw() const
{
	ImGui::SetNextWindowBgAlpha(CUIThemeManager::Get().TransparentDefault);
}
