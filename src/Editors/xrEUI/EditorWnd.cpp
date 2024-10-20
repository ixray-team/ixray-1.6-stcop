#include "stdafx.h"
#include "xrUITheme.h"

IEditorWnd::~IEditorWnd()
{
}

void IEditorWnd::ResetBegin()
{

}

void IEditorWnd::ResetEnd()
{

}

void IEditorWnd::BeginDraw() const
{
	if (!IsFocused && !IsDocked && !IsContextMenu)
	{
		ImGui::SetNextWindowBgAlpha(CUIThemeManager::Get().TransparentUnfocused);
	}
}

void IEditorWnd::EndDraw() const
{
	ImGui::SetNextWindowBgAlpha(CUIThemeManager::Get().TransparentDefault);
}
