#include "stdafx.h"

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
		ImGui::SetNextWindowBgAlpha(0.33f);
	}
}

void XrUI::EndDraw() const
{
	ImGui::SetNextWindowBgAlpha(1);
}
