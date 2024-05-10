#include "stdafx.h"
#include "UIToolbar.h"

void CUIToolbar::CheckAction(ETAction Action) 
{
	auto ActiveAction = Tools->GetAction();

	if (Action == ActiveAction)
	{
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
		IsActiveAction = true;
	}
}

void CUIToolbar::CheckAxis(ETAxis Axis)
{
	auto ActiveAxis = Tools->GetAxis();

	if (Axis == ActiveAxis)
	{
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
		IsActiveAction = true;
	}
}

void CUIToolbar::EndCheck()
{
	if (IsActiveAction || IsActiveAxis)
	{
		ImGui::PopStyleColor();
		IsActiveAction = false;
		IsActiveAxis = false;
	}
}

void CUIToolbar::OnCreate()
{
	Select = EDevice->Resources->_CreateTexture("ed\\bar\\select");
	Move = EDevice->Resources->_CreateTexture("ed\\bar\\move");
	Rotate = EDevice->Resources->_CreateTexture("ed\\bar\\rotate");
	Scale = EDevice->Resources->_CreateTexture("ed\\bar\\scale");
}

void CUIToolbar::Draw()
{
	if (!bFocus)
	{
		ImGui::SetNextWindowBgAlpha(0.4f);
	}

	if (ImGui::Begin("##ToolbarHor", nullptr, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoScrollbar))
	{
		bFocus = ImGui::IsWindowFocused();
		constexpr ImVec2 SizeImg = { 14, 17 };
		constexpr ImVec2 SizeBtn = { 24, 24 };

		CheckAction(etaSelect);
		if (ImGui::ImageButton(Select->pSurface, SizeImg))
		{
			Tools->SetAction(etaSelect);
		}
		EndCheck();

		CheckAction(etaMove);
		if (ImGui::ImageButton(Move->pSurface, SizeImg))
		{
			Tools->SetAction(etaMove);
		}
		EndCheck();

		CheckAction(etaRotate);
		if (ImGui::ImageButton(Rotate->pSurface, SizeImg))
		{
			Tools->SetAction(etaRotate);
		}
		EndCheck();

		CheckAction(etaScale);
		if (ImGui::ImageButton(Scale->pSurface, SizeImg))
		{
			Tools->SetAction(etaScale);
		}
		EndCheck();
		ImGui::Separator();

		CheckAxis(etAxisX);
		if (ImGui::Button("X", SizeBtn))
		{
			Tools->SetAxis(etAxisX);
		}
		EndCheck();

		CheckAxis(etAxisY);
		if (ImGui::Button("Y", SizeBtn))
		{
			Tools->SetAxis(etAxisY);
		}
		EndCheck();

		CheckAxis(etAxisZ);
		if (ImGui::Button("Z", SizeBtn))
		{
			Tools->SetAxis(etAxisZ);
		}
		EndCheck();

		CheckAxis(etAxisZX);
		if (ImGui::Button("XZ", SizeBtn))
		{
			Tools->SetAxis(etAxisZX);
		}
		EndCheck();
		ImGui::Separator();
	}

	ImGui::End();
}