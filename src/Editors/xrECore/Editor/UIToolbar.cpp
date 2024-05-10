#include "stdafx.h"
#include "UIToolbar.h"
#include "UI_ToolsCustom.h"

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

	if (ImGui::Begin("##ToolbarHor", nullptr, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoScrollbar))
	{
		bFocus = ImGui::IsWindowFocused();
		constexpr ImVec2 SizeImg = { 15, 18 };
		constexpr ImVec2 SizeBtn = { 25, 25 };

		if (ImGui::ImageButton(Select->pSurface, SizeImg))
		{
			Tools->SetAction(etaSelect);
		}

		if (ImGui::ImageButton(Move->pSurface, SizeImg))
		{
			Tools->SetAction(etaMove);
		}

		if (ImGui::ImageButton(Rotate->pSurface, SizeImg))
		{
			Tools->SetAction(etaRotate);
		}

		if (ImGui::ImageButton(Scale->pSurface, SizeImg))
		{
			Tools->SetAction(etaScale);
		}
		ImGui::Separator();

		if (ImGui::Button("X", SizeBtn))
		{
			Tools->SetAxis(etAxisX);
		}

		if (ImGui::Button("Y", SizeBtn))
		{
			Tools->SetAxis(etAxisY);
		}

		if (ImGui::Button("Z", SizeBtn))
		{
			Tools->SetAxis(etAxisZ);
		}

		if (ImGui::Button("XZ", SizeBtn))
		{
			Tools->SetAxis(etAxisZX);
		}

		ImGui::Separator();
	}

	ImGui::End();
}