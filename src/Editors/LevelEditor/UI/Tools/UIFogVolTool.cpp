#include "stdafx.h"
UIFogVolTool::UIFogVolTool()
{
}

UIFogVolTool::~UIFogVolTool()
{
}

void UIFogVolTool::Draw()
{
	float ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			{
				if (ImGui::Button("Group Selected", { SizeX, 0.f }))ParentTools->GroupSelected();
				ImGui::SameLine(0, ItemSpacingX);
				if (ImGui::Button("UnGroup Selected", { SizeX, 0.f }))ParentTools->UnGroupCurrent();
			}
			XRay::ImGui::EndExpand();
		}
		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}
