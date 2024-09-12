#include "stdafx.h"
UIFogVolTool::UIFogVolTool()
{
}

UIFogVolTool::~UIFogVolTool()
{
}

void UIFogVolTool::Draw()
{
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode(g_pStringTable->translate("ed_st_commands").c_str()))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::PushItemWidth(-1);
		float size = float(ImGui::CalcItemWidth());
		{
			if (ImGui::Button(g_pStringTable->translate("ed_st_group_sel").c_str(), ImVec2(size / 2, 0)))ParentTools->GroupSelected();
			ImGui::SameLine(0, 2);
			if (ImGui::Button(g_pStringTable->translate("ed_st_ungroup_sel").c_str(), ImVec2(size / 2, 0)))ParentTools->UnGroupCurrent();
		}

		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
}
