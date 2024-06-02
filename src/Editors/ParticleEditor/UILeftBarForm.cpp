#include "stdafx.h"

UILeftBarForm::UILeftBarForm()
{
}

UILeftBarForm::~UILeftBarForm()
{
}

void UILeftBarForm::Draw()
{
	if (ImGui::Begin("LeftBar", 0))
	{
		if (ImGui::Button("Add Group"))
		{
			PTools->AppendPG(0, "pg");
			PTools->Modified();
		}
		ImGui::SameLine();

		if (ImGui::Button("Add Particle"))
		{
			PTools->AppendPE(0, "pe");
			PTools->Modified();
		}
		ImGui::SameLine();

		if (ImGui::Button("Clone"))
		{
			PTools->CloneCurrent();
		}
		ImGui::SameLine();

		if (ImGui::Button("Remove"))
		{
			PTools->RemoveCurrent();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_Once);

		if (ImGui::TreeNode("Items"))
		{
			ImGui::BeginGroup();
			PTools->m_PList->Draw();
			ImGui::EndGroup();
			ImGui::TreePop();
		}

		if (ImGui::TreeNode("Reference List"))
		{
			PTools->DrawReferenceList();
			ImGui::TreePop();
		}
	}
	
	ImGui::End();
}
