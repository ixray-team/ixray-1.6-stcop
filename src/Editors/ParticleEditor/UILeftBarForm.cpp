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
		}

		ImGui::Separator();

		if (ImGui::Button("Import Effect"))
		{
			PTools->ImportPE();
		}
		
		ImGui::Separator();

		{
			if (ImGui::Button("All Particles"))
			{
				PTools->m_SelectedTypes = PEd::LisType::Groups | PEd::LisType::Effects;
			}
			ImGui::SameLine();
			if (ImGui::Button("Groups"))
			{
				PTools->m_SelectedTypes = u8(PEd::LisType::Groups);
			}
			ImGui::SameLine();
			if (ImGui::Button("Effects"))
			{
				PTools->m_SelectedTypes = u8(PEd::LisType::Effects);
			}
		}
		
		ImGui::Separator();

		ImGui::SetNextItemOpen(true, ImGuiCond_Once);

		if (ImGui::TreeNode("Items"))
		{
			ImGui::BeginGroup();
			auto CurrentList = PTools->GetCurrentList();
			R_ASSERT(CurrentList);
			CurrentList->Draw();
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
