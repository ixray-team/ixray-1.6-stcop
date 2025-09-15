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

			if (ImGui::Button("Add PAC"))
			{
				PTools->AppendPAC(0, "pac");
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
				PTools->m_SelectedTypes = PEd::ListTypeBase(PEd::LisType::All);
			}
			ImGui::SameLine();
			if (ImGui::Button("Groups"))
			{
				PTools->m_SelectedTypes = PEd::ListTypeBase(PEd::LisType::Groups);
			}
			ImGui::SameLine();
			if (ImGui::Button("Effects"))
			{
				PTools->m_SelectedTypes = PEd::ListTypeBase(PEd::LisType::Effects);
			}
			ImGui::SameLine();
			if (ImGui::Button("Anim Curves"))
			{
				PTools->m_SelectedTypes = PEd::ListTypeBase(PEd::LisType::AnimCurve);
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
