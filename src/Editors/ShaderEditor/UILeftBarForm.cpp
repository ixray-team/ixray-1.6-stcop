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
		if (ImGui::BeginTabBar("##tabs", ImGuiTabBarFlags_TabListPopupButton | ImGuiTabBarFlags_FittingPolicyScroll))
		{
			for (auto& tool : STools->m_Tools)
			{
				const char* name = tool.second->ToolsName();
				if (ImGui::BeginTabItem(name))
				{
					if (STools->m_Current != tool.second)
					{
						STools->OnChangeEditor(tool.second);
					}
					ImGui::BeginChild("main", ImVec2(0, 0), false, 0);


					if (ImGui::Button("Create"))
					{
						tool.second->OnCreateItem("new_item");
						tool.second->Modified();
					}
					ImGui::SameLine();

					if (ImGui::Button("Clone"))
					{
						if (tool.second->m_CurrentItem != nullptr)
						{
							xr_string CloneName = tool.second->m_CurrentItem->Key();
							CloneName += "_clone";

							tool.second->OnCloneItem(tool.second->m_CurrentItem->Key(), CloneName.c_str());
						}
					}
					ImGui::SameLine();

					if (ImGui::Button("Remove"))
					{
						tool.second->RemoveCurrent();
					}

					if (!STools->m_PreviewProps->Empty())
					{
						ImGui::SetNextItemOpen(true, ImGuiCond_Once);
						if (ImGui::TreeNode("Preview"))
						{
							STools->m_PreviewProps->Draw();
							ImGui::TreePop();
						}
					}

					ImGui::SetNextItemOpen(true, ImGuiCond_Once);
					if (ImGui::TreeNode("Items"))
					{
						ImGui::BeginGroup();
						STools->m_Items->Draw();
						ImGui::EndGroup();
						ImGui::TreePop();
					}
					ImGui::EndChild();
					ImGui::EndTabItem();
				}

			}

			ImGui::EndTabBar();
		}
	}
	ImGui::End();

	if (ImGui::Begin("Item Properties"))
	{
		STools->m_ItemProps->Draw();
	}
	ImGui::End();
}
