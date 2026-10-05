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
		if (!STools->m_Current && !STools->m_Tools.empty())
		{
			STools->OnChangeEditor(STools->m_Tools.begin()->second);
		}

		if (ISHTools* Tool = STools->m_Current)
		{
			if (ImGui::Button("Create"))
			{
				Tool->OnCreateItem("new_item");
				Tool->Modified();
			}
			ImGui::SameLine();

			if (ImGui::Button("Clone"))
			{
				if (Tool->m_CurrentItem != nullptr)
				{
					xr_string CloneName = Tool->m_CurrentItem->Key();
					CloneName += "_clone";

					Tool->OnCloneItem(Tool->m_CurrentItem->Key(), CloneName.c_str());
				}
			}
			ImGui::SameLine();

			if (ImGui::Button("Remove"))
			{
				Tool->RemoveCurrent();
			}

			if (!STools->m_PreviewProps->Empty())
			{
				ImGui::SetNextItemOpen(true, ImGuiCond_Once);
				if (XRay::ImGui::BeginExpand("Preview"))
				{
					if (ImGui::BeginChild("##ShPreview", { 0, 68 }))
					{
						STools->m_PreviewProps->Draw();
					}
					ImGui::EndChild();
					XRay::ImGui::EndExpand();
				}
			}

			ImGui::SetNextItemOpen(true, ImGuiCond_Once);
			if (XRay::ImGui::BeginExpand("Items"))
			{
				ImGui::BeginGroup();
				STools->m_Items->Draw();
				ImGui::EndGroup();
				XRay::ImGui::EndExpand();
			}
		}
	}
	ImGui::End();

	if (ImGui::Begin("Item Properties"))
	{
		STools->m_ItemProps->Draw();
	}
	ImGui::End();
}
