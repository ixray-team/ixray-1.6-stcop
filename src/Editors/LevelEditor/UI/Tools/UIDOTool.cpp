#include "stdafx.h"
UIDOTool::UIDOTool()
{
	m_DOShuffle = false;
}

UIDOTool::~UIDOTool()
{
}

void UIDOTool::Draw()
{
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode("Commands"))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			if (ImGui::Button("First Initialize", ImVec2(-1, 0)))
			{
				if (DM->Initialize())
					Scene->UndoSave(); 
			}
			if (ImGui::Button("Reinitialize All", ImVec2(-1, 0))) 
			{
				if (DM->Reinitialize())
					Scene->UndoSave();
			}
			if (ImGui::Button("Reinitialize Objects Only", ImVec2(-1, 0)))
			{
				if (DM->UpdateObjects(true, false))
					Scene->UndoSave();
			}
			if (ImGui::Button("Reinitialize Selected Slot Objects", ImVec2(-1, 0)))
			{
				if (DM->UpdateObjects(false, true))
					Scene->UndoSave();
			}
			ImGui::Separator();
			if (ImGui::Button("Update Renderer", ImVec2(-1, 0)))
			{
				DM->InvalidateCache();
				Scene->UndoSave();
			}
			ImGui::Separator();
			if (ImGui::Button("Clear Slots", ImVec2(-1, 0)))
			{
				if (ELog.DlgMsg(mtConfirmation, mbYes| mbNo, "Are you sure to reset slots?") == mrYes) {
					DM->ClearSlots();
					Scene->UndoSave();
				}
			}
			if (ImGui::Button("Clear Details", ImVec2(-1, 0)))
			{
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to clear details?") == mrYes) {
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
					DM->Clear();
					Scene->UndoSave();
				}
			}
			ImGui::Separator();
			if (ImGui::Button("Object List", ImVec2(-1, 0))) { m_DOShuffle = true; UIDOShuffle::Show(DM); }
		}

		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}

	HandleDragDrop();
}

void UIDOTool::HandleDragDrop()
{
	const ImGuiPayload* payload = ImGui::GetDragDropPayload();

	if (payload && ImGui::IsMouseDragging(ImGuiMouseButton_Left) && GUIManager->DnDType == EDragDropType::Details)
	{
		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		ImVec2 p_min = ImGui::GetItemRectMin();
		ImVec2 p_max = ImGui::GetItemRectMax();
		draw_list->AddRectFilled(p_min, p_max, IM_COL32(50, 50, 70, 100));
		draw_list->AddRect(p_min, p_max, IM_COL32(100, 180, 255, 255));
	}

	if (!ImGui::BeginDragDropTarget())
		return;

	auto ImData = ImGui::AcceptDragDropPayload("TEST#dti");

	if (ImData == nullptr)
	{
		ImGui::EndDragDropTarget();
		return;
	}

	struct DragDropData
	{
		xr_string FileName;
	} Data = *(DragDropData*)ImData->Data;


	if (Data.FileName.ends_with(".dti"))
	{
		m_DOShuffle = true; 
		UIDOShuffle::Show(DM);
		UIDOShuffle::LoadFromStream(Data.FileName);
	}

	ImGui::EndDragDropTarget();
}

void UIDOTool::OnDrawUI()
{
	if (m_DOShuffle)
	{
		if (UIDOShuffle::GetResult())
		{
			m_DOShuffle = false;
		}
		UIDOShuffle::Update();
	}
}
