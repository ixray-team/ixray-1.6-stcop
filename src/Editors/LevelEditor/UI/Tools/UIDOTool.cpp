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
	if (ImGui::TreeNode(g_pStringTable->translate("ed_st_commands").c_str()))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			if (ImGui::Button(g_pStringTable->translate("ed_st_first_init").c_str(), ImVec2(-1, 0)))
			{
				if (DM->Initialize())
					Scene->UndoSave(); 
			}
			if (ImGui::Button(g_pStringTable->translate("ed_st_reinit_all").c_str(), ImVec2(-1, 0)))
			{
				if (DM->Reinitialize())
					Scene->UndoSave();
			}
			if (ImGui::Button(g_pStringTable->translate("ed_st_reinit_obj_only").c_str(), ImVec2(-1, 0)))
			{
				if (DM->UpdateObjects(true, false))
					Scene->UndoSave();
			}
			if (ImGui::Button(g_pStringTable->translate("ed_st_reinit_sel_obj").c_str(), ImVec2(-1, 0)))
			{
				if (DM->UpdateObjects(false, true))
					Scene->UndoSave();
			}
			ImGui::Separator();
			if (ImGui::Button(g_pStringTable->translate("ed_st_update_renderer").c_str(), ImVec2(-1, 0)))
			{
				DM->InvalidateCache();
				Scene->UndoSave();
			}
			ImGui::Separator();
			if (ImGui::Button(g_pStringTable->translate("ed_st_clear_slots").c_str(), ImVec2(-1, 0)))
			{
				if (ELog.DlgMsg(mtConfirmation, mbYes| mbNo, g_pStringTable->translate("ed_st_clear_slots_msg").c_str()) == mrYes) {
					DM->ClearSlots();
					Scene->UndoSave();
				}
			}
			if (ImGui::Button(g_pStringTable->translate("ed_st_clear_details").c_str(), ImVec2(-1, 0)))
			{
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, g_pStringTable->translate("ed_st_clear_details_msg").c_str()) == mrYes) {
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
					DM->Clear();
					Scene->UndoSave();
				}
			}
			ImGui::Separator();
			if (ImGui::Button(g_pStringTable->translate("ed_st_obj_list").c_str(), ImVec2(-1, 0))) { m_DOShuffle = true; UIDOShuffle::Show(DM); }
		}

		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
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
