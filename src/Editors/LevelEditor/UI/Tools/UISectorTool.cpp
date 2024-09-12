#include "stdafx.h"
UISectorTool::UISectorTool()
{
	m_Edit = false;
    m_CreateNewMultiple = false;
    m_CreateNewSingle = false;
    m_MeshAdd = true;
    m_BoxPick = false;
}

UISectorTool::~UISectorTool()
{
}
void UISectorTool::Draw()
{
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_commands").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::Button(g_pStringTable->translate("ed_st_validate_sectors").c_str(), ImVec2(-1, 0)))
            {
                PortalUtils.Validate(true);
            }
            if (ImGui::Button(g_pStringTable->translate("ed_st_capture_volume").c_str(), ImVec2(-1, 0)))
            {
                CSector* S = PortalUtils.GetSelectedSector();
                if (S) {
                    S->CaptureInsideVolume();
                    Scene->UndoSave();
                }
            }
            if (ImGui::Button(g_pStringTable->translate("ed_st_distribute_objects").c_str(), ImVec2(-1, 0)))
            {
                CSector* S = PortalUtils.GetSelectedSector();
                if (S) {
                    S->DistributeInsideObjects();
                    Scene->UndoSave();
                }
            }
            ImGui::Separator();
            if (ImGui::Button(g_pStringTable->translate("ed_st_create_default").c_str(), ImVec2(-1, 0)))
            {
                CCustomObject* O = Scene->FindObjectByName(DEFAULT_SECTOR_NAME, OBJCLASS_SECTOR);
                if (O) ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_default_sector_present").c_str());
                else {
                    if (!PortalUtils.CreateDefaultSector()) ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_cant_create_default").c_str());
                }
            }
            if (ImGui::Button(g_pStringTable->translate("ed_st_remove_default").c_str(), ImVec2(-1, 0)))
            {
                if (!PortalUtils.RemoveDefaultSector()) ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_no_default_sector").c_str());
            }

        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
    if(m_Edit)ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (m_Edit && ImGui::TreeNode(g_pStringTable->translate("ed_st_edit").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::Checkbox(g_pStringTable->translate("ed_st_create_from_mesh").c_str(), &m_CreateNewSingle))
            {
                m_CreateNewMultiple = false;
            }
            if (ImGui::Checkbox(g_pStringTable->translate("ed_st_create_from_obj").c_str(), &m_CreateNewMultiple))
            {
                m_CreateNewSingle = false;
            }
            ImGui::Separator();
            ImGui::Text(g_pStringTable->translate("ed_st_meshes").c_str());
            ImGui::SameLine();
            // St4lker0k765: хз зачем, но на всякий случай M+ и M- тоже перевёл в string_table
            if (ImGui::RadioButton(g_pStringTable->translate("ed_st_plus_mesh").c_str(), m_MeshAdd))
            {
                m_MeshAdd = true;
           }
            ImGui::SameLine();
            if (ImGui::RadioButton(g_pStringTable->translate("ed_st_minus_mesh").c_str(), !m_MeshAdd))
            {
                m_MeshAdd = false;
            }
            if (ImGui::Checkbox(g_pStringTable->translate("ed_st_box_pick").c_str(), &m_BoxPick));
            if (m_CreateNewSingle || m_CreateNewMultiple)
            {
                m_BoxPick = false;
            }
        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }

}
