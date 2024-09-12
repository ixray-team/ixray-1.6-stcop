#include "stdafx.h"
UIPortalTool::UIPortalTool()
{
}

UIPortalTool::~UIPortalTool()
{
}

void UIPortalTool::Draw()
{
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_commands").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::Button(g_pStringTable->translate("ed_st_invert_orientation").c_str(), ImVec2(-1, 0)))
            {
                ObjectList lst;
                if (Scene->GetQueryObjects(lst, OBJCLASS_PORTAL, 1, 1, 0)) {
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
                        CPortal* _O = (CPortal*)*it;
                        _O->InvertOrientation(true);
                    }
                }
            }
            if (ImGui::Button(g_pStringTable->translate("ed_st_compute_all_portals").c_str(), ImVec2(-1, 0)))
            {
                if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes |mbNo, g_pStringTable->translate("ed_st_compute_all_portals_msg").c_str()))
                {
                    int cnt = PortalUtils.CalculateAllPortals();
                    if (cnt) ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_calculated_portals_count").c_str(), cnt);
                }
            }
            if (ImGui::Button(g_pStringTable->translate("ed_st_compute_sel_portals").c_str(), ImVec2(-1, 0)))
            {
                if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes |mbNo, g_pStringTable->translate("ed_st_compute_all_portals_msg").c_str()))
                {
                    int cnt = PortalUtils.CalculateSelectedPortals();
                    if (cnt) ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_calculated_portals_count").c_str(), cnt);
                }
            }
            if (ImGui::Button(g_pStringTable->translate("ed_st_remove_similar").c_str(), ImVec2(-1, 0)))
            {
                tool->RemoveSimilar();
            }
        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
}
