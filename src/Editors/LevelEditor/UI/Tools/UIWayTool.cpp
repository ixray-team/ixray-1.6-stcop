#include "stdafx.h"

UIWayTool::UIWayTool()
{
	m_WayMode = true;
	m_AutoLink = true;
}

UIWayTool::~UIWayTool()
{
}

void UIWayTool::Draw()
{
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_commands").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::RadioButton(g_pStringTable->translate("ed_st_way_mode").c_str(), m_WayMode))
            {
                LTools->SetTarget(OBJCLASS_WAY, 0);
                m_WayMode = true;
            }
            ImGui::SameLine();
            if (ImGui::RadioButton(g_pStringTable->translate("ed_st_way_point").c_str(), m_WayMode == false))
            {
                LTools->SetTarget(OBJCLASS_WAY, 1);
                m_WayMode = false;
            }
        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_link_cmd").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::Checkbox(g_pStringTable->translate("ed_st_auto_link").c_str(), &m_AutoLink))
            {
               
            }
            ImGui::PushItemWidth(-1);
            float size = float(ImGui::CalcItemWidth());
            {
                if (ImGui::Button(g_pStringTable->translate("ed_st_create_1_link").c_str(), ImVec2(size / 2, 0)))
                {
                    if (m_WayMode) {
                        ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_enter_point_mode").c_str());
                        return;
                    }
                    bool bRes = false;
                    ObjectList lst;
                    Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
                    // remove links
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
                        ((CWayObject*)(*it))->RemoveLink();
                        bRes |= ((CWayObject*)(*it))->Add1Link();
                    }
                    if (bRes) Scene->UndoSave();
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }
                ImGui::SameLine(0, 2);
                if (ImGui::Button(g_pStringTable->translate("ed_st_convert_to_1_link").c_str(), ImVec2(size / 2, 0)))
                {
                    ObjectList lst;
                    int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++)
                        ((CWayObject*)(*it))->Convert1Link();
                    if (cnt) Scene->UndoSave();
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }

                if (ImGui::Button(g_pStringTable->translate("ed_st_create_2_link").c_str(), ImVec2(size / 2, 0)))
                {
                    if (m_WayMode) {
                        ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_enter_point_mode").c_str());
                        return;
                    }
                    bool bRes = false;
                    ObjectList lst;
                    Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++)
                        bRes |= ((CWayObject*)(*it))->Add2Link();
                    if (bRes) Scene->UndoSave();
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }
                ImGui::SameLine(0, 2);
                if (ImGui::Button(g_pStringTable->translate("ed_st_convert_to_2_link").c_str(), ImVec2(size / 2, 0)))
                {
                    ObjectList lst;
                    int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++)
                        ((CWayObject*)(*it))->Convert2Link();
                    if (cnt) Scene->UndoSave();
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }

                if (ImGui::Button(g_pStringTable->translate("ed_st_invert_link").c_str(), ImVec2(size / 2, 0)))
                {
                    if (m_WayMode) {
                        ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_enter_point_mode").c_str());
                        return;
                    }
                    ObjectList lst;
                    int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++)
                        ((CWayObject*)(*it))->InvertLink();
                    if (cnt) Scene->UndoSave();
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }
                ImGui::SameLine(0, 2);
                if (ImGui::Button(g_pStringTable->translate("ed_st_remove_link").c_str(), ImVec2(size / 2, 0)))
                {
                    if (m_WayMode) {
                        ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_enter_point_mode").c_str());
                        return;
                    }
                    ObjectList lst;
                    int cnt = Scene->GetQueryObjects(lst, OBJCLASS_WAY, 1, 1, 0);
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++)
                        ((CWayObject*)(*it))->RemoveLink();
                    if (cnt) Scene->UndoSave();
                    ExecCommand(COMMAND_UPDATE_PROPERTIES);
                }
            }

        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
}
