#include "stdafx.h"

UIShapeTool::UIShapeTool()
{
    Tool = nullptr;
    m_AttachShape = false;
}

UIShapeTool::~UIShapeTool()
{
}

void UIShapeTool::Draw()
{
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_commands").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::RadioButton(g_pStringTable->translate("ed_st_sphere").c_str(), m_SphereMode))
            {
                m_SphereMode = true;
            }ImGui::SameLine();
            if (ImGui::RadioButton(g_pStringTable->translate("ed_st_box").c_str(), m_SphereMode == false))
            {
                m_SphereMode = false;
            }
        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_edit").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::Checkbox(g_pStringTable->translate("ed_st_attach_shape").c_str(), &m_AttachShape))
            {
                if(m_AttachShape)
                ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
            }
            ImGui::SameLine(0, 10);
            if (ImGui::Button(g_pStringTable->translate("ed_st_detach_all").c_str(), ImVec2(-1, 0)))
            {
                ObjectList lst;
                if (Scene->GetQueryObjects(lst, OBJCLASS_SHAPE, 1, 1, 0)) {
                    Scene->SelectObjects(false, OBJCLASS_SHAPE);
                    for (ObjectIt it = lst.begin(); it != lst.end(); it++)
                        ((CEditShape*)*it)->Detach();
                }
            }
        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
    ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
    if (ImGui::TreeNode(g_pStringTable->translate("ed_st_level_bound").c_str()))
    {
        ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
        {
            if (ImGui::Checkbox(g_pStringTable->translate("ed_st_edit_level_bound").c_str(), &EditLevelBound))
            {
                if (EditLevelBound)
                    Tool->OnEditLevelBounds(false);
            }
            if(EditLevelBound)
            if (ImGui::Button(g_pStringTable->translate("ed_st_recalc").c_str(), ImVec2(-1, 0)))
            {
                Tool->OnEditLevelBounds(true);
            }
        }
        ImGui::Separator();
        ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
        ImGui::TreePop();
    }
}
