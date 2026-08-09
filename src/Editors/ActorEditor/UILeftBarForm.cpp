#include "stdafx.h"

#include "../xrECore/Editor/EditMesh.h"

extern ECORE_API bool g_force16BitTransformQuant;
extern ECORE_API bool g_force32BitTransformQuant;

UILeftBarForm::UILeftBarForm()
{
	m_RenderMode = Render_Editor;
	m_PickMode = 2;

	if (g_force16BitTransformQuant)
	{
		m_AnimMode = e16bit;
	}
	else if (g_force32BitTransformQuant)
	{
		m_AnimMode = e32bit;
	}
}

UILeftBarForm::~UILeftBarForm()
{
}

void UILeftBarForm::SetSmooth(ESmoothGroup mode)
{
    EPrefs->SmoothGroup = mode;

    if (!ATools->CurrentObject())
        return;

    for (CEditableMesh* Mesh : ATools->CurrentObject()->Meshes())
    {
        u32 Count = Mesh->m_SVertInfl;
        Mesh->UnloadSVertices();
        Mesh->GenerateSVertices(Count);
    }
}

void UILeftBarForm::SetAnim(EAnimMode mode)
{
    m_AnimMode = mode;

    g_force16BitTransformQuant = (mode == e16bit);
    g_force32BitTransformQuant = (mode == e32bit);
}

void UILeftBarForm::Draw()
{
    ImVec2	ItemSpacing = ImGui::GetStyle().ItemSpacing;
    ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, { 300, 100 });
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(ItemSpacing.y, ItemSpacing.y));
	ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);
    if (ImGui::Begin("Object Tool", 0))
    {
        if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
        {

            ImGui::SetNextItemOpen(true, ImGuiCond_Once);
            if (XRay::ImGui::BeginExpand("Render"))
            {

                bool editor = (m_RenderMode == Render_Editor);
                bool engine = (m_RenderMode == Render_Engine);

                if (XRay::ImGui::BeginTable("RenderSetttings", 2, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
                {
                    ImGui::TableSetupColumn("##Key", ImGuiTableColumnFlags_WidthFixed);
                    ImGui::TableSetupColumn("##Values", ImGuiTableColumnFlags_WidthStretch);

                    XRay::ImGui::TableNextRow();
                    XRay::ImGui::TableNextColumn();     XRay::ImGui::TextFramed("Render Style:");
                    XRay::ImGui::TableNextColumn();

                    ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
                    if (XRay::ImGui::ToolbarButton("editor", "Editor", &editor, { 0, 0 }, ImDrawFlags_RoundCornersLeft))
                    {
                        ATools->PhysicsStopSimulate();
                        m_RenderMode = Render_Editor;
                        ExecCommand(COMMAND_UPDATE_PROPERTIES);
                        UI->RedrawScene();
                    }
                    ImGui::SameLine();
                    if (XRay::ImGui::ToolbarButton("engine", "Engine", &engine, { 0, 0 }, ImDrawFlags_RoundCornersRight))
                    {
                        ATools->PhysicsStopSimulate();

                        if (!ATools->IsVisualPresent())
                            ExecCommand(COMMAND_MAKE_PREVIEW);

                        if (!ATools->IsVisualPresent())
                            SetRenderMode(false);
                        else
                            SetRenderMode(true);

                        ExecCommand(COMMAND_UPDATE_PROPERTIES);
                        UI->RedrawScene();
                    }
                    ImGui::PopStyleVar();

                    XRay::ImGui::TableNextRow();
                    XRay::ImGui::TableNextColumn();     XRay::ImGui::TextFramed("Smooth:");
                    XRay::ImGui::TableNextColumn();

                    bool edge = EPrefs->SmoothGroup == ESmoothGroup::Edges;
                    bool normal = EPrefs->SmoothGroup == ESmoothGroup::Normals;
                    bool other = EPrefs->SmoothGroup == ESmoothGroup::Other;

                    if (XRay::ImGui::ToolbarButton("edge", "Edges", &edge, { 0, 0 }, ImDrawFlags_RoundCornersLeft))
                    {
                        SetSmooth(ESmoothGroup::Edges);
                    }

                    ImGui::SameLine(0, 0);

                    if (XRay::ImGui::ToolbarButton("normal", "Normals", &normal, { 0, 0 }, ImDrawFlags_RoundCornersNone))
                    {
                        SetSmooth(ESmoothGroup::Normals);
                    }

                    ImGui::SameLine(0, 0);

                    if (XRay::ImGui::ToolbarButton("other", "Legacy", &other, { 0, 0 }, ImDrawFlags_RoundCornersRight))
                    {
                        SetSmooth(ESmoothGroup::Other);
                    }

                    XRay::ImGui::EndTable();
                }

                XRay::ImGui::EndExpand();
            }

        ImGui::SetNextItemOpen(true, ImGuiCond_Once);
        if (XRay::ImGui::BeginExpand("Animation"))
        {
            bool b8 = (m_AnimMode == e8bit);
            bool b16 = (m_AnimMode == e16bit);
            bool b32 = (m_AnimMode == e32bit);

            if (XRay::ImGui::BeginTable("RenderSetttings", 2, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
            {
                ImGui::TableSetupColumn("##Key", ImGuiTableColumnFlags_WidthFixed);
                ImGui::TableSetupColumn("##Values", ImGuiTableColumnFlags_WidthStretch);

                XRay::ImGui::TableNextRow();
                XRay::ImGui::TableNextColumn();     XRay::ImGui::TextFramed("Anim Format:");
                XRay::ImGui::TableNextColumn();

                ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
                if (XRay::ImGui::ToolbarButton("8bit", "8-bit", &b8, { 0, 0 }, ImDrawFlags_RoundCornersLeft))
                    SetAnim(e8bit);

                ImGui::SameLine();

                if (XRay::ImGui::ToolbarButton("16bit", "16-bit", &b16, { 0, 0 }, ImDrawFlags_RoundCornersNone))
                    SetAnim(e16bit);

                ImGui::SameLine();

                if (XRay::ImGui::ToolbarButton("32bit", "32-bit", &b32, { 0, 0 }, ImDrawFlags_RoundCornersRight))
                    SetAnim(e32bit);
                ImGui::PopStyleVar();

                XRay::ImGui::EndTable();
            }

            XRay::ImGui::EndExpand();
        }

        ImGui::SetNextItemOpen(true, ImGuiCond_Once);
        if (XRay::ImGui::BeginExpand("Tools"))
        {
            if (XRay::ImGui::Button("Bone View", { -0.01, 0 }))
            {
                ATools->BoneView->Show(true);
            }

            if (XRay::ImGui::Button("Bone Parts", { -0.01, 0 }))
            {
                UIBoneForm::Show();
            }

            XRay::ImGui::EndExpand();
        }

        ImGui::SetNextItemOpen(true, ImGuiCond_Once);
        if (XRay::ImGui::BeginExpand("Pick"))
        {
            ImGui::AlignTextToFramePadding();             // выравниваем текст по середине строки по Y
            XRay::ImGui::TextFramed("Mode:");
            ImGui::SameLine(0, 1.f);

            ImGui::SetNextItemWidth(-0.01);                  // растягиваем combo на всё оставшееся место
            static const char* PickModeList[] = { "None", "Surface", "Bone" };
            ImGui::Combo("##Mode", &m_PickMode, PickModeList, IM_ARRAYSIZE(PickModeList));

            XRay::ImGui::EndExpand();
        }
		    XRay::ImGui::EndDarkChild();
        }
    }

    ImGui::End();
	ImGui::PopStyleVar();

    // OBJECT ITEMS
    if (ImGui::Begin("Object Properties"))
    {
        ImGui::BeginGroup();
        ATools->m_ObjectItems->Draw();
        ImGui::EndGroup();


        if (ATools->CurrentObject() != nullptr)
        {
            if (!ATools->CurrentObject()->m_objectFlags.test(CEditableObject::eoDynamic))
            {
                ImGui::Separator();

                if (ImGui::Button("Make dynamic", { -1, 0 }))
                {
                    ATools->CurrentObject()->CreateBone("idle");

                    for (EditMeshIt mesh_it = ATools->CurrentObject()->FirstMesh();
                        mesh_it != ATools->CurrentObject()->LastMesh();
                        mesh_it++)
                    {
                        CEditableMesh* pMesh = *mesh_it;
                        pMesh->AssignMesh("idle");
                    }

                    ATools->RealUpdateProperties();
                }
            }
        }
    }
    ImGui::End();

	ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);
    if (ImGui::Begin("Item Properties", 0))
    {
        ImGui::BeginGroup();
        ATools->m_Props->Draw();
        ImGui::EndGroup();
    }
    ImGui::End();

    ImGui::PopStyleVar(3);
}

void UILeftBarForm::SetRenderMode(bool bEngineMode)
{
	if (ATools->IsVisualPresent() && bEngineMode)
		m_RenderMode = Render_Engine;
	else 		
		m_RenderMode = Render_Editor;
	ATools->PlayMotion();
}
