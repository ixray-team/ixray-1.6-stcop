#include "stdafx.h"
#include "UITopBarForm.h"
#include "IconsFontAwesome6.h"

UITopBarForm::UITopBarForm()
{
    m_timeUndo  = 0;
    m_timeRedo  = 0;

    InitIcons();

    m_Simulate  = false;
}

UITopBarForm::~UITopBarForm() {}

#define IMGUI_HINT_BUTTON(Name, Ptr, Hint, dImDrawFlags, Callback) \
			Ptr->Load(); \
			if (XRay::ImGui::ToolbarIconButton("##" Name, Ptr->get_SRView()->GetRawSRV(), nullptr, dImDrawFlags)) \
				Callback(); \
			if (ImGui::IsItemHovered()) \
			{ \
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand); \
				ImGui::SetTooltip(Hint); \
			} \
			ImGui::SameLine()

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

void UITopBarForm::Draw()
{
	ImGuiViewport* viewport = ImGui::GetMainViewport();
    const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
    const float IconSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::IconSize);
    const float ButtonRadius = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonRadius);
    const float ToolbarPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding);
    ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + UI->GetMenuBarHeight()));
	ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, ButtonSize + ToolbarPadding * 2));
	ImGui::SetNextWindowViewport(viewport->ID);

	ImGuiWindowFlags window_flags = 0
		| ImGuiWindowFlags_NoDocking
		| ImGuiWindowFlags_NoTitleBar
		| ImGuiWindowFlags_NoResize
		| ImGuiWindowFlags_NoMove
		| ImGuiWindowFlags_NoScrollbar
		;
    ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.f);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(ToolbarPadding, 0));
    ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
    ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(ToolbarPadding * 0.5f, ToolbarPadding));
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);
    ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.f, 0.f, 0.f, 0.f));
    ImGui::PushStyleColor(ImGuiCol_WindowBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);

    ImGui::Begin("TOOLBAR", NULL, window_flags);
    {
        if (ImGui::BeginTable("##ToolbarTable", 5, ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_ContextMenuInBody | ImGuiTableFlags_Hideable))
        {
            ImGui::TableSetupColumn("Actions");
            ImGui::TableSetupColumn("File");
            ImGui::TableSetupColumn("Directory Actions");
            ImGui::TableSetupColumn("Physics");
            ImGui::TableSetupColumn("Preferences");

            auto yMaxSize = ImGui::GetContentRegionAvail().y;

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_BUTTON("Undo", Icons["undo"], "Undo the last action", ImDrawFlags_RoundCornersLeft, ClickUndo);
                IMGUI_HINT_BUTTON("Redo", Icons["redo"], "Repeat the last action", ImDrawFlags_RoundCornersRight, ClickRedo);
            }

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_BUTTON("I_CNS", Icons["new_scene"], "Clear/New Scene", ImDrawFlags_RoundCornersLeft, ClickNew);
                IMGUI_HINT_BUTTON("I_OL", Icons["open_level"], "Open level", ImDrawFlags_RoundCornersNone, ClickOpen);
                IMGUI_HINT_BUTTON("I_SL", Icons["save_level"], "Save level", ImDrawFlags_RoundCornersRight, ClickSave);
            }

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_BUTTON("I_OGF", Icons["open_gamedata_folder"], "Open 'gamedata' folder", ImDrawFlags_RoundCornersAll, ClickOpenGameData);
            }

            if (ImGui::TableNextColumn())
            {
                if (XRay::ImGui::ToolbarButton("PhysSimulation228", "Phys Simulation", &m_Simulate, { 0.f, ButtonSize }, ImDrawFlags_RoundCornersAll))
                {
                    bool isPhysics = ATools->IsPhysics();

                    if (isPhysics)
                        ATools->PhysicsSimulate();
                    else
                        ATools->PhysicsStopSimulate();
                }
                if (ImGui::IsItemHovered())
                {
                    ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
                    ImGui::SetTooltip("Activates physics simulation");
                }
            }

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_BUTTON("I_Preferences", Icons["preferences"], "Preferences", ImDrawFlags_RoundCornersAll, ClickPreferences);
            }
        }
        ImGui::EndTable();
    }
    ImGui::End();
    ImGui::PopStyleColor(2);
    ImGui::PopStyleVar(6);
}

void UITopBarForm::InitIcons()
{
    Icons["undo"] = EDevice->Resources->_CreateTexture("ed\\icons\\Edit Undo");
    Icons["redo"] = EDevice->Resources->_CreateTexture("ed\\icons\\Edit Redo");
    Icons["new_scene"] = EDevice->Resources->_CreateTexture("ed\\icons\\File New");
    Icons["open_level"] = EDevice->Resources->_CreateTexture("ed\\icons\\File Open");
    Icons["save_level"] = EDevice->Resources->_CreateTexture("ed\\icons\\File Save");
    Icons["open_gamedata_folder"] = EDevice->Resources->_CreateTexture("ed\\icons\\File Open Game Data Folder");
    Icons["preferences"] = EDevice->Resources->_CreateTexture("ed\\icons\\Tab Preferences");
}

void UITopBarForm::ClickUndo()
{
    ExecCommand(COMMAND_UNDO);
}

void UITopBarForm::ClickRedo()
{
    ExecCommand(COMMAND_REDO);
}

void UITopBarForm::ClickNew()
{
    ExecCommand(COMMAND_CLEAR);
}

void UITopBarForm::ClickOpen()
{
    ExecCommand(COMMAND_LOAD);
}

void UITopBarForm::ClickSave()
{
    ExecCommand(COMMAND_SAVE, xr_string(ATools->m_LastFileName.c_str()));
}

void UITopBarForm::ClickOpenGameData()
{
    string_path GameDataPath;
    FS.update_path(GameDataPath, "$game_data$", "");
    ShellExecuteA(NULL, "open", GameDataPath, NULL, NULL, SW_SHOWDEFAULT);
}

void UITopBarForm::ClickPreferences()
{
    ExecCommand(COMMAND_EDITOR_PREF);
}
