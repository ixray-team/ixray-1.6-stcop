#include "stdafx.h"
#include "UITopBarForm.h"
#include "IconsFontAwesome6.h"

UITopBarForm::UITopBarForm()
{
    m_timeUndo  = 0;
    m_timeRedo  = 0;

    m_tReload   = EDevice->Resources->_CreateTexture("ed\\bar\\reload_configs");
}

UITopBarForm::~UITopBarForm() {}

#define IMGUI_HINT_BUTTON(Name, Ptr, Hint, Callback) \
			Ptr->Load(); \
			if (ImGui::ImageButton("##" Name, Ptr->get_SRView()->GetRawSRV(), ImVec2(20, 20))) \
				Callback(); \
			if (ImGui::IsItemHovered()) \
			{ \
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand); \
				ImGui::SetTooltip(Hint); \
			} \
			ImGui::SameLine()

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)
#define IMGUI_HINT_AF_BUTTON_EX(Name, Timer, Hint, Callback) \
			if (ImGui::Button(Name"##" STR(__LINE__) , ImVec2(yMaxSize, yMaxSize))) \
			{ \
				Callback(); \
				Timer = EDevice->TimerAsync() + 130;\
			} \
			if (ImGui::IsItemHovered()) \
			{ \
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand); \
				ImGui::SetTooltip(Hint); \
			} \
			ImGui::SameLine()
#define IMGUI_HINT_AF_BUTTON(Name, Hint, Callback) \
			if (ImGui::Button(Name"##" STR(__LINE__), ImVec2(yMaxSize, yMaxSize))) \
				Callback(); \
			if (ImGui::IsItemHovered()) \
			{ \
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand); \
				ImGui::SetTooltip(Hint); \
			} \
			ImGui::SameLine()

void UITopBarForm::Draw()
{
    ImGuiViewport* viewport = ImGui::GetMainViewport();
    ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + UI->GetMenuBarHeight()));
    ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, UIToolBarSize));
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
    ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(2, 0));
    ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(2, 2));
    ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(-2, 0));
    ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(6, 6));
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);
    ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.f, 0.f, 0.f, 0.f));

    ImGui::Begin("TOOLBAR", NULL, window_flags);
    {
        ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 6);
        ImGui::SetCursorPosY(ImGui::GetCursorPosY() - 6);

        if (ImGui::BeginTable("##ToolbarTable", 4, ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_ContextMenuInBody | ImGuiTableFlags_Hideable))
        {
            ImGui::TableSetupColumn("Actions");
            ImGui::TableSetupColumn("File");
            ImGui::TableSetupColumn("Directory Actions");
            ImGui::TableSetupColumn("Preferences");

            auto yMaxSize = ImGui::GetContentRegionAvail().y;

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_AF_BUTTON_EX(ICON_FA_ROTATE_LEFT, m_timeUndo, "Undo the last action", ClickUndo);
                IMGUI_HINT_AF_BUTTON_EX(ICON_FA_ROTATE_RIGHT, m_timeRedo, "Repeat the last action", ClickRedo);
            }

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_BUTTON("Reload", m_tReload, "Reload everything '.xr'", ClickReload);
                IMGUI_HINT_AF_BUTTON(ICON_FA_FLOPPY_DISK, "Save all '.xr'", ClickSave);
            }

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_AF_BUTTON(ICON_FA_FOLDER_OPEN, "Open 'gamedata' folder", ClickOpenGameData);
            }

            if (ImGui::TableNextColumn())
            {
                IMGUI_HINT_AF_BUTTON(ICON_FA_SLIDERS, "Preferences", ClickPreferences);
            }
        }
        ImGui::EndTable();
    }
    ImGui::End();
    ImGui::PopStyleColor();
    ImGui::PopStyleVar(7);
}

void UITopBarForm::ClickUndo()
{
    ExecCommand(COMMAND_UNDO);
}

void UITopBarForm::ClickRedo()
{
    ExecCommand(COMMAND_REDO);
}

void UITopBarForm::ClickSave()
{
    ExecCommand(COMMAND_SAVE);
}
void UITopBarForm::ClickReload()
{
    ExecCommand(COMMAND_LOAD);
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
