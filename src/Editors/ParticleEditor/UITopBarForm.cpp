#include "stdafx.h"

UITopBarForm::UITopBarForm()
{
    m_tUndo            = EDevice->Resources->_CreateTexture("ed\\bar\\Undo");
    m_timeUndo         = 0;
    m_tRedo            = EDevice->Resources->_CreateTexture("ed\\bar\\Redo");
    m_timeRedo         = 0;
    m_tSaveParticles   = EDevice->Resources->_CreateTexture("ed\\bar\\save");
    m_tReloadParticles = EDevice->Resources->_CreateTexture("ed\\bar\\reload_configs");
    m_tOpen            = EDevice->Resources->_CreateTexture("ed\\bar\\open");
    m_tSaveXr          = EDevice->Resources->_CreateTexture("ed\\bar\\save_xr");
    m_tOpenGameData    = EDevice->Resources->_CreateTexture("ed\\bar\\open_gamedata");
    m_tValidate        = EDevice->Resources->_CreateTexture("ed\\bar\\validate");
}

UITopBarForm::~UITopBarForm() {}

void UITopBarForm::Draw()
{
    ImGuiViewport* viewport = ImGui::GetMainViewport();
    ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + UI->GetMenuBarHeight()));
    ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, UIToolBarSize));
    ImGui::SetNextWindowViewport(viewport->ID);

    ImGuiWindowFlags window_flags = 0
        | ImGuiWindowFlags_NoTitleBar
        | ImGuiWindowFlags_NoResize
        | ImGuiWindowFlags_NoDocking
        | ImGuiWindowFlags_NoMove
        | ImGuiWindowFlags_NoScrollbar
        | ImGuiWindowFlags_NoSavedSettings
        ;
    ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.f);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding,ImVec2( 2,4));
    ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(2, 2));
    ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 2));
    ImGui::Begin("TOOLBAR", NULL, window_flags);
    {
        m_tUndo->Load();
        if (ImGui::ImageButton(m_tUndo->pSurface, ImVec2(20, 20), ImVec2(m_timeUndo > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_timeUndo > EDevice->TimerAsync() ? 1 : 0.5, 1), 0))
        {
            m_timeUndo = EDevice->TimerAsync() + 130;
            ClickUndo();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Undo the last action.");
        }
        ImGui::SameLine();

        m_tRedo->Load();
        if (ImGui::ImageButton(m_tRedo->pSurface, ImVec2(20, 20), ImVec2(m_timeRedo > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_timeRedo > EDevice->TimerAsync() ? 1 : 0.5, 1), 0))
        {
            m_timeRedo = EDevice->TimerAsync() + 130;
            ClickRedo();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Repeat the last action.");
        }
        ImGui::SameLine();

        m_tSaveParticles->Load();
        if (ImGui::ImageButton(m_tSaveParticles->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
        {
            ClickSaveParticles();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Save the particles in unpacked form, in 'rawdata/particles'");
        }
        ImGui::SameLine();

        m_tReloadParticles->Load();
        if (ImGui::ImageButton(m_tReloadParticles->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
        {
            ClickReloadParticles();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Reload particles from 'rawdata/particles'");
        }
        ImGui::SameLine();

        m_tOpen->Load();
        if (ImGui::ImageButton(m_tOpen->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
        {
            ClickOpen();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Open file .xr");
        }
        ImGui::SameLine();

        m_tSaveXr->Load();
        if (ImGui::ImageButton(m_tSaveXr->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
        {
            ClickSaveXr();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Save file .xr");
        }
        ImGui::SameLine();

        m_tOpenGameData->Load();
        if (ImGui::ImageButton(m_tOpenGameData->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
        {
            ClickOpenGameData();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Open folder 'GameData'");
        }
        ImGui::SameLine();

        m_tValidate->Load();
        if (ImGui::ImageButton(m_tValidate->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
        {
            ClickValidate();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Particle Validation.");
        }
    }
    ImGui::End();
    ImGui::PopStyleVar(5);
}

void UITopBarForm::ClickUndo()
{
    ExecCommand(COMMAND_UNDO);
}

void UITopBarForm::ClickRedo()
{
    ExecCommand(COMMAND_REDO);
}

void UITopBarForm::ClickSaveParticles()
{
    ExecCommand(COMMAND_SAVE);
}
void UITopBarForm::ClickReloadParticles()
{
    ExecCommand(COMMAND_LOAD);
}

void UITopBarForm::ClickOpen()
{
    ExecCommand(COMMAND_LOAD_XR);
}
void UITopBarForm::ClickSaveXr()
{
    ExecCommand(COMMAND_SAVE_XR);
}

void UITopBarForm::ClickOpenGameData()
{
    string_path GameDataPath;
    FS.update_path(GameDataPath, "$game_data$", "");
    ShellExecuteA(NULL, "open", GameDataPath, NULL, NULL, SW_SHOWDEFAULT);
}

void UITopBarForm::ClickValidate()
{
    ExecCommand(COMMAND_VALIDATE);
}
