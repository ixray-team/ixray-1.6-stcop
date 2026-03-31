#include "stdafx.h"
#include "UITopBarForm.h"
#include <shellapi.h>
#include "IconsFontAwesome6.h"
#include "../xrECore/Editor/imgui_EditorEx.h"

UITopBarForm::UITopBarForm()
{
    m_timeUndo               = 0;
    m_timeRedo               = 0;
    m_tAIMap                 = EDevice->Resources->_CreateTexture("ed\\bar\\AIMap");
    m_tPlayInEditor          = EDevice->Resources->_CreateTexture("ed\\bar\\play_in_editor");
    m_tPlayPC                = EDevice->Resources->_CreateTexture("ed\\bar\\play_pc");
    m_tPlayCleanGame         = EDevice->Resources->_CreateTexture("ed\\bar\\play_clean_game");
    m_tTerminated            = EDevice->Resources->_CreateTexture("ed\\bar\\terminated");

    m_tReloadConfigs         = EDevice->Resources->_CreateTexture("ed\\bar\\reload_configs");
    
	InitIcons();

	m_VerifySpaceRestrictors = false;
    m_Simulate               = false;
}

UITopBarForm::~UITopBarForm() 
{
	Icons.clear();
}


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
		| ImGuiWindowFlags_NoScrollWithMouse
		;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(ToolbarPadding, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(ToolbarPadding, ToolbarPadding));
	ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);
	ImGui::PushStyleColor(ImGuiCol_Button, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ToolbarButtonTint).Value);
	ImGui::PushStyleColor(ImGuiCol_WindowBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);

	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, { ButtonSize, ButtonSize + ToolbarPadding * 2 });
	if (ImGui::Begin("TOOLBAR", NULL, window_flags))
	{
		ImGui::PopStyleVar(); // WindowMinSize
		if (ImGui::BeginTable("##ToolbarTable", 10, ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_ContextMenuInBody | ImGuiTableFlags_Hideable))
		{
			ImGui::TableSetupColumn("Actions", ImGuiTableColumnFlags_WidthFixed);
			ImGui::TableSetupColumn("File");
			ImGui::TableSetupColumn("PIE Pre-Build");
			ImGui::TableSetupColumn("PIE Actions");
			ImGui::TableSetupColumn("Compile Actions");
			ImGui::TableSetupColumn("Engine");
			ImGui::TableSetupColumn("Directory Actions");
			ImGui::TableSetupColumn("Sound Preferences");
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
				IMGUI_HINT_BUTTON("I_CNS", Icons["new_scene"],"Clear/New Scene", ImDrawFlags_RoundCornersLeft, ClickNew);
				IMGUI_HINT_BUTTON("I_OL", Icons["open_level"], "Open level", ImDrawFlags_RoundCornersNone, ClickOpen);
				IMGUI_HINT_BUTTON("I_SL", Icons["save_level"],"Save level", ImDrawFlags_RoundCornersRight, ClickSave);
			}

			if (ImGui::TableNextColumn())
			{
				IMGUI_HINT_BUTTON("BuildCFORM", Icons["build_cform"], "Build CFORM", ImDrawFlags_RoundCornersLeft, ClickCForm);
				IMGUI_HINT_BUTTON("BuildAIMap", Icons["build_ai_map"], "Build AI-Map", ImDrawFlags_RoundCornersNone, ClickAIMap);
				IMGUI_HINT_BUTTON("BuildGameGraph", Icons["build_game_graph"], "Build Game Graph", ImDrawFlags_RoundCornersRight, ClickGGraph);
			}

			if (ImGui::TableNextColumn())
			{
				if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
				{
					IMGUI_HINT_BUTTON("StopPIE", m_tTerminated, "Stop Play in Editor", ImDrawFlags_RoundCornersLeft, ClickTerminated);
				}
				else if (Scene->IsPlayInEditor())
				{
					IMGUI_HINT_BUTTON("StopPIE", m_tTerminated, "Stop Play in Editor", ImDrawFlags_RoundCornersLeft, Scene->Stop);
				}
				else
				{
					IMGUI_HINT_BUTTON("StartPIE", Icons["play_in_editor"], "Start Play in Editor", ImDrawFlags_RoundCornersLeft, ClickPlayInEditor);
				}

				Icons["play_in_editor_settings"]->Load();
				if (XRay::ImGui::ToolbarIconButton("##PlaySettings", Icons["play_in_editor_settings"]->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersRight, ButtonRadius, { ButtonSize * 0.5f, ButtonSize }, { IconSize * 0.4f, IconSize * 0.4f }))
				{
					ImGui::OpenPopup("test");
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Play in Editor settings");
				}

				ImGui::SameLine();
				if (ImGui::BeginPopup("test"))
				{
					ImGui::Checkbox("Verify space restrictors", &m_VerifySpaceRestrictors);
					ImGui::Checkbox("Apply camera pos to actor", &UseCameraPosForActor);
					ImGui::Checkbox("Build artefact spawn positions", &((CLevelPreferences*)EPrefs)->PIEArtSpawnPos);
					ImGui::EndPopup();
				}
			}

			if (ImGui::TableNextColumn())
			{
				ImGui::BeginDisabled(LTools->IsCompilerRunning() || LTools->IsGameRunning());
				IMGUI_HINT_BUTTON("ReloadCfg", Icons["reload_configs"], "Reload Configs", ImDrawFlags_RoundCornersLeft, ClickReloadConfigs);
				IMGUI_HINT_BUTTON("BuildandMake", Icons["build_and_make"], "Build and Make", ImDrawFlags_RoundCornersRight, ClickBuildAndMake);
				ImGui::EndDisabled();
			}

			if (ImGui::TableNextColumn())
			{
				ImGui::BeginDisabled(LTools->IsCompilerRunning() || LTools->IsGameRunning());
				IMGUI_HINT_BUTTON("PlayPC", Icons["play_level"], "Play level", ImDrawFlags_RoundCornersLeft, ClickPlayPC);
				IMGUI_HINT_BUTTON("PlayLIG", Icons["play_level_in_game"], "Play level in game", ImDrawFlags_RoundCornersRight, ClickPlayCleanGame);
				ImGui::EndDisabled();
			}

			if (ImGui::TableNextColumn())
			{
				IMGUI_HINT_BUTTON("I_OGF", Icons["open_gamedata_folder"],"Open 'gamedata' folder", ImDrawFlags_RoundCornersAll, ClickOpenGameData);
			}
/*
			if (ImGui::TableNextColumn())
			{
				//ApplyBackground("Hint");
				ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 3);
				ImGui::Checkbox("Hint ", &MainForm->GetRenderForm()->UseHint);
				//CalcTableEndPos("Hint");
			}
*/

			if (ImGui::TableNextColumn())
			{
				//ApplyBackground("Sound Preferences");
				ImGui::BeginDisabled(psDeviceFlags.is(rsMuteSounds));
				//ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 3);
				ImGui::SetNextItemWidth(GUIManager->ScaleByDpi(150));
				ImGui::SliderFloat(!psDeviceFlags.is(rsMuteSounds) ? ICON_FA_VOLUME_HIGH : ICON_FA_VOLUME_XMARK, &EPrefs->sound_volume, 0, 1, "%.2f");
				ImGui::EndDisabled();
				//CalcTableEndPos("Sound Preferences");
			}

			if (ImGui::TableNextColumn())
			{
				ImGui::PushStyleColor(ImGuiCol_ButtonActive, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::Accent).Value);
				//ApplyBackground("Physics");
				
				if (XRay::ImGui::ToolbarButton("PhysSimulation228","Phys Simulation", &m_Simulate, { 0.f, ButtonSize }, ImDrawFlags_RoundCornersLeft))
				//if (ImGui::Checkbox("Phys Simulation", &m_Simulate))
				{
					ExecCommand(COMMAND_SIMULATE, true);
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Activates the physics simulation of the selected objects");
				}
				ImGui::SameLine(0, 0);

				//ImGui::SetCursorPosY(3);

				if (XRay::ImGui::ToolbarButton("UsePos228", "Use Pos", nullptr, { 0.f, ButtonSize }, ImDrawFlags_RoundCornersRight))
				//if (ImGui::Button("Use Pos"))
				{
					ExecCommand(COMMAND_USE_SIMULATE_POSITIONS, true);
				}

				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Use the position of the selected object when physics simulation is active\r\nThe position of the object will be applied when simulating physics");
				}
				ImGui::PopStyleColor();
				//CalcTableEndPos("Physics");
			}

			if (ImGui::TableNextColumn())
			{
				IMGUI_HINT_BUTTON("I_Preferences", Icons["prefs"], "Preferences", ImDrawFlags_RoundCornersAll, ClickPreferences);
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
	Icons["build_cform"] = EDevice->Resources->_CreateTexture("ed\\icons\\Build CForm");
	Icons["build_ai_map"] = EDevice->Resources->_CreateTexture("ed\\icons\\Build AI-Map");
	Icons["build_game_graph"] = EDevice->Resources->_CreateTexture("ed\\icons\\Build Graph");
	Icons["play_in_editor"] = EDevice->Resources->_CreateTexture("ed\\icons\\Run PiE");
	Icons["play_in_editor_settings"] = EDevice->Resources->_CreateTexture("ed\\bar\\arrow");

	Icons["reload_configs"] = EDevice->Resources->_CreateTexture("ed\\icons\\Settings Update Configs");
	Icons["build_and_make"] = EDevice->Resources->_CreateTexture("ed\\icons\\Build and Make");
	Icons["play_level"] = EDevice->Resources->_CreateTexture("ed\\icons\\Play Level");
	Icons["play_level_in_game"] = EDevice->Resources->_CreateTexture("ed\\icons\\Play Game");
	Icons["open_gamedata_folder"] = EDevice->Resources->_CreateTexture("ed\\icons\\File Open Game Data Folder");
	Icons["prefs"] = EDevice->Resources->_CreateTexture("ed\\icons\\Tab Outliner");

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
	ExecCommand(COMMAND_SAVE, xr_string(LTools->m_LastFileName.c_str()));
}
void UITopBarForm::ClickReloadConfigs()
{
	xr_delete(pSettings);
	string_path 			si_name;
	FS.update_path(si_name, "$game_config$", "system.ltx");
	pSettings = new CInifile(si_name, TRUE);// FALSE,TRUE,TRUE);
	xr_delete(pGameIni);
	string_path					fname;
	FS.update_path(fname, "$game_config$", "game.ltx");
	pGameIni = new CInifile(fname, TRUE);
	g_SEFactoryManager->reload();
	g_pGamePersistent->OnAppEnd();
	g_pGamePersistent->OnAppStart();
	Tools->UpdateProperties();
}

void UITopBarForm::ClickOpenGameData()
{
	string_path GameDataPath;
	FS.update_path(GameDataPath, "$game_data$", "");
	ShellExecuteA(NULL, "open", GameDataPath, NULL, NULL, SW_SHOWDEFAULT);
}
void UITopBarForm::ClickCForm()
{
	Scene->BuildCForm();

}
void UITopBarForm::ClickAIMap()
{
	Scene->BuildAIMap();

}
void UITopBarForm::ClickGGraph()
{
	Scene->BuildGameGraph();

}
void UITopBarForm::ClickPlayInEditor()
{
	Scene->Play();
}
void UITopBarForm::ClickBuildAndMake()
{
	if (Builder.Compile(false,false))
	{
		LTools->RunXrLC();
	}
}
void UITopBarForm::ClickTerminated()
{
	LTools->Terminated();
}
void UITopBarForm::ClickPlayPC()
{
	if (!Scene->BuildForPCPlay())
		return;

	string_path params;
	xr_sprintf(params, "-r4 -start server(%s/single/alife/new) client(localhost) -noprefetch -nointro -fsltx fsgame_editor.ltx", Scene->m_LevelOp.m_FNLevelPath.c_str());

	LTools->RunGame(params);

}
void UITopBarForm::ClickPlayCleanGame()
{
	LTools->RunGame("-noprefetch -r4  -fsltx fsgame_editor.ltx");
}

void UITopBarForm::ClickPreferences()
{
	ExecCommand(COMMAND_EDITOR_PREF);
}