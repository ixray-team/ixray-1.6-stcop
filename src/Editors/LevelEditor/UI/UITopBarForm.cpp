#include "stdafx.h"
#include "UITopBarForm.h"
#include <shellapi.h>
#include "IconsFontAwesome6.h"
#include <imgui_internal.h>

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
    m_VerifySpaceRestrictors = false;
    m_Simulate               = false;
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

#define IMGUI_HINT_BUTTON_EX(Name, Ptr, Timer, Hint, Callback) \
			Ptr->Load(); \
			if (ImGui::ImageButton("##" Name, Ptr->get_SRView()->GetRawSRV(), ImVec2(20, 20), ImVec2(Timer > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(Timer > EDevice->TimerAsync() ? 1 : 0.5, 1))) \
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
	ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, 28));
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
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(2, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(-2, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(6, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);
	ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.f, 0.f, 0.f, 0.f));

	if (ImGui::Begin("TOOLBAR", NULL, window_flags))
	{
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 6);

		if (ImGui::BeginTable("##ToolbarTable", 11, ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_ContextMenuInBody | ImGuiTableFlags_Hideable))
		{
			ImGui::TableSetupColumn("Actions");
			ImGui::TableSetupColumn("File");
			ImGui::TableSetupColumn("PIE Pre-Build");
			ImGui::TableSetupColumn("PIE Actions");
			ImGui::TableSetupColumn("Compile Actions");
			ImGui::TableSetupColumn("Engine");
			ImGui::TableSetupColumn("Directory Actions");
			ImGui::TableSetupColumn("Hint");
			ImGui::TableSetupColumn("Sound Preferences");
			ImGui::TableSetupColumn("Physics");
			ImGui::TableSetupColumn("Preferences");

			auto yMaxSize = ImGui::GetContentRegionAvail().y;

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("Actions");
				IMGUI_HINT_AF_BUTTON_EX(ICON_FA_ROTATE_LEFT, m_timeUndo, "Undo the last action", ClickUndo);
				IMGUI_HINT_AF_BUTTON_EX(ICON_FA_ROTATE_RIGHT, m_timeRedo, "Repeat the last action", ClickRedo);
				CalcTableEndPos("Actions");
			}

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("File");
				IMGUI_HINT_AF_BUTTON(ICON_FA_FILE, "Clear/New Scene", ClickNew);
				IMGUI_HINT_AF_BUTTON(ICON_FA_FILE_IMPORT, "Open level", ClickOpen);
				IMGUI_HINT_AF_BUTTON(ICON_FA_FLOPPY_DISK, "Save level", ClickSave);
				CalcTableEndPos("File");
			}

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("PIE Pre-Build");
				IMGUI_HINT_AF_BUTTON(ICON_FA_CUBE, "Build CFORM", ClickCForm);
				IMGUI_HINT_BUTTON("BuildAIMap", m_tAIMap, "Build AI-Map", ClickAIMap);
				IMGUI_HINT_AF_BUTTON(ICON_FA_DIAGRAM_PROJECT, "Build Game Graph", ClickGGraph);
				CalcTableEndPos("PIE Pre-Build");
			}

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("PIE Actions");
				if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
				{
					IMGUI_HINT_BUTTON("StopPIE", m_tTerminated, "Stop Play in Editor", ClickTerminated);
				}
				else if (Scene->IsPlayInEditor())
				{
					IMGUI_HINT_BUTTON("StopPIE", m_tTerminated, "Stop Play in Editor", Scene->Stop);
				}
				else
				{
					IMGUI_HINT_BUTTON("StartPIE", m_tPlayInEditor, "Start Play in Editor", ClickPlayInEditor);
				}

				if (ImGui::ArrowButton("##PlaySettings", ImGuiDir_Down, ImVec2(ImGui::GetFrameHeight(), 20), 0))
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
				CalcTableEndPos("PIE Actions");
			}

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("Compile Actions");
				ImGui::BeginDisabled(LTools->IsCompilerRunning() || LTools->IsGameRunning());
				IMGUI_HINT_BUTTON("ReloadCfg", m_tReloadConfigs, "Reload Configs", ClickReloadConfigs);
				IMGUI_HINT_AF_BUTTON(ICON_FA_TROWEL_BRICKS, "Build and Make", ClickBuildAndMake);
				ImGui::EndDisabled();
				CalcTableEndPos("Compile Actions");
			}

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("Engine");
				ImGui::BeginDisabled(LTools->IsCompilerRunning() || LTools->IsGameRunning());
				IMGUI_HINT_BUTTON("PlayPC", m_tPlayPC, "Play level", ClickPlayPC);
				IMGUI_HINT_BUTTON("PlayLIG", m_tPlayCleanGame, "Play level in game", ClickPlayCleanGame);
				ImGui::EndDisabled();
				CalcTableEndPos("Engine");
			}

			if (ImGui::TableNextColumn())
			{
				ApplyBackground("Directory Actions");
				IMGUI_HINT_AF_BUTTON(ICON_FA_FOLDER_OPEN, "Open 'gamedata' folder", ClickOpenGameData);
				CalcTableEndPos("Directory Actions");
			}

			if (ImGui::TableNextColumn())
			{
				//ApplyBackground("Hint");
				ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 3);
				ImGui::Checkbox("Hint ", &MainForm->GetRenderForm()->UseHint);
				//CalcTableEndPos("Hint");
			}

			if (ImGui::TableNextColumn())
			{
				//ApplyBackground("Sound Preferences");
				ImGui::BeginDisabled(psDeviceFlags.is(rsMuteSounds));
				ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 3);
				ImGui::SetNextItemWidth(150);
				ImGui::SliderFloat(!psDeviceFlags.is(rsMuteSounds) ? ICON_FA_VOLUME_HIGH : ICON_FA_VOLUME_XMARK, &EPrefs->sound_volume, 0, 1, "%.2f");
				ImGui::EndDisabled();
				//CalcTableEndPos("Sound Preferences");
			}

			if (ImGui::TableNextColumn())
			{
				//ApplyBackground("Physics");
				ImGui::SetCursorPosY(3);
				if (ImGui::Checkbox("Phys Simulation", &m_Simulate))
				{
					ExecCommand(COMMAND_SIMULATE, true);
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Activates the physics simulation of the selected objects");
				}
				ImGui::SameLine(0, 10);

				ImGui::SetCursorPosY(3);


				ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.f, 0.f, 0.f, 0.15f));
				if (ImGui::Button("Use Pos"))
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
				ApplyBackground("Preferences");
				IMGUI_HINT_AF_BUTTON(ICON_FA_SLIDERS, "Preferences", ClickPreferences);
				CalcTableEndPos("Preferences");
			}
        }
		ImGui::EndTable();
	}
	ImGui::End();
	ImGui::PopStyleColor();
	ImGui::PopStyleVar(6);
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

void UITopBarForm::ApplyBackground(const xr_string& TableColumName)
{
	if (!TableSizes.contains(TableColumName))
	{
		return;
	}

	constexpr float padding = 2.0f;
	constexpr float rounding = 6.0f;
	ImU32 Color = ImGui::GetColorU32(ImVec4(0.14f, 0.14f, 0.14f, 0.85f));

	ImVec2 TopLeft = ImGui::GetCursorScreenPos();
	TopLeft.x += padding;
	TopLeft.y += padding + 1;

	ImVec2& BottomRight = TableSizes[TableColumName];
	ImGui::GetWindowDrawList()->AddRectFilled(TopLeft, BottomRight, Color, rounding, ImDrawFlags_RoundCornersAll);
}

void UITopBarForm::CalcTableEndPos(const xr_string& TableColumName)
{
	if (TableSizes.contains(TableColumName))
	{
		return;
	}

	constexpr float padding = 2.0f;
	constexpr float rounding = 6.0f;

	ImVec2& BottomRight = TableSizes[TableColumName]; 
	BottomRight = ImGui::GetItemRectMax();
	BottomRight.x -= padding;
	BottomRight.y -= padding * 2;

	if (HeightCell == 0)
	{
		HeightCell = BottomRight.y;
	}
	else
	{
		BottomRight.y = HeightCell;
	}
}
