#include "stdafx.h"
#include "UITopBarForm.h"
#include <shellapi.h>

UITopBarForm::UITopBarForm()
{
    m_tUndo                  = EDevice->Resources->_CreateTexture("ed\\bar\\Undo");
    m_timeUndo               = 0;
    m_tRedo                  = EDevice->Resources->_CreateTexture("ed\\bar\\Redo");
    m_timeRedo               = 0;
    m_tNew                   = EDevice->Resources->_CreateTexture("ed\\bar\\new");
    m_tOpen                  = EDevice->Resources->_CreateTexture("ed\\bar\\open");
    m_tSave                  = EDevice->Resources->_CreateTexture("ed\\bar\\save");
    m_tCForm                 = EDevice->Resources->_CreateTexture("ed\\bar\\CForm");
    m_tAIMap                 = EDevice->Resources->_CreateTexture("ed\\bar\\AIMap");
    m_tGGraph                = EDevice->Resources->_CreateTexture("ed\\bar\\GGraph");
    m_tPlayInEditor          = EDevice->Resources->_CreateTexture("ed\\bar\\play_in_editor");
    m_tPlayPC                = EDevice->Resources->_CreateTexture("ed\\bar\\play_pc");
    m_tBuildAndMake          = EDevice->Resources->_CreateTexture("ed\\bar\\build_all");
    m_tPlayCleanGame         = EDevice->Resources->_CreateTexture("ed\\bar\\play_clean_game");
    m_tTerminated            = EDevice->Resources->_CreateTexture("ed\\bar\\terminated");

    m_tReloadConfigs         = EDevice->Resources->_CreateTexture("ed\\bar\\reload_configs");
    m_tOpenGameData          = EDevice->Resources->_CreateTexture("ed\\bar\\open_gamedata");
    m_VerifySpaceRestrictors = false;
}

UITopBarForm::~UITopBarForm() {}

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
		| ImGuiWindowFlags_NoSavedSettings
		;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding,ImVec2( 2,2));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(2, 2));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 2));
	ImGui::Begin("TOOLBAR", NULL, window_flags);
	{


		m_tUndo->Load();
		if (ImGui::ImageButton(m_tUndo->pSurface, ImVec2(20, 20), ImVec2(m_timeUndo > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_timeUndo > EDevice->TimerAsync() ? 1 : 0.5, 1), 0))
		{
			m_timeUndo = EDevice->TimerAsync() + 130;
			ClickUndo();
		}ImGui::SameLine();
		m_tRedo->Load();
		if (ImGui::ImageButton(m_tRedo->pSurface, ImVec2(20, 20), ImVec2(m_timeRedo > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_timeRedo > EDevice->TimerAsync() ? 1 : 0.5, 1), 0))
		{
			m_timeRedo = EDevice->TimerAsync() + 130;
			ClickRedo();
		}ImGui::SameLine();

		m_tNew->Load();
		if (ImGui::ImageButton(m_tNew->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickNew();
		}ImGui::SameLine();
		m_tOpen->Load();
		if (ImGui::ImageButton(m_tOpen->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickOpen();
		}ImGui::SameLine();
		m_tSave->Load();
		if (ImGui::ImageButton(m_tSave->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickSave();
		}ImGui::SameLine();

		m_tCForm->Load();
		if (ImGui::ImageButton(m_tCForm->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickCForm();
		}ImGui::SameLine();
		m_tAIMap->Load();
		if (ImGui::ImageButton(m_tAIMap->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickAIMap();
		}ImGui::SameLine();
		m_tGGraph->Load();
		if (ImGui::ImageButton(m_tGGraph->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickGGraph();
		}ImGui::SameLine();



		if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
		{
			m_tTerminated->Load();
			if (ImGui::ImageButton(m_tTerminated->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
			{
				ClickTerminated();
			}
		}
		else
		{
			m_tPlayInEditor->Load();
			if (ImGui::ImageButton(m_tPlayInEditor->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
			{
				ClickPlayInEditor();
			}
		}
		{
			ImGui::SameLine(0,0);
			if (ImGui::ArrowButton("##PlaySettings", ImGuiDir_Down, ImVec2(ImGui::GetFrameHeight(), 20),0))
				ImGui::OpenPopup("test");

			ImGui::SameLine();
			if (ImGui::BeginPopup("test"))
			{
				ImGui::Checkbox("Verify space restrictors",&m_VerifySpaceRestrictors);
				ImGui::EndPopup();
			}
		}

		if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
		{
			ImGui::BeginDisabled();
		}
		ImGui::SameLine();
		m_tReloadConfigs->Load();
		if (ImGui::ImageButton(m_tReloadConfigs->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickReloadConfigs();
		}ImGui::SameLine();

		m_tBuildAndMake->Load();
		if (ImGui::ImageButton(m_tBuildAndMake->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickBuildAndMake();
		}
		ImGui::SameLine();
		m_tPlayPC->Load();

		if (ImGui::ImageButton(m_tPlayPC->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickPlayPC();
		}
		ImGui::SameLine();
		m_tPlayCleanGame->Load();
		if (ImGui::ImageButton(m_tPlayCleanGame->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickPlayCleanGame();
		}
		ImGui::SameLine();

		
		if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
		{
			ImGui::EndDisabled();
		}

		m_tOpenGameData->Load();
		if (ImGui::ImageButton(m_tOpenGameData->pSurface, ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickOpenGameData();
		}
	}
	ImGui::SameLine(0,1);
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
	pSettings = xr_new<CInifile>(si_name, TRUE);// FALSE,TRUE,TRUE);
	xr_delete(pGameIni);
	string_path					fname;
	FS.update_path(fname, "$game_config$", "game.ltx");
	pGameIni = xr_new<CInifile>(fname, TRUE);
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

	LTools->RunGame("-editor_scene -start server(editor/single/alife/new) client(localhost) -nointro -noprefetch");
}
void UITopBarForm::ClickPlayCleanGame()
{
	LTools->RunGame("");
}
