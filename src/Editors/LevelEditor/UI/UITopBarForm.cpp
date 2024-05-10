#include "stdafx.h"
#include "UITopBarForm.h"

UITopBarForm::UITopBarForm()
{
	m_tUndo = EDevice->Resources->_CreateTexture("ed\\bar\\Undo"); m_timeUndo = 0;
	m_tRedo = EDevice->Resources->_CreateTexture("ed\\bar\\Redo"); m_timeRedo = 0;
	m_tNew = EDevice->Resources->_CreateTexture("ed\\bar\\new");
	m_tOpen = EDevice->Resources->_CreateTexture("ed\\bar\\open");
	m_tSave= EDevice->Resources->_CreateTexture("ed\\bar\\save");
	m_tCForm = EDevice->Resources->_CreateTexture("ed\\bar\\CForm");
	m_tAIMap = EDevice->Resources->_CreateTexture("ed\\bar\\AIMap");
	m_tGGraph = EDevice->Resources->_CreateTexture("ed\\bar\\GGraph");
	m_tPlayInEditor = EDevice->Resources->_CreateTexture("ed\\bar\\play_in_editor");
	m_tPlayPC = EDevice->Resources->_CreateTexture("ed\\bar\\play_pc");
	m_tBuildAndMake = EDevice->Resources->_CreateTexture("ed\\bar\\build_all");
	m_tPlayCleanGame= EDevice->Resources->_CreateTexture("ed\\bar\\play_clean_game");
	m_tTerminated = EDevice->Resources->_CreateTexture("ed\\bar\\terminated");

	m_tReloadConfigs = EDevice->Resources->_CreateTexture("ed\\bar\\reload_configs");
	m_tOpenGameData = EDevice->Resources->_CreateTexture("ed\\bar\\open_gamedata");
	m_VerifySpaceRestrictors = false;
	RefreshBar();
}

UITopBarForm::~UITopBarForm()
{
	
}

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
		if (ImGui::ImageButton(m_tUndo->surface_get(), ImVec2(20, 20), ImVec2(m_timeUndo > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_timeUndo > EDevice->TimerAsync() ? 1 : 0.5, 1), 0))
		{
			m_timeUndo = EDevice->TimerAsync() + 130;
			ClickUndo();
		}ImGui::SameLine();
		m_tRedo->Load();
		if (ImGui::ImageButton(m_tRedo->surface_get(), ImVec2(20, 20), ImVec2(m_timeRedo > EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_timeRedo > EDevice->TimerAsync() ? 1 : 0.5, 1), 0))
		{
			m_timeRedo = EDevice->TimerAsync() + 130;
			ClickRedo();
		}ImGui::SameLine();

		m_tNew->Load();
		if (ImGui::ImageButton(m_tNew->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickNew();
		}ImGui::SameLine();
		m_tOpen->Load();
		if (ImGui::ImageButton(m_tOpen->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickOpen();
		}ImGui::SameLine();
		m_tSave->Load();
		if (ImGui::ImageButton(m_tSave->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickSave();
		}ImGui::SameLine();

		m_tCForm->Load();
		if (ImGui::ImageButton(m_tCForm->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickCForm();
		}ImGui::SameLine();
		m_tAIMap->Load();
		if (ImGui::ImageButton(m_tAIMap->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickAIMap();
		}ImGui::SameLine();
		m_tGGraph->Load();
		if (ImGui::ImageButton(m_tGGraph->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickGGraph();
		}ImGui::SameLine();



		if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
		{
			m_tTerminated->Load();
			if (ImGui::ImageButton(m_tTerminated->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
			{
				ClickTerminated();
			}
		}
		else
		{
			m_tPlayInEditor->Load();
			if (ImGui::ImageButton(m_tPlayInEditor->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
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
		if (ImGui::ImageButton(m_tReloadConfigs->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickReloadConfigs();
		}ImGui::SameLine();

		m_tBuildAndMake->Load();
		if (ImGui::ImageButton(m_tBuildAndMake->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickBuildAndMake();
		}
		ImGui::SameLine();
		m_tPlayPC->Load();

		if (ImGui::ImageButton(m_tPlayPC->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickPlayPC();
		}
		ImGui::SameLine();
		m_tPlayCleanGame->Load();
		if (ImGui::ImageButton(m_tPlayCleanGame->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickPlayCleanGame();
		}
		ImGui::SameLine();

		
		if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
		{
			ImGui::EndDisabled();
		}

		m_tOpenGameData->Load();
		if (ImGui::ImageButton(m_tOpenGameData->surface_get(), ImVec2(20, 20), ImVec2(0, 0), ImVec2(1, 1), 0))
		{
			ClickOpenGameData();
		}
	}
	ImGui::SameLine(0,1);
	ImGui::End();
	ImGui::PopStyleVar(5);
	
}
void UITopBarForm::RefreshBar()
{/*
	{
		m_bSelect = false;
		m_bAdd = false;

		switch (Tools->GetAction()) {
		case etaSelect: 	m_bSelect = true; 	break;
		case etaAdd:    m_bAdd = true; 		break;
		default: THROW;
		}
	}
	// settings
	m_bCsLocal = Tools->GetSettings(etfCSParent);
	m_bNuScale = Tools->GetSettings(etfNUScale);
	m_bNSnap = Tools->GetSettings(etfNormalAlign);
	m_bGSnap = Tools->GetSettings(etfGSnap);
	m_bOSnap = Tools->GetSettings(etfOSnap);
	m_bMoveToSnap = Tools->GetSettings(etfMTSnap);
	m_bVSnap = Tools->GetSettings(etfVSnap);
	m_bASnap = Tools->GetSettings(etfASnap);
	m_bMSnap = Tools->GetSettings(etfMSnap);*/
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
/*
void UITopBarForm::ClickZoom()
{
	ExecCommand(COMMAND_ZOOM_EXTENTS, FALSE);
}

void UITopBarForm::ClickZoomSel()
{
	ExecCommand(COMMAND_ZOOM_EXTENTS, TRUE);
}
void  UITopBarForm::ClickSelect()
{
	ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
	m_bSelect = true;
	m_bAdd = false;
}
void  UITopBarForm::ClickAdd()
{
	ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
	m_bSelect = false;
	m_bAdd = true;
}

void  UITopBarForm::ClickCsLocal(){ ExecCommand(COMMAND_SET_SETTINGS, etfCSParent, m_bCsLocal); }
void  UITopBarForm::ClickNuScale() { ExecCommand(COMMAND_SET_SETTINGS, etfNUScale, m_bNuScale); }
void  UITopBarForm::ClickGSnap() { ExecCommand(COMMAND_SET_SETTINGS, etfGSnap, m_bGSnap); }
void  UITopBarForm::ClickOSnap() { ExecCommand(COMMAND_SET_SETTINGS, etfOSnap, m_bOSnap); }
void  UITopBarForm::ClickMoveToSnap() { ExecCommand(COMMAND_SET_SETTINGS, etfMTSnap, m_bMoveToSnap); }
void  UITopBarForm::ClickNSnap() { ExecCommand(COMMAND_SET_SETTINGS, etfNormalAlign, m_bNSnap); }
void  UITopBarForm::ClickVSnap() { ExecCommand(COMMAND_SET_SETTINGS, etfVSnap, m_bVSnap); }
void  UITopBarForm::ClickASnap() { ExecCommand(COMMAND_SET_SETTINGS, etfASnap, m_bASnap); }
void  UITopBarForm::ClickMSnap() { ExecCommand(COMMAND_SET_SETTINGS, etfMSnap, m_bMSnap); }

void  UITopBarForm::ClickCameraP(){ EDevice->m_Camera.SetStyle(csPlaneMove); UI->RedrawScene();}
void  UITopBarForm::ClickCameraA(){ EDevice->m_Camera.SetStyle(cs3DArcBall); UI->RedrawScene();}
void  UITopBarForm::ClickCameraF(){ EDevice->m_Camera.SetStyle(csFreeFly); UI->RedrawScene();}

void  UITopBarForm::ClickViewB1() { EDevice->m_Camera.ViewBack(); UI->RedrawScene(); }
void  UITopBarForm::ClickViewB2() { EDevice->m_Camera.ViewBottom();UI->RedrawScene(); }
void  UITopBarForm::ClickViewF() { EDevice->m_Camera.ViewFront(); UI->RedrawScene();}
void  UITopBarForm::ClickViewL() { EDevice->m_Camera.ViewLeft();UI->RedrawScene(); }
void  UITopBarForm::ClickViewR() { EDevice->m_Camera.ViewRight(); UI->RedrawScene();}
void  UITopBarForm::ClickViewT() { EDevice->m_Camera.ViewTop();UI->RedrawScene(); }
void  UITopBarForm::ClickViewX() { EDevice->m_Camera.ViewReset(); UI->RedrawScene();}*/