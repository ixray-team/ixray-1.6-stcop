// LevelEditor.cpp : Определяет точку входа для приложения.
//
#include "stdafx.h"

#include "Engine/XrGameManager.h"
#include "Engine/XRayEditor.h"

#include "Editor/Utils/ContentView.h"
#include "Editor/Scene/LEPhysics.h"
#include "Nodes/UIDialogsView.h"
#include "../xrECore/Editor/UIEditLightAnim.h"

#include "../../xrPlay/Splash.h"

#include "../../xrEngine/std_classes.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/XR_IOConsole.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrEngine/FPSCounter.h"

#include "IconsFontAwesome6.h"

ECORE_API extern bool bIsLevelEditor;
void DragDrop(const xr_string&, int);

static DialogEditor* g_DialogEditor = nullptr;

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, char* pCmdLine, int nCmdShow)
{
	bIsLevelEditor = true;

	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS))
	{
		Msg("! SDL_Init Error: %s", SDL_GetError());
		return 0;
	}

	splash::SetBackground(IDB_LE);
	std::jthread s(splash::Show);

	splash::SetProgressStatus(5, "Initializing Debugger");

	Debug._initialize(false);
	
	splash::SetProgressStatus(10, "Initializing Core System");

	const char* FSName = "fs.ltx";
	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(pCmdLine, fsgame_ltx_name)) {
		int						sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(pCmdLine, fsgame_ltx_name) + sz, "%[^ ] ", fsgame);
	}

	CFilewatcher::instance().SetFilewatcherActive(true);
	Core._initialize("LevelEditor", ELogCallback, 1, fsgame[0] ? fsgame : FSName);

	splash::SetProgressStatus(20, "Initializing Level Tools");

	Tools = new CLevelTool();
	LTools = static_cast<CLevelTool*>(Tools);

	splash::SetProgressStatus(25, "Registering UI Commands");

	UI = new CLevelMain();
	UI->RegisterCommands();
	UI->GeneralTabs.push_back({ICON_FA_MOUNTAIN " Scene View##scene_view", []()->bool {return Scene->IsUnsaved(); }});
	UI->GeneralTabs.push_back({ICON_FA_COMMENT_DOTS " Dialog Editor", nullptr});
	UI->GeneralTabs.push_back({ICON_FA_LIGHTBULB " Light Anim Editor", nullptr});

	LUI = static_cast<CLevelMain*>(UI);

	splash::SetProgressStatus(30, "Creating Editor Scene");

	Scene = new EScene();
	EditorScene = Scene;

	splash::SetProgressStatus(25, "Initializing Content View");

	GContentView = new CContentView;

	splash::SetProgressStatus(30, "Creating Main UI Form");

	UIMainForm* MainForm = new UIMainForm();

	pApp = new XRayEditor();
	g_XrGameManager = new XrGameManager();
	g_SEFactoryManager = new XrSEFactoryManager();

	splash::SetProgressStatus(40, "Loading Game Materials");

	// Initialize APP
	GameMaterialLibraryEditors->Load();

	splash::SetProgressStatus(55, "Initializing Game Persistent Objects");

	g_pGamePersistent = static_cast<IGame_Persistent*>(g_XrGameManager->Create(CLSID_GAME_PERSISTANT));
	EDevice->seqAppStart.Process<&pureAppStart::OnAppStart>();

	splash::SetProgressStatus(65, "Setting Up Console");

	Console->Execute("default_controls");

	xr_strcpy(Console->ConfigFile, "user_editor.ltx");

	if (strstr(Core.Params, "-ltx ")) {
		string64 c_name;
		sscanf(strstr(Core.Params, "-ltx ") + 5, "%[^ ] ", c_name);
		xr_strcpy(Console->ConfigFile, c_name);
	}

	Console->ExecuteScript(Console->ConfigFile);

	Console->Hide();

	splash::SetProgressStatus(75, "Performing Final UI Setup");

	::MainForm = MainForm;
	MainForm->TabIndex = -1;
	UI->Push(MainForm, false);

	g_DialogEditor = new DialogEditor();
	g_DialogEditor->TabIndex = 1;
	g_DialogEditor->Show(true);
	UI->Push(g_DialogEditor, false);

	pFPSCounter = new XRay::Hardware::FPSCounter();
	UIEditLightAnim::Show();

	bool NeedExit = false;
	splash::SetProgressStatus(85, "Performing Final Checks");
	MainForm->GetRenderForm()->DragFunctor = DragDrop;
	
	splash::SetProgressStatus(90, "Finalizing UI Setup");
	GContentView->Init();
	UI->PushBegin(GContentView);

	splash::SetProgressStatus(100, "Finalizing");
	splash::Close();
	while (!NeedExit)
	{
		SDL_Event Event;
		while (SDL_PollEvent(&Event))
		{
			switch (Event.type)
			{
			case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
			{
				SDL_WindowID MainWndID = SDL_GetWindowID(g_AppInfo.Window);
				if (Event.window.windowID == MainWndID)
				{
					GContentView->Destroy();
					NeedExit = true;
				}

				break;
			}
			case SDL_EVENT_WINDOW_RESIZED:
			{
				SDL_WindowID MainWndID = SDL_GetWindowID(g_AppInfo.Window);
				if (UI && REDevice && Event.window.windowID == MainWndID)
				{
					if (Event.window.data1 != DevicePtr->Width || Event.window.data2 != DevicePtr->Height)
					{
						UI->Resize(Event.window.data1, Event.window.data2, true);
						EPrefs->SaveConfig();
					}
				}
				break;
			}
			case SDL_EVENT_WINDOW_SHOWN:
			case SDL_EVENT_WINDOW_MOUSE_ENTER:
				Device.b_is_Active = true;
				//if (UI) UI->OnAppActivate();

				break;
			case SDL_EVENT_WINDOW_HIDDEN:
			case SDL_EVENT_WINDOW_MOUSE_LEAVE:
				Device.b_is_Active = !!psDeviceFlags.test(rsDeviceActive);
				//if (UI)UI->OnAppDeactivate();
				break;

			case SDL_EVENT_KEY_DOWN:
				if (UI)
				{
					UI->KeyDown(Event.key.scancode, UI->GetShiftState());
					UI->ApplyShortCutInput(Event.key.scancode);

					if (UI->IsPlayInEditor())
					{
						if (pInput->IsAcquire)
						{
							pInput->KeyboardButtonUpdate(Event.key.scancode, true);
						}
						else if (Event.key.scancode == SDL_SCANCODE_LALT)
						{
							pInput->acquire();
							UI->IsEnableInput = false;
							ShowCursor(false);
						}
					}
				}break;
			case SDL_EVENT_KEY_UP:
				if (UI) {
					UI->KeyUp(Event.key.scancode, UI->GetShiftState());
					if(UI->IsPlayInEditor() && pInput->IsAcquire) 
					{
						if (pInput->IsAcquire)
						{
							pInput->KeyboardButtonUpdate(Event.key.scancode, false);
						}
					}
				}
				break;
			case SDL_EVENT_MOUSE_MOTION:
			{
				if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					break;

				pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
			} break;
			case SDL_EVENT_MOUSE_WHEEL:
			{
				if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					break;

				pInput->MouseScroll(Event.wheel.y);
			}break;
			case SDL_EVENT_MOUSE_BUTTON_DOWN:
			case SDL_EVENT_MOUSE_BUTTON_UP:
			{
				if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					break;

				int mouse_button = 0;
				if (Event.button.button == SDL_BUTTON_LEFT) { mouse_button = 0; }
				if (Event.button.button == SDL_BUTTON_RIGHT) { mouse_button = 1; }
				if (Event.button.button == SDL_BUTTON_MIDDLE) { mouse_button = 2; }
				if (Event.type == SDL_EVENT_MOUSE_BUTTON_DOWN) {
					pInput->MousePressed(mouse_button);
				}
				else {
					pInput->MouseReleased(mouse_button);
				}
			}
			break;
			}

			if (!UI->ProcessEvent(&Event))
				break;
		}

		MainForm->Frame();

		if (g_pGamePersistent)
			g_pGamePersistent->UpdateParticles();
	}
	s.join();
	xr_delete(g_FontManager);

	g_scene_physics.DestroyAll();
	g_scene_physics.DestroyObjectSpace();

	xr_delete(MainForm);
	//очищение памяти таблицы строк
	CStringTable::Destroy();
	xr_delete(pApp);
	xr_delete(g_XrGameManager);
	xr_delete(g_SEFactoryManager);
	Core._destroy();
	return 0;
}