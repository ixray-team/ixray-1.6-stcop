// LevelEditor.cpp : Определяет точку входа для приложения.
//
#include "stdafx.h"

#include "Engine/XrGameManager.h"
#include "Engine/XRayEditor.h"

#include "Editor/Utils/ContentView.h"
#include "Editor/Utils/GitIntegration.h"
#include "Editor/Utils/GitLFSConfig.h"
#include "Editor/Scene/LEPhysics.h"
#include "Nodes/UIDialogsView.h"
#include "ParticleEditor/UI_ParticleTools.h"
#include "ParticleEditor/UIParticleEditorForm.h"
#include "ActorEditor/ActorEditor.h"
#include "ShaderEditor/UI_shadertools.h"
#include "ShaderEditor/UIShaderEditorForm.h"
#include "UI/LayoutEditor/UILayoutEditorForm.h"
#include "UI/Coder/UICoderForm.h"
#include "Nodes/Logic/UILogicEditorForm.h"
#include "../xrECore/Editor/UILogForm.h"
#include "../xrECore/Editor/UIEditLightAnim.h"
#include "../xrECore/Editor/UIMinimapEditorForm.h"



#include "../../xrCore/Kernel/TaskbarProgress.h"

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
void DragPreview(const xr_string*, int);

static DialogEditor* g_DialogEditor = nullptr;

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, char* pCmdLine, int nCmdShow)
{
	bIsLevelEditor = true;

	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS))
	{
		Msg("! SDL_Init Error: %s", SDL_GetError());
		return 0;
	}



	CRenderDevice::SetStartupStatus(5, "Initializing Debugger");

	Debug._initialize(false);
	
	CRenderDevice::SetStartupStatus(10, "Initializing Core System");

	const char* FSName = "fs.ltx";
	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(pCmdLine, fsgame_ltx_name)) {
		int						sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(pCmdLine, fsgame_ltx_name) + sz, "%519[^ ] ", fsgame);
	}

	CFilewatcher::instance().SetFilewatcherActive(true);
	Core._initialize("LevelEditor", ELogCallback, 1, fsgame[0] ? fsgame : FSName);

	CRenderDevice::SetStartupStatus(20, "Initializing Level Tools");

	Tools = new CLevelTool();
	LTools = static_cast<CLevelTool*>(Tools);

	CRenderDevice::SetStartupStatus(25, "Registering UI Commands");

	EContext.UI = new CLevelMain();
	EContext.UI->RegisterCommands();
	EContext.UI->GeneralTabs.push_back({ICON_FA_MOUNTAIN " Scene View##scene_view", []()->bool {return Scene->IsUnsaved(); }});
	EContext.UI->GeneralTabs.push_back({ICON_FA_COMMENT_DOTS " Dialogs", nullptr});
	EContext.UI->GeneralTabs.push_back({ICON_FA_LIGHTBULB " Light Animations", nullptr});
	EContext.UI->GeneralTabs.push_back({ICON_FA_MAP " Minimap", nullptr});
	EContext.UI->GeneralTabs.push_back({ICON_FA_FIRE " Particles", []()->bool {return PTools != nullptr && PTools->IsModified(); }});
	const int ParticleEditorTab = (int)EContext.UI->GeneralTabs.size() - 1;
	EContext.UI->GeneralTabs.push_back({ICON_FA_PERSON " Actors", []()->bool {return ATools != nullptr && ATools->IsModified(); }});
	const int ActorEditorTab = (int)EContext.UI->GeneralTabs.size() - 1;
	EContext.UI->GeneralTabs.push_back({ICON_FA_PALETTE " Shaders", []()->bool {return STools != nullptr && STools->IsModified(); }});
	const int ShaderEditorTab = (int)EContext.UI->GeneralTabs.size() - 1;
	EContext.UI->GeneralTabs.push_back({ICON_FA_OBJECT_GROUP " UI Layout", []()->bool {return UILayoutEditorForm::IsModified(); }});
	const int LayoutEditorTab = (int)EContext.UI->GeneralTabs.size() - 1;
	EContext.UI->GeneralTabs.push_back({ICON_FA_CODE " Coder", []()->bool {return UICoderForm::IsModified(); }});
	const int CoderTab = (int)EContext.UI->GeneralTabs.size() - 1;
	EContext.UI->GeneralTabs.push_back({ICON_FA_DIAGRAM_PROJECT " Logic", []()->bool {return UILogicEditorForm::IsModified(); }});
	const int LogicTab = (int)EContext.UI->GeneralTabs.size() - 1;

	LUI = static_cast<CLevelMain*>(EContext.UI);

	CRenderDevice::SetStartupStatus(30, "Creating Editor Scene");

	Scene = new EScene();
	EditorScene = Scene;

	CRenderDevice::SetStartupStatus(25, "Initializing Content View");

	GContentView = new CContentView;
	GContentView->SetStateName("ContentBrowser");

	CRenderDevice::SetStartupStatus(26, "Initializing Git Integration");

	static CGitIntegration GitIntegration;
	GitIntegration.Initialize();
	CGitLFSConfig::Instance().Load();

	CRenderDevice::SetStartupStatus(30, "Creating Main UI Form");

	UIMainForm* MainForm = new UIMainForm();

	pApp = new XRayEditor();
	g_XrGameManager = new XrGameManager();
	g_SEFactoryManager = new XrSEFactoryManager();

	CRenderDevice::SetStartupStatus(40, "Loading Game Materials");

	// Initialize APP
	GameMaterialLibraryEditors->Load();

	CRenderDevice::SetStartupStatus(55, "Initializing Game Persistent Objects");

	g_pGamePersistent = static_cast<IGame_Persistent*>(g_XrGameManager->Create(CLSID_GAME_PERSISTANT));
	EDevice->seqAppStart.Process<&pureAppStart::OnAppStart>();
	g_pGamePersistent->Environment().EnsureMixer();

	CRenderDevice::SetStartupStatus(65, "Setting Up Console");

	Console->Execute("default_controls");

	xr_strcpy(Console->ConfigFile, "user_editor.ltx");

	if (strstr(Core.Params, "-ltx ")) {
		string64 c_name = {};
		sscanf(strstr(Core.Params, "-ltx ") + 5, "%63[^ ] ", c_name);
		xr_strcpy(Console->ConfigFile, c_name);
	}

	Console->ExecuteScript(Console->ConfigFile);

	Console->Hide();
	g_lighting_mode = ELightingMode::Dynamic;

	CRenderDevice::SetStartupStatus(75, "Performing Final UI Setup");

	::MainForm = MainForm;
	MainForm->TabIndex = -1;
	EContext.UI->Push(MainForm, false);

	g_DialogEditor = new DialogEditor();
	g_DialogEditor->TabIndex = 1;
	g_DialogEditor->Show(true);
	EContext.UI->Push(g_DialogEditor, false);

	UIParticleEditorForm::Create(ParticleEditorTab);
	UIActorEditorForm::Create(ActorEditorTab);
	UIShaderEditorForm::Create(ShaderEditorTab);
	UILayoutEditorForm::Create(LayoutEditorTab);
	UICoderForm::Create(CoderTab);
	UILogicEditorForm::Create(LogicTab);
	// The Logic tab shows its own Message Log
	UILogForm::HideFunctor = &UILogicEditorForm::IsActive;

	pFPSCounter = new XRay::Hardware::FPSCounter();
	UIEditLightAnim::Show();
	UIMinimapEditorForm::Show();

	bool NeedExit = false;
	CRenderDevice::SetStartupStatus(85, "Performing Final Checks");
	MainForm->GetRenderForm()->DragFunctor = DragDrop;
	MainForm->GetRenderForm()->DragPreviewFunctor = DragPreview;
	
	CRenderDevice::SetStartupStatus(90, "Finalizing UI Setup");
	GContentView->Init();
	EContext.UI->PushBegin(GContentView);

	// An .object passed on the command line opens in the Actor Editor tab
	{
		int ArgsCount = 0;
		LPWSTR* Args = CommandLineToArgvW(GetCommandLineW(), &ArgsCount);
		if (Args != nullptr)
		{
			if (ArgsCount > 1)
			{
				xr_string FirstArg = Platform::UTF8_to_CP1251(Platform::TCHAR_TO_ANSI_U8(Args[1]));
				if (FirstArg.ends_with(".object"))
				{
					UIActorEditorForm::Open(FirstArg.c_str());
				}
			}
			LocalFree(Args);
		}
	}

	CRenderDevice::SetStartupStatus(95, "Baking Lua API");
	UICoderForm::BakeIfMissing();

	CRenderDevice::FinishStartup();
	SDL_RaiseWindow(g_AppInfo.Window);
	// The startup screen polls the events: SDL_EVENT_WINDOW_SHOWN never reaches the loop below
	Device.b_is_Active = true;
	
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
				if (EContext.UI && REDevice && Event.window.windowID == MainWndID)
				{
					if (Event.window.data1 != DevicePtr->Width || Event.window.data2 != DevicePtr->Height)
					{
						EContext.UI->Resize(Event.window.data1, Event.window.data2, true);
						EPrefs->SaveConfig();
					}
				}
				break;
			}
			case SDL_EVENT_WINDOW_SHOWN:
			case SDL_EVENT_WINDOW_FOCUS_GAINED:
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
				if (EContext.UI)
				{
					EContext.UI->KeyDown(Event.key.scancode, EContext.UI->GetShiftState());
					EContext.UI->ApplyShortCutInput(Event.key.scancode);

					if (EContext.UI->IsPlayInEditor())
					{
						if (pInput->IsAcquire)
						{
							pInput->KeyboardButtonUpdate(Event.key.scancode, true);
						}
						else if (Event.key.scancode == SDL_SCANCODE_LALT)
						{
							pInput->acquire();
							EContext.UI->IsEnableInput = false;
							ShowCursor(false);
						}
					}
				}break;
			case SDL_EVENT_KEY_UP:
				if (EContext.UI)
				{
					EContext.UI->KeyUp(Event.key.scancode, EContext.UI->GetShiftState());
					if (EContext.UI->IsPlayInEditor() && pInput->IsAcquire) 
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
				if (EContext.UI->IsPlayInEditor() && !pInput->IsAcquire)
				{
					break;
				}
				pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
			} break;
			case SDL_EVENT_MOUSE_WHEEL:
			{
				if (EContext.UI->IsPlayInEditor() && !pInput->IsAcquire)
				{
					break;
				}
				pInput->MouseScroll(Event.wheel.y);
			}break;
			case SDL_EVENT_DROP_FILE:
			{
				xr_string File = Event.drop.data;
				xr_strlwr(File);
				UIActorEditorForm::DropFile(File);
			}break;
			case SDL_EVENT_MOUSE_BUTTON_DOWN:
			case SDL_EVENT_MOUSE_BUTTON_UP:
			{
				if (EContext.UI->IsPlayInEditor() && !pInput->IsAcquire)
				{
					break;
				}
				int mouse_button = 0;
				if (Event.button.button == SDL_BUTTON_LEFT)
				{
					mouse_button = 0;
				}
				if (Event.button.button == SDL_BUTTON_RIGHT)
				{
					mouse_button = 1;
				}
				if (Event.button.button == SDL_BUTTON_MIDDLE)
				{
					mouse_button = 2;
				}
				if (Event.type == SDL_EVENT_MOUSE_BUTTON_DOWN)
				{
					pInput->MousePressed(mouse_button);
				}
				else
				{
					pInput->MouseReleased(mouse_button);
				}
			}
			break;
			}

			if (!EContext.UI->ProcessEvent(&Event))
			{
				break;
			}
		}

		MainForm->Frame();
		LUI->UpdateTaskbar();

		if (g_pGamePersistent)
			g_pGamePersistent->UpdateParticles();
	}
	//s.join();

	CTaskbarProgress::Instance().Release();

	// Windows are destroyed before the preferences are saved: keep their full state
	CEditorWndStates::Get().Save();

	// Gives Tools back to the level tool before COMMAND_DESTROY
	UIActorEditorForm::Destroy();
	UIShaderEditorForm::Destroy();
	UILayoutEditorForm::Destroy();
	UICoderForm::Destroy();
	UILogicEditorForm::Destroy();

	// The shared content browser icons hold textures: release them before the device
	CContentView::ClearIcons();
	
	CGitLFSConfig::Instance().Save();
	GitIntegration.Shutdown();
	
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