// Level Editor entry point.
//
#include "stdafx.h"

#include "Engine/XrGameManager.h"
#include "Engine/XRayEditor.h"

#include "Editor/Utils/ContentView.h"
#include "Editor/Utils/GitIntegration.h"
#include "Editor/Utils/GitLFSConfig.h"
#include "Editor/Scene/LEPhysics.h"
#include "Nodes/UINodesEditorForm.h"
#include "Nodes/UIMacroView.h"
#include "ParticleEditor/UI_ParticleTools.h"
#include "ParticleEditor/UIParticleEditorForm.h"
#include "ActorEditor/ActorEditor.h"
#include "ShaderEditor/UI_shadertools.h"
#include "ShaderEditor/UIShaderEditorForm.h"
#include "UI/LayoutEditor/UILayoutEditorForm.h"
#include "UI/Coder/UICoderForm.h"
#include "Editor/XVisor/XVisor.h"
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

#include "IconsFontAwesome7.h"

#include <fstream>
#include <nlohmann/json.hpp>
#include <shellapi.h>

ECORE_API extern bool bIsLevelEditor;
void DragDrop(const xr_string&, int);
void DragPreview(const xr_string*, int);


namespace SdkMacro
{
	constexpr ULONG_PTR SdkMacroMessage = 0x4958524d;
	constexpr wchar_t SdkMacroWindowProperty[] = L"IXRay.SdkMacro.v1";
	WNDPROC g_EditorWindowProc = nullptr;
	std::filesystem::path g_EditorSdkConfig;
	nlohmann::json g_PendingMacro;
	bool g_RunningMacro = false;

	LRESULT CALLBACK SdkMacroWindowProc(HWND Window, UINT Message, WPARAM First, LPARAM Second)
	{
		if (Message == WM_COPYDATA)
		{
			const auto* Copy = reinterpret_cast<const COPYDATASTRUCT*>(Second);
			if (Copy && Copy->dwData == SdkMacroMessage && Copy->lpData && Copy->cbData <= 65536)
			{
				const auto* Start = static_cast<const char*>(Copy->lpData);
				auto Request = nlohmann::json::parse(Start, Start + Copy->cbData, nullptr, false);
				if (!Request.is_array() || Request.size() != 3 ||
					!Request[0].is_string() || !Request[1].is_string() || !Request[2].is_string())
				{
					return 0;
				}

				std::error_code Error;
				if (!std::filesystem::equivalent(g_EditorSdkConfig,
					std::filesystem::u8path(Request[0].get_ref<const std::string&>()), Error))
				{
					return 2;
				}
				if (g_RunningMacro || !g_PendingMacro.is_null())
				{
					return 3;
				}
				g_PendingMacro = std::move(Request);
				return 1;
			}
		}
		return CallWindowProcW(g_EditorWindowProc, Window, Message, First, Second);
	}

	int RunMacroBatch(const wchar_t* MacroPath, const wchar_t* ResultPath, bool KeepOpen)
	{
		std::ofstream Output(std::filesystem::path(ResultPath), std::ios::binary | std::ios::trunc);
		if (!Output)
		{
			return 2;
		}

		const std::filesystem::path Path(MacroPath);
		nlohmann::json Response;
		const auto Extension = Path.extension().string();
		if (xr_stricmp(Extension.c_str(), ".json") == 0)
		{
			std::ifstream Input(Path, std::ios::binary);
			Response = CUIMacroView::RunBatch(nlohmann::json::parse(Input, nullptr, false));
		}
		else
		{
			const auto Macro = Path.string();
			if (Macro.size() >= sizeof(string_path))
			{
				return 2;
			}

			if (!FS.exist(Macro.c_str()))
			{
				const auto Directory = Path.parent_path().string() + "\\";
				FS.append_path("$sdk_macro_batch$", Directory.c_str(), nullptr, false);
			}
			auto Result = ExecCommand(COMMAND_RUN_MACRO, xr_string(Macro.c_str()), u32(1));
			if (Result.IsInteger())
			{
				Response = {{"success", u32(Result) != 0}, {"result", u32(Result)}};
			}
			else
			{
				Response = {{"success", true}, {"result", xr_string(Result).c_str()}};
			}
		}
		if (KeepOpen)
		{
			Response["editor_pid"] = GetCurrentProcessId();
		}
		const auto& LogPath = xrLogger::GetLogPath();
		if (LogPath[0])
		{
			Response["log_path"] = LogPath;
		}
		xrLogger::FlushLog();
		Output << Response.dump(-1, ' ', true, nlohmann::json::error_handler_t::replace) << '\n';
		Output.close();
		return Output ? (Response["success"].get<bool>() ? 0 : 1) : 2;
	}
}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, char* pCmdLine, int nCmdShow)
{
	xr_vector<std::wstring> Arguments;
	{
		int ArgumentCount = 0;
		wchar_t** const RawArguments = CommandLineToArgvW(GetCommandLineW(), &ArgumentCount);
		if (RawArguments)
		{
			const xr_scope_exit FreeArguments([RawArguments] { LocalFree(RawArguments); });
			Arguments.assign(RawArguments, RawArguments + ArgumentCount);
		}
	}
	const xr_wstring_view Mode = Arguments.size() > 1 ? xr_wstring_view(Arguments[1]) : L"";
	const bool KeepOpen = Mode == L"--open-sdk-macro";
	const bool MacroBatch = KeepOpen || Mode == L"--run-sdk-macro";
	if (MacroBatch && Arguments.size() != 4 && Arguments.size() != 6)
	{
		return 2;
	}
	if (MacroBatch && Arguments.size() == 6 && Arguments[4] != L"-fsltx")
	{
		return 2;
	}

	bIsLevelEditor = true;

	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS))
	{
		Msg("! SDL_Init Error: %s", SDL_GetError());
		return 0;
	}



	CRenderDevice::SetStartupStatus(5, "Initializing Debugger");

	Debug._initialize(false);
	if (MacroBatch)
	{
		ignore_error_window = true;
	}
	
	CRenderDevice::SetStartupStatus(10, "Initializing Core System");

	const char* FSName = "fs.ltx";
	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (MacroBatch && Arguments.size() == 6)
	{
		const auto Config = std::filesystem::path(Arguments[5]).string();
		if (Config.size() >= sizeof(fsgame))
		{
			return 2;
		}
		xr_strcpy(fsgame, Config.c_str());
	}
	else if (!MacroBatch && strstr(pCmdLine, fsgame_ltx_name)) {
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
	// Light Animations and Minimap keep the tabs 2 and 3
	EContext.UI->GeneralTabs.push_back({ICON_FA_DIAGRAM_PROJECT " Nodes", []()->bool {return UINodesEditorForm::IsModified(); }});
	const int NodesTab = (int)EContext.UI->GeneralTabs.size() - 1;
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

	EContext.Visor = new CXVisor();
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

	UIParticleEditorForm::Create(ParticleEditorTab);
	UIActorEditorForm::Create(ActorEditorTab);
	UIShaderEditorForm::Create(ShaderEditorTab);
	UILayoutEditorForm::Create(LayoutEditorTab);
	UICoderForm::Create(CoderTab);
	UINodesEditorForm::Create(NodesTab);
	// The Nodes tab shows its own Message Log
	UILogForm::HideFunctor = []() { return UINodesEditorForm::IsActive(); };

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
	if (Arguments.size() > 1)
	{
		xr_string FirstArg = Platform::UTF8_to_CP1251(Platform::TCHAR_TO_ANSI_U8(Arguments[1].c_str()));
		if (FirstArg.ends_with(".object"))
		{
			UIActorEditorForm::Open(FirstArg.c_str());
		}
	}

	CRenderDevice::SetStartupStatus(95, "Baking Lua API");
	UICoderForm::BakeIfMissing();

	CRenderDevice::FinishStartup();
	SDL_RaiseWindow(g_AppInfo.Window);
	// The startup screen polls the events: SDL_EVENT_WINDOW_SHOWN never reaches the loop below
	Device.b_is_Active = true;
	
	const int ExitCode = MacroBatch ? SdkMacro::RunMacroBatch(Arguments[2].c_str(), Arguments[3].c_str(), KeepOpen) : 0;
	if (MacroBatch && (!KeepOpen || ExitCode != 0))
	{
		GContentView->Destroy();
		NeedExit = true;
	}
	if (KeepOpen && ExitCode == 0)
	{
		ignore_error_window = false;
	}
	const auto Window = static_cast<HWND>(SDL_GetPointerProperty(
		SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr));
	if (!NeedExit && Window)
	{
		std::error_code Error;
		SdkMacro::g_EditorSdkConfig = std::filesystem::absolute(fsgame[0] ? fsgame : FSName, Error);
		if (!Error)
		{
			SdkMacro::g_EditorWindowProc = reinterpret_cast<WNDPROC>(SetWindowLongPtrW(Window, GWLP_WNDPROC,
				reinterpret_cast<LONG_PTR>(SdkMacro::SdkMacroWindowProc)));
			if (SdkMacro::g_EditorWindowProc)
			{
				SetPropW(Window, SdkMacro::SdkMacroWindowProperty, reinterpret_cast<HANDLE>(1));
			}
		}
	}
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

		if (!NeedExit && !SdkMacro::g_PendingMacro.is_null() && !LUI->LoaderEvent.IsBusy() &&
			EContext.UI->GetEState() == esEditScene)
		{
			const auto Request = std::exchange(SdkMacro::g_PendingMacro, nullptr);
			const auto Macro = std::filesystem::u8path(Request[1].get_ref<const std::string&>());
			const auto Result = std::filesystem::u8path(Request[2].get_ref<const std::string&>());
			std::error_code Error;
			if (std::filesystem::is_directory(Result.parent_path(), Error))
			{
				SdkMacro::g_RunningMacro = true;
				SdkMacro::RunMacroBatch(Macro.c_str(), Result.c_str(), true);
				SdkMacro::g_RunningMacro = false;
				SDL_RaiseWindow(g_AppInfo.Window);
			}
		}
		MainForm->Frame();
		LUI->UpdateTaskbar();

		if (g_pGamePersistent)
			g_pGamePersistent->UpdateParticles();
	}
	//s.join();

	if (SdkMacro::g_EditorWindowProc)
	{
		RemovePropW(Window, SdkMacro::SdkMacroWindowProperty);
		SetWindowLongPtrW(Window, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(SdkMacro::g_EditorWindowProc));
	}
	CTaskbarProgress::Instance().Release();

	// Windows are destroyed before the preferences are saved: keep their full state
	CEditorWndStates::Get().Save();

	// Gives Tools back to the level tool before COMMAND_DESTROY
	UIActorEditorForm::Destroy();
	UIShaderEditorForm::Destroy();
	UILayoutEditorForm::Destroy();
	UICoderForm::Destroy();
	UINodesEditorForm::Destroy();
	xr_delete(EContext.Visor);

	// The shared content browser icons hold textures: release them before the device
	CContentView::ClearIcons();
	
	CGitLFSConfig::Instance().Save();
	GitIntegration.Shutdown();
	
	xr_delete(g_FontManager);

	g_scene_physics.DestroyAll();
	g_scene_physics.DestroyObjectSpace();

	xr_delete(MainForm);

	// Release the string table.
	CStringTable::Destroy();

	xr_delete(pApp);
	xr_delete(g_XrGameManager);
	xr_delete(g_SEFactoryManager);

	Core._destroy();
	return ExitCode;
}
