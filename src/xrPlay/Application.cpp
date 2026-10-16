#include "Application.h"
#include "Splash.h"
#include "UIEditorMain.h"

#include "../xrEngine/x_ray.h"
#include "../xrEngine/Autotest.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"

#include "../xrCore/git_version.h"

#ifdef IXR_MACOS
// AI: SDL_Vulkan_LoadLibrary(), used in BeginPlay() to bring up the Vulkan loader for the Metal
// AI: window, lives in this header and SDL.h does not pull it in.
#	include <SDL3/SDL_vulkan.h>
#endif

#ifndef DEBUG_DRAW
#	define NO_MULTI_INSTANCES
#endif

int CApplication::Run()
{
	// Check for another instance
#if defined(NO_MULTI_INSTANCES) && defined(IXR_WINDOWS)
	Platform::CMutexHandle CheckPresenceMutex = CheckSingleInstance();
	if (!CheckPresenceMutex.IsValid())
	{
		return 2;
	}
#endif

	PROF_EVENT("ENGINE_ENTRY");
	if (!BeginPlay())
	{
		return -1;
	}

	// plat
	std::jthread s(splash::Show);

	SteamWorks.BeginPlay();
	InitEngine();
	splash::Close();

	MigrateToGameWindow();
	EngineLoopAndDestroy();

	EndPlay();

	s.join();

	return Autotest::Verdict();
}

void CApplication::InitEngine()
{
	PROF_EVENT("INIT_ENGINE");
#ifdef DEBUG_DRAW
	xrLogger::EnableFastDebugLog();
#endif

	splash::SetProgressStatus(30, "Initializing engine");
	EngineLoadStage2();

	splash::SetProgressStatus(40, "Calculating renderer list");
	Engine.External.CreateRendererList();

	Console = new CConsole();

	splash::SetProgressStatus(50, "Reading user settings");
	EngineLoadStage3();

	ConfigureRenderer();

	splash::SetProgressStatus(60, "Initializing engine external");
	Engine.External.Initialize();

	Msg
	(
		"IX-Ray %s %s build info: hash[%s] branch[%s] commit author[%s]",
		EngineExternal().GetCurrentPlatformFullName(),
		_VER, _HASH, _BRANCH, _AUTHOR
	);

	splash::SetProgressStatus(70, "Creating device");
	EngineLoadStage4();

	splash::SetProgressStatus(80, "Loading custom settings");
	LoadCustomSettings();
	InitDebugTools();

	splash::SetProgressStatus(90, "Loading menu");
	EngineLoadStage5();
}

int CApplication::BeginPlay()
{
	PROF_EVENT("BEGIN_PLAY");

	SDL_SetHint(SDL_HINT_JOYSTICK_HIDAPI_STEAMDECK, "1");
	SDL_SetHint("SDL_GAMECONTROLLER_ALLOW_STEAM_VIRTUAL_GAMEPAD", "1");
	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_GAMEPAD | SDL_INIT_EVENTS))
	{
		return -1;
	}

	splash::SetProgressStatus(5, "Initializing debugger");
	Debug._initialize(false);
	
	splash::SetProgressStatus(10, "Calculating display modes");
	EnumerateDisplayModes();

	// AI: a hidden window keeps test runs off the screen, a normal run shows it (that is what makes the
	// AI: first frame visible); only autotest runs are meant to stay invisible.
	const bool l_hiddenWindow = strstr(Core.Params, "-autotest") != nullptr;
	SDL_WindowFlags WndFlags = l_hiddenWindow ? SDL_WINDOW_HIDDEN : 0;
#ifndef IXR_WINDOWS
	WndFlags = SDL_WINDOW_VULKAN | (l_hiddenWindow ? SDL_WINDOW_HIDDEN : 0);
#endif
#ifdef IXR_MACOS
	// AI: SDL3 allows only one of OPENGL/METAL/VULKAN per window (SDL_CreateWindow fails with
	// AI: "Conflicting window graphics flags specified"), and on macOS the Vulkan surface is built from
	// AI: a Metal view (Cocoa_Vulkan_CreateSurface -> Cocoa_Metal_CreateView, which needs
	// AI: SDL_WINDOW_METAL). The Vulkan loader is therefore loaded explicitly right after the window is
	// AI: created, because SDL does that itself only for SDL_WINDOW_VULKAN windows and DXVK's SDL3 WSI
	// AI: asks SDL for vkGetInstanceProcAddr.
	WndFlags = SDL_WINDOW_VULKAN | (l_hiddenWindow ? SDL_WINDOW_HIDDEN : 0);
#endif

	g_AppInfo.Window = SDL_CreateWindow("IX-Ray Engine", 0, 0, WndFlags);

#ifdef IXR_MACOS
	if (!SDL_Vulkan_LoadLibrary(nullptr))
		Msg("! AI: SDL_Vulkan_LoadLibrary failed: %s", SDL_GetError());
#endif

	splash::SetProgressStatus(20, "Initializing xrCore");
	EngineLoadStage1(CommandLine.data());

	return 1;
}

void CApplication::EndPlay()
{
	PROF_EVENT("END_PLAY");
	xr_delete(g_pStringTable);
	Core._destroy();
}

void CApplication::InitDebugTools()
{
#ifdef DEBUG_DRAW
	RenderUI();
	EditorLuaInit();
	ECSViewDraw();
#endif
}

CApplication::CApplication(ENTRY_ARGS)
{
#ifdef IXR_WINDOWS
	CommandLine = lpCmdLine;
#else
	for (int i = 0; i < argc; ++i)
	{
		CommandLine += argv[i];
		CommandLine += " ";
	}
#endif
}

void CApplication::EnumerateDisplayModes()
{
	PROF_EVENT("EnumerateDisplayModes");
	SDL_DisplayID PrimaryDisplay = SDL_GetPrimaryDisplay();
	if (!PrimaryDisplay)
	{
		return;
	}
	bool IsHigherResolutionFound = false;

	const char* DisplayName = SDL_GetDisplayName(PrimaryDisplay);
	SDL_Log("Enumerating for display %" SDL_PRIu32 ": %s\n", PrimaryDisplay, DisplayName ? DisplayName : "Unknown");

	const SDL_DisplayMode* DisplayMode = SDL_GetDesktopDisplayMode(PrimaryDisplay);
	if (!DisplayMode)
	{
		SDL_Log("Failed to get display mode, using defaults ...");
		psCurrentVidMode[0] = 1024;
		psCurrentVidMode[1] = 768;
		return;
	}

	if (IsHigherResolutionFound && psCurrentVidMode[0] < DisplayMode->w && psCurrentVidMode[1] < DisplayMode->h)
	{
		psCurrentVidMode[0] = DisplayMode->w;
		psCurrentVidMode[1] = DisplayMode->h;
	}
	else if (!IsHigherResolutionFound)
	{
		psCurrentVidMode[0] = DisplayMode->w;
		psCurrentVidMode[1] = DisplayMode->h;
		//IsHigherResolutionFound = true;
	}
}

void CApplication::MigrateToGameWindow()
{
	PROF_EVENT("MigrateToGameWindow");
	SDL_ShowWindow(g_AppInfo.Window);
	SDL_SetWindowTitle(g_AppInfo.Window, "IX-Ray Engine");

	Console->Execute("vid_restart");
	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Device.Width, &Device.Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &Device.PosX, &Device.PosY);

	if (Autotest::Active())
		SDL_HideWindow(g_AppInfo.Window);
}

void CApplication::LoadCustomSettings()
{
	PROF_EVENT("LoadCustomSettings");
	FS_FileSet SettingsFiles = {};
	FS.file_list(SettingsFiles, _game_config_, FS_ListFiles, "ixray_settings\\default_settings*.ltx");

	for (auto& FSFile : SettingsFiles)
	{
		string_path DefaultSettings = {};
		FS.update_path(DefaultSettings, _game_config_, FSFile.name.c_str());
		Console->ExecuteScript(DefaultSettings);
	}
}

void CApplication::ConfigureRenderer()
{
	if (Core.ParamsData.test(ECoreParams::r4))
	{
		Console->Execute("renderer renderer_r4");
	}
	else if (Core.ParamsData.test(ECoreParams::r2))
	{
		Console->Execute("renderer renderer_r2");
	}
	else
	{
		CCC_LoadCFG_custom* pTmp = new CCC_LoadCFG_custom("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
		// � ����� ������ ���� �������� ������� CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
	}
}

#if defined(IXR_WINDOWS)
Platform::CMutexHandle CApplication::CheckSingleInstance()
{
#define STALKER_PRESENCE_MUTEX TEXT("Local\\STALKER-COP")

	HANDLE CheckPresenceMutex = OpenMutex(READ_CONTROL, false, STALKER_PRESENCE_MUTEX);
	if (CheckPresenceMutex == nullptr)
	{
		CheckPresenceMutex = CreateMutex(nullptr, false, STALKER_PRESENCE_MUTEX);
		if (CheckPresenceMutex == nullptr)
		{
			return Platform::CMutexHandle(nullptr);
		}
		return Platform::CMutexHandle(CheckPresenceMutex);
	}

	CloseHandle(CheckPresenceMutex);
	return Platform::CMutexHandle(nullptr);
}
#endif
