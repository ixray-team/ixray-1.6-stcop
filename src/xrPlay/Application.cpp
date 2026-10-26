#include "Application.h"
#include "UIEditorMain.h"

#include "../xrEngine/x_ray.h"
#include "../xrEngine/Autotest.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"
#include "../xrEngine/device.h"

#include "../xrCore/git_version.h"
#include "../xrCore/CrashDumpHelper.h"

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

	SteamWorks.BeginPlay();
	InitEngine();
	CrashDumpHelper::Start();

	MigrateToGameWindow();
	EngineLoopAndDestroy();

	EndPlay();

	return Autotest::Verdict();
}

void CApplication::InitEngine()
{
	PROF_EVENT("INIT_ENGINE");
#ifdef DEBUG_DRAW
	xrLogger::EnableFastDebugLog();
#endif

	CRenderDevice::SetStartupStatus(30, "Initializing engine core");
	EngineLoadStage2();

	CRenderDevice::SetStartupStatus(40, "Calculating renderer list");
	Engine.External.CreateRendererList();

	Console = new CConsole();

	CRenderDevice::SetStartupStatus(50, "Reading user settings");
	EngineLoadStage3();

	ConfigureRenderer();

	CRenderDevice::SetStartupStatus(60, "Initializing renderer external");
	Engine.External.Initialize();

	Msg
	(
		"IX-Ray %s %s build info: hash[%s] branch[%s] commit author[%s]",
		EngineExternal().GetCurrentPlatformFullName(),
		_VER, _HASH, _BRANCH, _AUTHOR
	);

	CRenderDevice::SetStartupStatus(70, "Creating render device");
	EngineLoadStage4();

	CRenderDevice::SetStartupStatus(85, "Loading custom settings");
	LoadCustomSettings();
	InitDebugTools();

	CRenderDevice::SetStartupStatus(90, "Loading persistent objects");
	EngineLoadStage5();
}

int CApplication::BeginPlay()
{
	PROF_EVENT("BEGIN_PLAY");

#if defined(IXR_APPLE_SERIES)
	// Default fullscreen uses a separate Space, which drops the window's key status and with it
	// every mouse click. The non-Spaces path is a borderless window on the current Space.
	SDL_SetHint(SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES, "0");
#endif
	SDL_SetHint(SDL_HINT_JOYSTICK_HIDAPI_STEAMDECK, "1");
	SDL_SetHint("SDL_GAMECONTROLLER_ALLOW_STEAM_VIRTUAL_GAMEPAD", "1");
	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_GAMEPAD | SDL_INIT_EVENTS))
	{
		return -1;
	}

	CRenderDevice::SetStartupStatus(5, "Initializing debugger");
	Debug._initialize(false);
	
	CRenderDevice::SetStartupStatus(10, "Calculating display modes");
	EnumerateDisplayModes();

	const bool headless = Autotest::Active() || CommandLine.find("-autotest") != xr_string::npos;
	SDL_WindowFlags WndFlags = headless ? SDL_WINDOW_HIDDEN : 0;
#if defined(IXR_APPLE_SERIES)
	// DXMT presents through its own CAMetalLayer (see MacMetalBridge.mm), no Vulkan surface.
	WndFlags |= SDL_WINDOW_METAL;
#elif !defined(IXR_WINDOWS)
	WndFlags = SDL_WINDOW_VULKAN;
#endif

	g_AppInfo.Window = SDL_CreateWindow("IX-Ray Engine", psCurrentVidMode[0], psCurrentVidMode[1], WndFlags);
	if (!headless && g_AppInfo.Window)
	{
		SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
		SDL_ShowWindow(g_AppInfo.Window);
		SDL_RaiseWindow(g_AppInfo.Window);
		SDL_PumpEvents();
	}

	CRenderDevice::SetStartupStatus(20, "Initializing xrCore");
	EngineLoadStage1(CommandLine.data());

	return 1;
}

void CApplication::EndPlay()
{
	PROF_EVENT("END_PLAY");
	SteamWorks.EndPlay();
	xr_delete(g_pStringTable);
	if (g_AppInfo.Window)
	{
		SDL_DestroyWindow(g_AppInfo.Window);
		g_AppInfo.Window = nullptr;
	}
	SDL_Quit();
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
	// Windows fills Core.Params from GetCommandLine; on other platforms it stayed
	// EMPTY, silently disabling every command-line switch (-nointro, -autotest, ...).
	xr_strcpy(Core.Params, sizeof(Core.Params), CommandLine.c_str());
	xr_strlwr(Core.Params);
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
	const bool headless = Autotest::Active();
	if (headless)
	{
		psDeviceFlags.set(rsFullscreen, FALSE);
	}
	else
	{
		SDL_ShowWindow(g_AppInfo.Window);
		SDL_RaiseWindow(g_AppInfo.Window);
	}

#if defined(IXR_APPLE_SERIES)
	if (headless)
	{
		// HACK: a hidden window starves DXMT presents; keep it visible but off screen.
		SDL_SetWindowPosition(g_AppInfo.Window, -3000, -3000);
		SDL_ShowWindow(g_AppInfo.Window);
	}
#endif

	SDL_SetWindowTitle(g_AppInfo.Window, "IX-Ray Engine");

	Console->Execute("vid_restart");
	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Device.Width, &Device.Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &Device.PosX, &Device.PosY);

	if (headless)
	{
#if defined(IXR_APPLE_SERIES)
		SDL_SetWindowPosition(g_AppInfo.Window, -3000, -3000);
#else
		SDL_HideWindow(g_AppInfo.Window);
#endif
	}
	else
	{
		SDL_RaiseWindow(g_AppInfo.Window);
	}
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
	if (strstr(Core.Params, "-r1"))
	{
		Console->Execute("renderer renderer_r4_static");
	}
	else if (Core.ParamsData.test(ECoreParams::r4))
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
