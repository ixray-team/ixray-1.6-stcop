#include "resource.h"

#include "../xrEngine/stdafx.h"
#include "../xrEngine/x_ray.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"

#include <SDL3/SDL.h>
#include "Splash.h"

#include "../xrCore/git_version.h"
#include "UIEditorMain.h"

#include "AMDGPUTransferee.h"
#include "NvGPUTransferee.h"

#ifndef DEBUG
//#define NO_MULTI_INSTANCES
#endif

void EnumerateDisplayModes()
{
	SDL_DisplayID primaryDisplay = SDL_GetPrimaryDisplay();
	if (!primaryDisplay)
	{
		return;
	}
	bool isHigherResolutionFound = false;

	const char* name = SDL_GetDisplayName(primaryDisplay);
	SDL_Log("Enumerating for display %" SDL_PRIu32 ": %s\n", primaryDisplay, name ? name : "Unknown");

	const SDL_DisplayMode* pDisplayMode = SDL_GetDesktopDisplayMode(primaryDisplay);
	if (!pDisplayMode)
	{
		SDL_Log("Failed to get display mode, using defaults ...");
		psCurrentVidMode[0] = 800;
		psCurrentVidMode[1] = 600;
		return;
	}

	if (isHigherResolutionFound && psCurrentVidMode[0] < pDisplayMode->w && psCurrentVidMode[1] < pDisplayMode->h)
	{
		psCurrentVidMode[0] = pDisplayMode->w;
		psCurrentVidMode[1] = pDisplayMode->h;
	}
	else if (!isHigherResolutionFound)
	{
		psCurrentVidMode[0] = pDisplayMode->w;
		psCurrentVidMode[1] = pDisplayMode->h;
		isHigherResolutionFound = true;
	}
}


void MigrateToGameWindow()
{
	SDL_SetWindowTitle(g_AppInfo.Window, "IX-Ray Engine");
		
	SDL_SetWindowFocusable(g_AppInfo.Window, TRUE);
	SDL_SetWindowShape(g_AppInfo.Window, FALSE);
	SDL_SetWindowBordered(g_AppInfo.Window, true);
	SDL_RaiseWindow(g_AppInfo.Window);
	Console->Execute("vid_restart");
	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Device.Width, &Device.Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &Device.PosX, &Device.PosY);
}

static void LoadCustomSettings()
{
	FS_FileSet settingsFiles = {};
	FS.file_list(settingsFiles, "$game_config$", FS_ListFiles, "ixray_settings\\default_settings*.ltx");

	for (auto& fsFile : settingsFiles)
	{
		string_path defaultSettings = {};
		FS.update_path(defaultSettings, "$game_config$", fsFile.name.c_str());
		Console->ExecuteScript(defaultSettings);
	}
}

int APIENTRY WinMain
(
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	char* lpCmdLine,
	int nCmdShow
)
{
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMEPAD) != 0) {
		return -1;
	}

	Debug._initialize(false);

	// Check for another instance
#ifdef NO_MULTI_INSTANCES
#define STALKER_PRESENCE_MUTEX TEXT("Local\\STALKER-COP")

	HANDLE hCheckPresenceMutex = INVALID_HANDLE_VALUE;
	hCheckPresenceMutex = OpenMutex(READ_CONTROL, FALSE, STALKER_PRESENCE_MUTEX);
	if (hCheckPresenceMutex == nullptr) {
		// New mutex
		hCheckPresenceMutex = CreateMutex(nullptr, FALSE, STALKER_PRESENCE_MUTEX);
		if (hCheckPresenceMutex == nullptr)
			// Shit happens
			return 2;
	}
	else {
		// Already running
		CloseHandle(hCheckPresenceMutex);
		return 1;
	}
#endif
	EnumerateDisplayModes();

	splash::show((void*&)g_AppInfo.Window);

	EngineLoadStage1(lpCmdLine);

	g_pGPU = new CNvReader();
	g_pGPU->Initialize();
	if (!((CNvReader*)(g_pGPU))->bSupport)
	{
		xr_delete(g_pGPU);
		g_pGPU = new CAMDReader;
		g_pGPU->Initialize();
	}
#ifdef DEBUG
	xrLogger::EnableFastDebugLog();
#endif
	EngineLoadStage2();

	Engine.External.CreateRendererList();

	Console = new CConsole();
	EngineLoadStage3();

	if (Core.ParamsData.test(ECoreParams::r4)) {
		Console->Execute("renderer renderer_r4");
	}
	else if (Core.ParamsData.test(ECoreParams::r2)) {
		Console->Execute("renderer renderer_r2");
	} else {
		CCC_LoadCFG_custom* pTmp = new CCC_LoadCFG_custom("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
		// В любом случае надо вызывать команду CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
	}

	LoadCustomSettings();
	Engine.External.Initialize();

	//Console->Execute("stat_memory");
	Msg("IX-Ray CoP %s build info: hash[%s] branch[%s] commit author[%s]", _VER, _HASH, _BRANCH, _AUTHOR);

	EngineLoadStage4();

	// Splash wnd => Game wnd
	splash::hide();
	MigrateToGameWindow();
	
#ifdef DEBUG_DRAW
	RenderUI();
	EditorLuaInit();
#endif

	EngineLoadStage5();

	xr_delete(g_pStringTable);
	xr_delete(g_pGPU);

	Core._destroy();

#ifdef NO_MULTI_INSTANCES		
	// Delete application presence mutex
	CloseHandle(hCheckPresenceMutex);
#endif

	return (0);
}
