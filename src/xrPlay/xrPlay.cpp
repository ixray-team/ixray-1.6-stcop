#include "resource.h"
#include "../xrEngine/stdafx.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"
#include "../xrCore/xrCore_platform.h"
#include <SDL3/SDL.h>
#include "DynamicSplashScreen.h"

#include "../GitParser/git_version.h"
#include "UIEditorMain.h"

#ifndef DEBUG
#define NO_MULTI_INSTANCES
#endif

ENGINE_API void EngineLoadStage1(char* Cmd);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();

INT_PTR CALLBACK logDlgProc(HWND hw, UINT msg, WPARAM wp, LPARAM lp);

void CreateGameWindow()
{
	if (g_AppInfo.Window == NULL) {
		SDL_WindowFlags window_flags = SDL_WINDOW_HIDDEN;
		u32 screen_width = GetSystemMetrics(SM_CXSCREEN);
		u32 screen_height = GetSystemMetrics(SM_CYSCREEN);
		g_AppInfo.Window = SDL_CreateWindow("IXRay HypeEngine", screen_width, screen_height, window_flags);
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
	if (hCheckPresenceMutex == NULL) {
		// New mutex
		hCheckPresenceMutex = CreateMutex(NULL, FALSE, STALKER_PRESENCE_MUTEX);
		if (hCheckPresenceMutex == NULL)
			// Shit happens
			return 2;
	}
	else {
		// Already running
		CloseHandle(hCheckPresenceMutex);
		return 1;
	}
#endif

	SetThreadAffinityMask(GetCurrentThread(), 1);
	CreateGameWindow();

	// Title window
	RegisterWindowClass(hInstance, nCmdShow);

	EngineLoadStage1(lpCmdLine);

	EngineLoadStage2();

	Engine.External.CreateRendererList();

	Console = xr_new<CConsole>();
	EngineLoadStage3();

	if (strstr(Core.Params, "-r4"))
		Console->Execute("renderer renderer_r4");
	else if (strstr(Core.Params, "-r2"))
		Console->Execute("renderer renderer_r2");
	else {
		CCC_LoadCFG_custom* pTmp = xr_new<CCC_LoadCFG_custom>("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
		// В любом случае надо вызывать команду CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
	}
	Engine.External.Initialize();

	Console->Execute("stat_memory");

	Msg("IX-Ray 1.6.02 build info: hash[%s] branch[%s] commit author[%s]", _HASH, _BRANCH, _AUTHOR);
	
	EngineLoadStage4();

	// Destroy LOGO
	DestroyWindow(logoWindow);
	logoWindow = NULL;
	
	SDL_ShowWindow(g_AppInfo.Window);

	// Show main wnd
	Console->Execute("vid_restart");
#ifdef DEBUG_DRAW
	RenderUI();
#endif
	EngineLoadStage5();

	xr_delete(g_pStringTable);
	Core._destroy();

#ifdef NO_MULTI_INSTANCES		
	// Delete application presence mutex
	CloseHandle(hCheckPresenceMutex);
#endif

	return (0);
}
