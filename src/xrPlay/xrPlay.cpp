#include "resource.h"

#include "../xrEngine/stdafx.h"
#include "../xrEngine/x_ray.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"

#include <SDL2/SDL.h>
#include "DynamicSplashScreen.h"

#include "../xrCore/git_version.h"
#include "UIEditorMain.h"

#include "AMDGPUTransferee.h"
#include "NvGPUTransferee.h"

#ifndef DEBUG
#define NO_MULTI_INSTANCES
#endif

INT_PTR CALLBACK logDlgProc(HWND hw, UINT msg, WPARAM wp, LPARAM lp);

void EnumerateDisplayModes()
{
	int primaryDisplay = 0;

	int numDisplays = SDL_GetNumVideoDisplays();
	if (numDisplays <= primaryDisplay)
	{
		psCurrentVidMode[0] = 800;
		psCurrentVidMode[1] = 600;
		return;
	}

	const char* name = SDL_GetDisplayName(primaryDisplay);

	SDL_DisplayMode displayMode;
	if (SDL_GetDesktopDisplayMode(primaryDisplay, &displayMode) != 0)
	{
		psCurrentVidMode[0] = 800;
		psCurrentVidMode[1] = 600;
		return;
	}

	bool isHigherResolutionFound = false;

	if (isHigherResolutionFound && psCurrentVidMode[0] < displayMode.w && psCurrentVidMode[1] < displayMode.h)
	{
		psCurrentVidMode[0] = displayMode.w;
		psCurrentVidMode[1] = displayMode.h;
	}
	else if (!isHigherResolutionFound)
	{
		psCurrentVidMode[0] = displayMode.w;
		psCurrentVidMode[1] = displayMode.h;
		isHigherResolutionFound = true;
	}
}

void CreateGameWindow()
{
	if (g_AppInfo.Window == nullptr) {

		EnumerateDisplayModes();

		SDL_WindowFlags window_flags = SDL_WINDOW_HIDDEN;
		g_AppInfo.Window = SDL_CreateWindow(
			"IX-Ray Engine",
			SDL_WINDOWPOS_UNDEFINED,
			SDL_WINDOWPOS_UNDEFINED,
			psCurrentVidMode[0],
			psCurrentVidMode[1],
			window_flags
		);
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
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER) != 0) {
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

	//SetThreadAffinityMask(GetCurrentThread(), 1);
	CreateGameWindow();

	// Title window
	RegisterWindowClass(hInstance, nCmdShow);

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
	}
	else {
		CCC_LoadCFG_custom* pTmp = new CCC_LoadCFG_custom("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
		// В любом случае надо вызывать команду CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
	}
	Engine.External.Initialize();

	Console->Execute("stat_memory");
	Msg("IX-Ray CoP %s build info: hash[%s] branch[%s] commit author[%s]", _VER, _HASH, _BRANCH, _AUTHOR);

	EngineLoadStage4();

	// Destroy LOGO
	DestroyWindow(logoWindow);
	logoWindow = nullptr;

	SDL_ShowWindow(g_AppInfo.Window);

	// Show main wnd
	Console->Execute("vid_restart");
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
