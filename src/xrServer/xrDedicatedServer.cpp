#include "stdafx.h"
#include "resource.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/Text_Console.h"

ENGINE_API void EngineLoadStage1(char* lpCmdLine);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();

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
) {
	Debug._initialize(false);

	g_dedicated_server = true;
	
	CreateGameWindow();

	EngineLoadStage1(lpCmdLine);

	EngineLoadStage2();

	Console = xr_new<CTextConsole>();
	EngineLoadStage3();

	Engine.External.CreateRendererList();

	Console->Execute("renderer renderer_r1");

	Engine.External.Initialize();
	Console->Execute("stat_memory");

	EngineLoadStage4();

	// Show main wnd
	SDL_ShowWindow(g_AppInfo.Window);

	EngineLoadStage5();

	Core._destroy();

	return 0;
}
