#include "resource.h"
#include "../xrEngine/stdafx.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"
#include "../xrCore/xrCore_platform.h"
#include "DynamicSplashScreen.h"

ENGINE_API void EngineLoadStage1(char* Cmd);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();

INT_PTR CALLBACK logDlgProc(HWND hw, UINT msg, WPARAM wp, LPARAM lp);
LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

void CreateGameWindow() {
	// Unless a substitute hWnd has been specified, create a window to render into
	if (g_AppInfo.WindowHandle == NULL) {
		const wchar_t* wndclass = L"IX-RAY_1.6";

		// Register the windows class
		HINSTANCE hInstance = (HINSTANCE)GetModuleHandle(0);
		WNDCLASS wndClass = { CS_HREDRAW | CS_VREDRAW | CS_OWNDC, WndProc, 0, 0, hInstance,
							  LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1)),
							  LoadCursor(NULL, IDC_ARROW),
							  (HBRUSH)GetStockObject(BLACK_BRUSH),
							  NULL, wndclass };
		RegisterClass(&wndClass);

		// Set the window's initial style
		Device.m_dwWindowStyle = WS_POPUP;

		// Create the render window
		u32 screen_width = GetSystemMetrics(SM_CXSCREEN);
		u32 screen_height = GetSystemMetrics(SM_CYSCREEN);

		DEVMODE screen_settings;
		memset(&screen_settings, 0, sizeof(screen_settings));
		screen_settings.dmSize = sizeof(screen_settings);
		screen_settings.dmPelsWidth = (unsigned long)screen_width;
		screen_settings.dmPelsHeight = (unsigned long)screen_height;
		screen_settings.dmBitsPerPel = 32;
		screen_settings.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;

		ChangeDisplaySettings(&screen_settings, CDS_FULLSCREEN);

		g_AppInfo.WindowHandle = CreateWindowEx(WS_EX_TOPMOST, wndclass, L"S.T.A.L.K.E.R.: Call of Pripyat", Device.m_dwWindowStyle, 0, 0,
			screen_width, screen_height, 0L, 0, hInstance, 0L);
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
	Debug._initialize(false);

	// Check for another instance
#ifdef NO_MULTI_INSTANCES
#define STALKER_PRESENCE_MUTEX "Local\\STALKER-COP"

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

	if (strstr(Core.Params, "-r4")) {
		Console->Execute("renderer renderer_r4");
	} else if (strstr(Core.Params, "-r2")) {
		Console->Execute("renderer renderer_r2");
	} else {
		CCC_LoadCFG_custom* pTmp = xr_new<CCC_LoadCFG_custom>("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
		// В любом случае надо вызывать команду CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
	}
	Engine.External.Initialize();

	Console->Execute("stat_memory");

	EngineLoadStage4();

	// Destroy LOGO
	DestroyWindow(logoWindow);
	logoWindow = NULL;

	// Show main wnd
	Console->Execute("vid_restart");
	
	EngineLoadStage5();

	xr_delete(g_pStringTable);
	Core._destroy();

#ifdef NO_MULTI_INSTANCES		
	// Delete application presence mutex
	CloseHandle(hCheckPresenceMutex);
#endif

	return (0);
}
