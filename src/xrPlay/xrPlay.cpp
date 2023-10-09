#include "resource.h"
#include "../xrEngine/stdafx.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"

extern ENGINE_API bool g_dedicated_server;

static HWND logoWindow = NULL;

ENGINE_API void EngineLoadStage1(char* Cmd);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();

INT_PTR CALLBACK logDlgProc(HWND hw, UINT msg, WPARAM wp, LPARAM lp);
LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

void CreateGameWindow() {
	// Unless a substitute hWnd has been specified, create a window to render into
	if (Device.m_hWnd == NULL) {
		const char* wndclass = "_XRAY_1.6";

		// Register the windows class
		HINSTANCE hInstance = (HINSTANCE)GetModuleHandle(0);
		WNDCLASS wndClass = { 0, WndProc, 0, 0, hInstance,
							  LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1)),
							  LoadCursor(NULL, IDC_ARROW),
							  (HBRUSH)GetStockObject(BLACK_BRUSH),
							  NULL, wndclass };
		RegisterClass(&wndClass);

		// Set the window's initial style
		Device.m_dwWindowStyle = WS_BORDER | WS_DLGFRAME;

		// Set the window's initial width
		RECT rc;
		SetRect(&rc, 0, 0, 640, 480);
		AdjustWindowRect(&rc, Device.m_dwWindowStyle, FALSE);

		// Create the render window
		Device.m_hWnd = CreateWindowEx(WS_EX_TOPMOST,
			wndclass, "S.T.A.L.K.E.R.: Call of Pripyat", Device.m_dwWindowStyle,
			/*rc.left, rc.top, */CW_USEDEFAULT, CW_USEDEFAULT,
			(rc.right - rc.left), (rc.bottom - rc.top), 0L,
			0, hInstance, 0L);
	}
}

int APIENTRY WinMain(HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	char* lpCmdLine,
	int nCmdShow) {
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
	} else {
		// Already running
		CloseHandle(hCheckPresenceMutex);
		return 1;
	}
#endif

	SetThreadAffinityMask(GetCurrentThread(), 1);
	CreateGameWindow();

	// Title window
	logoWindow = CreateDialog(GetModuleHandle(NULL), MAKEINTRESOURCE(IDD_STARTUP), 0, logDlgProc);

	HWND logoPicture = GetDlgItem(logoWindow, IDC_STATIC_LOGO);
	RECT logoRect;
	GetWindowRect(logoPicture, &logoRect);

	SetWindowPos(
		logoWindow,
#ifndef DEBUG
		HWND_TOPMOST,
#else
		HWND_NOTOPMOST,
#endif // NDEBUG
		0,
		0,
		logoRect.right - logoRect.left,
		logoRect.bottom - logoRect.top,
		SWP_NOMOVE | SWP_SHOWWINDOW// | SWP_NOSIZE
	);

	UpdateWindow(logoWindow);

	EngineLoadStage1(lpCmdLine);

	EngineLoadStage2();

	Engine.External.CreateRendererList();

	Console = xr_new<CConsole>();
	EngineLoadStage3();

	if (strstr(Core.Params, "-r2a"))
		Console->Execute("renderer renderer_r2a");
	else if (strstr(Core.Params, "-r2"))
		Console->Execute("renderer renderer_r2");
	else {
		CCC_LoadCFG_custom* pTmp = xr_new<CCC_LoadCFG_custom>("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
	}
	Engine.External.Initialize();

	Console->Execute("stat_memory");

	EngineLoadStage4();

	// Destroy LOGO
	if (!g_dedicated_server) {
		DestroyWindow(logoWindow);
		logoWindow = NULL;
	}

	// Show main wnd
	ShowWindow(Device.m_hWnd, SW_SHOW);

	EngineLoadStage5();

	Core._destroy();

#ifdef NO_MULTI_INSTANCES		
	// Delete application presence mutex
	CloseHandle(hCheckPresenceMutex);
#endif

	return (0);
}
