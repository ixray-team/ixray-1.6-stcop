#include "stdafx.h"
#include "resource.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/Text_Console.h"

ENGINE_API void EngineLoadStage1(char* lpCmdLine);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();

LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

void CreateGameWindow() {
	// Unless a substitute hWnd has been specified, create a window to render into
	if (g_AppInfo.WindowHandle == NULL) {
		const wchar_t* wndclass = L"_XRAY_1.6";

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
		g_AppInfo.WindowHandle = CreateWindowEx(WS_EX_TOPMOST,
			wndclass, L"S.T.A.L.K.E.R.: Call of Pripyat", Device.m_dwWindowStyle,
			/*rc.left, rc.top, */CW_USEDEFAULT, CW_USEDEFAULT,
			(rc.right - rc.left), (rc.bottom - rc.top), 0L,
			0, hInstance, 0L);
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
	ShowWindow(g_AppInfo.WindowHandle, SW_SHOWNORMAL);

	EngineLoadStage5();

	Core._destroy();

	return 0;
}
