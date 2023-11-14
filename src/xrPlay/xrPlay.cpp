#include "resource.h"
#include "../xrEngine/stdafx.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrCore/xrCore_platform.h"

extern ENGINE_API bool g_dedicated_server;

static HWND logoWindow = NULL;

const TCHAR* c_szSplashClass = _T("SplashWindow");

ENGINE_API void EngineLoadStage1(char* Cmd);
ENGINE_API void EngineLoadStage2();
ENGINE_API void EngineLoadStage3();
ENGINE_API void EngineLoadStage4();
ENGINE_API void EngineLoadStage5();

INT_PTR CALLBACK logDlgProc(HWND hw, UINT msg, WPARAM wp, LPARAM lp);
LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

IStream* CreateStreamOnResource(LPCTSTR lpName, LPCTSTR lpType) {
    IStream* ipStream = NULL;

    HRSRC hrsrc = FindResource(NULL, lpName, lpType);
    if (hrsrc == NULL)
        return ipStream;

    DWORD dwResourceSize = SizeofResource(NULL, hrsrc);
    HGLOBAL hglbImage = LoadResource(NULL, hrsrc);
    if (hglbImage == NULL)
        return ipStream;

    LPVOID pvSourceResourceData = LockResource(hglbImage);
    if (pvSourceResourceData == NULL)
        return ipStream;

    HGLOBAL hgblResourceData = GlobalAlloc(GMEM_MOVEABLE, dwResourceSize);
    if (hgblResourceData == NULL)
        return ipStream;

    LPVOID pvResourceData = GlobalLock(hgblResourceData);

    if (pvResourceData == NULL)
        GlobalFree(hgblResourceData);

    CopyMemory(pvResourceData, pvSourceResourceData, dwResourceSize);

    GlobalUnlock(hgblResourceData);

    if (SUCCEEDED(CreateStreamOnHGlobal(hgblResourceData, TRUE, &ipStream)))
        return ipStream;
}

void RegisterWindowClass(HINSTANCE hInst) {
    WNDCLASS wc = { 0 };
    wc.lpfnWndProc = DefWindowProc;
    wc.hInstance = hInst;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.lpszClassName = c_szSplashClass;
    RegisterClass(&wc);
}

HWND CreateSplashWindow(HINSTANCE hInst) {
    HWND hwndOwner = CreateWindow(c_szSplashClass, NULL, WS_POPUP, 0, 0, 0, 0, NULL, NULL, hInst, NULL);
    return CreateWindowEx(WS_EX_LAYERED, c_szSplashClass, NULL, WS_POPUP | WS_VISIBLE, 0, 0, 0, 0,
        hwndOwner, NULL, hInst, NULL);
}

HWND WINAPI ShowSplash(HINSTANCE hInstance, int nCmdShow) {
    MSG msg;
    HWND hWnd;

    // image
    CImage img; // объект изображения

    // img.Destroy();
    img.Load(CreateStreamOnResource(MAKEINTRESOURCE(IDB_PNG1), _T("PNG"))); // загружаем сплеш из ресурсов
    int splashWidth = img.GetWidth();   // фиксируем ширину картинки
    int splashHeight = img.GetHeight(); // фиксируем высоту картинки

    // float temp_x_size = 860.f;
    // float temp_y_size = 461.f;
    int scr_x = GetSystemMetrics(SM_CXSCREEN);
    int scr_y = GetSystemMetrics(SM_CYSCREEN);

    int pos_x = (scr_x / 2) - (splashWidth / 2);
    int pos_y = (scr_y / 2) - (splashHeight / 2);

    // if (!RegClass(SplashProc, szClass, COLOR_WINDOW)) return FALSE;
    hWnd = CreateSplashWindow(hInstance);

    if (!hWnd)
        return FALSE;

    HDC hdcScreen = GetDC(NULL);
    HDC hDC = CreateCompatibleDC(hdcScreen);

    HBITMAP hBmp = CreateCompatibleBitmap(hdcScreen, splashWidth, splashHeight);
    HBITMAP hBmpOld = (HBITMAP)SelectObject(hDC, hBmp);
    // рисуем картиночку
    for (int i = 0; i < img.GetWidth(); i++) {
        for (int j = 0; j < img.GetHeight(); j++) {
            BYTE* ptr = (BYTE*)img.GetPixelAddress(i, j);
            ptr[0] = ((ptr[0] * ptr[3]) + 127) / 255;
            ptr[1] = ((ptr[1] * ptr[3]) + 127) / 255;
            ptr[2] = ((ptr[2] * ptr[3]) + 127) / 255;
        }
    }

    img.AlphaBlend(hDC, 0, 0, splashWidth, splashHeight, 0, 0, splashWidth, splashHeight);

    // alpha
    BLENDFUNCTION blend = { 0 };
    blend.BlendOp = AC_SRC_OVER;
    blend.SourceConstantAlpha = 255;
    blend.AlphaFormat = AC_SRC_ALPHA;

    POINT ptPos = { 0, 0 };
    SIZE sizeWnd = { splashWidth, splashHeight };
    POINT ptSrc = { 0, 0 };
    HWND hDT = GetDesktopWindow();

    if (hDT) {
        RECT rcDT;
        GetWindowRect(hDT, &rcDT);
        ptPos.x = (rcDT.right - splashWidth) / 2;
        ptPos.y = (rcDT.bottom - splashHeight) / 2;
    }

    UpdateLayeredWindow(hWnd, hdcScreen, &ptPos, &sizeWnd, hDC, &ptSrc, 0, &blend, ULW_ALPHA);

    return hWnd;
}

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
		Device.m_dwWindowStyle = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_POPUP;

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
    RegisterWindowClass(hInstance);
    logoWindow = ShowSplash(hInstance, nCmdShow);
    SendMessage(logoWindow, WM_DESTROY, 0, 0);

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
		// В любом случае надо вызывать команду CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
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
	ShowWindow(g_AppInfo.WindowHandle, SW_SHOW);

	EngineLoadStage5();

	Core._destroy();

#ifdef NO_MULTI_INSTANCES		
	// Delete application presence mutex
	CloseHandle(hCheckPresenceMutex);
#endif

	return (0);
}
