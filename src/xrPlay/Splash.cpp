////////////////////////////////////////////////////////////////////////////
//	Name		: Splash.cpp
//	Created 	: 17.09.2024
////////////////////////////////////////////////////////////////////////////

#include <windows.h>
#include <atlimage.h>//обертки для HDC
#include <string>
#include <thread>
#include <cmath>
//#include <../xrCore/_types.h>
#include"Splash.h"
#include"resource.h"

EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#pragma warning(disable: 4047)
HINSTANCE hInstanceG = (HINSTANCE)&__ImageBase;
#pragma warning(default: 4047)

#pragma warning(disable:4715) //-' Значение возвращается не при всех путях выполнения

HANDLE g_hEventFadeInReady = CreateEvent(NULL, TRUE, FALSE, NULL);

//struct pData {
//	u32 b_number;
//};

void FadeOutAndClose(HWND f_hWnd, HDC f_hdcScreen, HDC f_hDC, POINT f_ptPos, POINT f_ptSrc, SIZE f_sizeWnd, BLENDFUNCTION f_blend) {
	
	//Event чтобы не было конфликта с FadeIn()
	// иначе будет мерцание (один добавляет прозрачность, другой убавляет...)
	WaitForSingleObject(g_hEventFadeInReady, INFINITE);
	std::this_thread::sleep_for(std::chrono::milliseconds(500)); //на всякий, чтобы не сразу начало пропадать (если быстро дойдёт до функции)

	const int duration = 2000; // 
	const int max_value = 255;
	const int steps = 100; // Количество шагов
	const double factor = 5.0; // Фактор для экспоненциального замедления

	for (int i = 0; i <= steps; ++i) {
		double t = static_cast<double>(i) / steps;
		int value = static_cast<int>(max_value * std::exp(-factor * t));
		
		f_blend.SourceConstantAlpha = value;
		UpdateLayeredWindow(f_hWnd, f_hdcScreen, &f_ptPos, &f_sizeWnd, f_hDC, &f_ptSrc, 0, &f_blend, ULW_ALPHA);

		std::this_thread::sleep_for(std::chrono::milliseconds(duration / steps));
	}

	f_blend.SourceConstantAlpha = 0;
	UpdateLayeredWindow(f_hWnd, f_hdcScreen, &f_ptPos, &f_sizeWnd, f_hDC, &f_ptSrc, 0, &f_blend, ULW_ALPHA);

	DestroyWindow(f_hWnd);
	PostMessage(f_hWnd, WM_CLOSE, 0, 0);
}

void FadeIn(HWND f_hWnd, HDC f_hdcScreen, HDC f_hDC, POINT &f_ptPos, POINT &f_ptSrc, SIZE &f_sizeWnd, BLENDFUNCTION &f_blend) {

	const int duration = 3000; // 
	const int max_value = 255;
	const int steps = 200; // Количество шагов
	const double factor = 7.0; // Фактор для экспоненциального замедления

	for (int i = 0; i <= steps; ++i) {
		double t = static_cast<double>(i) / steps;
		int value = static_cast<int>(max_value * (1 - std::exp(-factor * t)));

		f_blend.SourceConstantAlpha = value;
		UpdateLayeredWindow(f_hWnd, f_hdcScreen, &f_ptPos, &f_sizeWnd, f_hDC, &f_ptSrc, 0, &f_blend, ULW_ALPHA);

		std::this_thread::sleep_for(std::chrono::milliseconds(duration / steps));
	}

	//на всякий случай
	f_blend.SourceConstantAlpha = 255;
	UpdateLayeredWindow(f_hWnd, f_hdcScreen, &f_ptPos, &f_sizeWnd, f_hDC, &f_ptSrc, 0, &f_blend, ULW_ALPHA);

	SetEvent(g_hEventFadeInReady);
}

IStream* CreateStreamOnResource(LPCTSTR lpName, LPCTSTR lpType)
{
	IStream* ipStream = NULL;
	HRSRC hrsrc;
	DWORD dwResourceSize;
	LPVOID pvSourceResourceData;
	HGLOBAL hgblResourceData;
	LPVOID pvResourceData;
	HGLOBAL hglbImage;

	HMODULE hMODULE = hInstanceG;

	hrsrc = FindResource(hMODULE, lpName, lpType);
	if (hrsrc == NULL)
		goto Return;

	dwResourceSize = SizeofResource(hMODULE, hrsrc);
	hglbImage = LoadResource(hMODULE, hrsrc);
	if (hglbImage == NULL)
		goto Return;

	pvSourceResourceData = LockResource(hglbImage);
	if (pvSourceResourceData == NULL)
		goto Return;

	hgblResourceData = GlobalAlloc(GMEM_MOVEABLE, dwResourceSize);
	if (hgblResourceData == NULL)
		goto Return;

	pvResourceData = GlobalLock(hgblResourceData);

	if (pvResourceData == NULL)
		goto FreeData;

	CopyMemory(pvResourceData, pvSourceResourceData, dwResourceSize);

	GlobalUnlock(hgblResourceData);

	if (SUCCEEDED(CreateStreamOnHGlobal(hgblResourceData, TRUE, &ipStream)))
		goto Return;

FreeData:
	GlobalFree(hgblResourceData);

Return:
	return ipStream;
}


BOOL RegClass(WNDPROC, LPCTSTR, UINT);
LRESULT CALLBACK SplashProc(HWND, UINT, WPARAM, LPARAM);

HINSTANCE hInstance;
TCHAR szClass[] = TEXT("STKSPLASH");

int WINAPI ShowSplash(HINSTANCE hInst, int nCmdShow, HANDLE* g_hEventSplashReady, HWND* g_hWndSplash/*, u32 BuildNumber*/)
{
	MSG msg;
	HWND s_hWnd;
	hInstance = hInst;

	if (!RegClass(SplashProc, szClass, COLOR_WINDOW)) return FALSE;

	//pData data = { BuildNumber };


	s_hWnd = CreateWindowEx(WS_EX_LAYERED | WS_EX_TOPMOST, szClass, TEXT("Splash"), WS_POPUP | WS_VISIBLE, 0, 0, 0, 0, 0, 0, hInstance, 0/* &data*/);

	if (!s_hWnd) {
		SetEvent(g_hEventSplashReady);
		return FALSE;
	}
	*g_hWndSplash = s_hWnd;
	SetEvent(*g_hEventSplashReady);
	
	while (GetMessage(&msg, NULL, 0, 0)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	// FX: Если ты пробуждаешь древнее зло, то будь добр: УСЫПИ ЕГО ОБРАТНО!!1!
	DestroyWindow(s_hWnd);
}

BOOL RegClass(WNDPROC Proc, LPCTSTR szName, UINT brBackground)
{
	WNDCLASS wc{};

	wc.style                    = 0;
	wc.cbClsExtra               = 0;
	wc.cbWndExtra               = 0;
	wc.lpfnWndProc              = Proc;
	wc.hInstance                = hInstance;
	wc.hIcon                    = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1));
	wc.hCursor                  = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground            = reinterpret_cast<HBRUSH>(static_cast<intptr_t>(brBackground + 1));
	wc.lpszMenuName             = NULL;
	wc.lpszClassName            = szName;

	return (RegisterClass(&wc) != 0);
}

LRESULT CALLBACK SplashProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	static HDC hdcScreen, hDC;
	static POINT ptPos, ptSrc;
	static SIZE sizeWnd;
	static BLENDFUNCTION blend{};
	//static u32 BuildNumber;
	switch (msg)
	{
	case WM_SIZE:
		break;
	case WM_CREATE:
	{
		//image
		CImage img;                             //объект изображения

		char path[MAX_PATH];

		GetModuleFileNameA(NULL, path, MAX_PATH);
		std::string splash_path = path;

		splash_path = splash_path.erase(splash_path.find_last_of('\\'), splash_path.size() - 1);
		splash_path += "\\splash.png";

		std::wstring splash_path_wide = std::wstring(splash_path.begin(), splash_path.end()); //ix

		HRESULT ErrorHandle = img.Load(splash_path_wide.c_str());              //загрузка сплеша

		int splashWidth = 0;
		int splashHeight = 0;
		if (ErrorHandle == E_FAIL)
		{
			img.Destroy();
			img.Load(CreateStreamOnResource(MAKEINTRESOURCE(IDB_PNG1), _T("PNG")));//загружаем сплеш
		}

		splashWidth = img.GetWidth();
		splashHeight = img.GetHeight();

		int scr_x = GetSystemMetrics(SM_CXSCREEN);
		int scr_y = GetSystemMetrics(SM_CYSCREEN);

		int pos_x = (scr_x / 2) - (splashWidth / 2);
		int pos_y = (scr_y / 2) - (splashHeight / 2);

		//win size
		SetWindowPos(hWnd, NULL, pos_x, pos_y, splashWidth, splashHeight, SWP_NOMOVE | SWP_NOZORDER);

		hdcScreen = GetDC(NULL);
		hDC = CreateCompatibleDC(hdcScreen);

		HBITMAP hBmp = CreateCompatibleBitmap(hdcScreen, splashWidth, splashHeight);
		HBITMAP hBmpOld = (HBITMAP)SelectObject(hDC, hBmp);

		//рисуем картиночку
		for (int i = 0; i < img.GetWidth(); i++)
		{
			for (int j = 0; j < img.GetHeight(); j++)
			{
				BYTE* ptr = (BYTE*)img.GetPixelAddress(i, j);
				ptr[0] = ((ptr[0] * ptr[3]) + 127) / 255;
				ptr[1] = ((ptr[1] * ptr[3]) + 127) / 255;
				ptr[2] = ((ptr[2] * ptr[3]) + 127) / 255;
			}
		}

		img.AlphaBlend(hDC, 0, 0, splashWidth, splashHeight, 0, 0, splashWidth, splashHeight);

		ptPos = { 0, 0 };
		sizeWnd = { splashWidth, splashHeight };
		ptSrc = { 0, 0 };
		HWND hDT = GetDesktopWindow();

		if (hDT)
		{
			RECT rcDT;
			GetWindowRect(hDT, &rcDT);
			ptPos.x = (rcDT.right - splashWidth) / 2;
			ptPos.y = (rcDT.bottom - splashHeight) / 2;
		}

		//
		blend.BlendOp = AC_SRC_OVER;
		blend.SourceConstantAlpha = 0;
		blend.AlphaFormat = AC_SRC_ALPHA;

		/*
		LPCREATESTRUCT pCreate = (LPCREATESTRUCT)lParam;
		pData* data = (pData*)pCreate->lpCreateParams;

		// Используем данные
		BuildNumber = data->b_number;

		char printText[64];
		snprintf(printText, sizeof(printText), "Build %u (%s %s)", BuildNumber, __DATE__, __TIME__);

		SetBkMode(hDC, TRANSPARENT);
		SetTextColor(hDC, RGB(190, 190, 190));

		SIZE textSize;
		GetTextExtentPoint32(hDC, printText, strlen(printText), &textSize);

		//nad lost aplha
		//TextOutA(hDC, 197, 130, printText, strlen(printText)); //
		
		//pod gunslinger
		int startX = 432 + (670 - 432 - textSize.cx) / 2;
		TextOutA(hDC, startX, 308, printText, strlen(printText));
		*/
		

		break;
	}
	case WM_FADEOUT:
	{
		std::thread([=]() {FadeOutAndClose(hWnd, hdcScreen, hDC, ptPos, ptSrc, sizeWnd, blend); }).detach();
		break;
	}
	case WM_FADIN:
	{
		std::thread([=]() { FadeIn(hWnd, hdcScreen, hDC, ptPos, ptSrc, sizeWnd, blend); }).detach();

		break;
	}
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	};
	return DefWindowProc(hWnd, msg, wParam, lParam);
}