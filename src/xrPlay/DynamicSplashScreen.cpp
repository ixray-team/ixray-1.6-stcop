#include "resource.h"
constexpr const wchar_t* c_szSplashClass = _T("SplashWindow");
HWND logoWindow = NULL;

IStream* CreateStreamOnResource(LPCTSTR lpName, LPCTSTR lpType) 
{
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

    return nullptr;
}

HWND CreateSplashWindow(HINSTANCE hInst) 
{
    HWND hwndOwner = CreateWindow(c_szSplashClass, NULL, WS_POPUP, 0, 0, 0, 0, NULL, NULL, hInst, NULL);
    return CreateWindowEx(WS_EX_LAYERED, c_szSplashClass, NULL, WS_POPUP | WS_VISIBLE, 0, 0, 0, 0,
        hwndOwner, NULL, hInst, NULL);
}

HWND WINAPI ShowSplash(HINSTANCE hInstance, int nCmdShow) 
{
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

    // alpha
    BLENDFUNCTION blend = { 0 };
    blend.BlendOp = AC_SRC_OVER;
    blend.SourceConstantAlpha = 255;
    blend.AlphaFormat = AC_SRC_ALPHA;

    POINT ptPos = { 0, 0 };
    SIZE sizeWnd = { splashWidth, splashHeight };
    POINT ptSrc = { 0, 0 };
    HWND hDT = GetDesktopWindow();

    if (hDT)
    {
        RECT rcDT;
        GetWindowRect(hDT, &rcDT);
        ptPos.x = (rcDT.right - splashWidth) / 2;
        ptPos.y = (rcDT.bottom - splashHeight) / 2;
    }

    UpdateLayeredWindow(hWnd, hdcScreen, &ptPos, &sizeWnd, hDC, &ptSrc, 0, &blend, ULW_ALPHA);

    return hWnd;
}

void RegisterWindowClass(HINSTANCE hInst, bool nCmdShow)
{
    WNDCLASS wc = { 0 };
    wc.lpfnWndProc = DefWindowProc;
    wc.hInstance = hInst;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.lpszClassName = c_szSplashClass;
    RegisterClass(&wc);

    logoWindow = ShowSplash(hInst, nCmdShow);
    SendMessage(logoWindow, WM_DESTROY, 0, 0);
}