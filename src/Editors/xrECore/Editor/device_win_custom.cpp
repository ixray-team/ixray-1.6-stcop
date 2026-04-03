#include "stdafx.h"
#include "device_win_custom.h"

#if _WINDOWS
#include <windows.h>
#include <dwmapi.h>
#pragma comment(lib, "dwmapi.lib")

#define custom_proc 1

extern CEditorRenderDevice* EDevice;
extern ECORE_API TUI* UI;
void RefreshFrame(HWND hwnd)
{
    SetWindowPos(hwnd, nullptr, 0, 0, 0, 0,
        SWP_FRAMECHANGED |
        SWP_NOMOVE |
        SWP_NOSIZE |
        SWP_NOZORDER |
        SWP_NOOWNERZORDER);
}
#if custom_proc
// ---- Сохраняем оригинальную процедуру ----
static WNDPROC g_OriginalWndProc = nullptr;

struct WindowState
{
    bool wasMaximized = false;
};

WindowState g_state;
// ---- Субклассированная процедура ----
static LRESULT CALLBACK CustomWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg)
    {
    case WM_SIZE:
    {
        if (wParam == SIZE_MAXIMIZED && !g_state.wasMaximized)
        {
            MARGINS margins = { 0, 0, 0, 0 };
            DwmExtendFrameIntoClientArea(hwnd, &margins);
            RefreshFrame(hwnd);

            if (!EDevice->isZoomed)
            {
                EDevice->MaximizedWindow();
            }
        }
        else if (wParam == SIZE_RESTORED && g_state.wasMaximized)
        {
            MARGINS margins = { 1, 1, 1, 1 };
            DwmExtendFrameIntoClientArea(hwnd, &margins);
            RefreshFrame(hwnd);

            if (EDevice->isZoomed)
            {
                EDevice->ResoreWindow(false);
            }
            else
            {
                //ТУТ НУЖНО ВЫЗВАТЬ ФУНКЦИЮ, КОТОРАЯ ПЕРЕРИСУЕТ ПРИЛОЖЕНИЕ \
                // БЕЗ ЭТОГО ВЫГЛЯДИТЬ КАК НЕПОНЯТНО ЧТО 
            }
        }

        g_state.wasMaximized = (wParam == SIZE_MAXIMIZED);
        break;
    }

    case WM_NCACTIVATE:
        // lParam = -1 → запретить перерисовку non-client area
        return DefWindowProc(hwnd, msg, wParam, -1);
    case WM_APP+1: //IX_RESTORE
    {
        break;
    }
    case WM_APP + 2: //IX_MAX
    {
        break;
    }

    case WM_SYSCOMMAND:
    {
        LRESULT result = CallWindowProc(g_OriginalWndProc, hwnd, msg, wParam, lParam);

        WPARAM cmd = wParam & 0xFFF0;
        if (cmd == SC_RESTORE || cmd == SC_MAXIMIZE)
        {
            MARGINS margins = { 1, 1, 1, 1 };

            DwmExtendFrameIntoClientArea(hwnd, &margins);

            RefreshFrame(hwnd);

            LONG style = GetWindowLong(hwnd, GWL_STYLE);
            style |= WS_THICKFRAME;
            style |= WS_CAPTION;
            SetWindowLong(hwnd, GWL_STYLE, style);
        }

        return result;
    }
    }

    return CallWindowProc(g_OriginalWndProc, hwnd, msg, wParam, lParam);
}
#endif
// ---- DWM: принудительно включаем DWM-рендеринг non-client area ----
void EnableDwmRendering(HWND hwnd)
{
    if (!hwnd) return;

    // ВАЖНО: DWMWA_NCRENDERING_ENABLED — read-only!
    // Нужно использовать DWMWA_NCRENDERING_POLICY
    DWMNCRENDERINGPOLICY policy = DWMNCRP_ENABLED;
    DwmSetWindowAttribute(
        hwnd,
        DWMWA_NCRENDERING_POLICY,
        &policy,
        sizeof(policy));
}

//void DwmExtendFrame(HWND hwnd, bool enable)
//{
//    if (!hwnd) return;
//    MARGINS margins = { 1, 1, 1, 1 };
//    DwmExtendFrameIntoClientArea(hwnd, &margins);
//}

void RemoveDwmBorder(HWND hwnd)
{
    COLORREF none = DWMWA_COLOR_NONE;
    DwmSetWindowAttribute(
        hwnd,
        DWMWA_BORDER_COLOR,
        &none,
        sizeof(none));
}

#if custom_proc

void _SubclassWindow(HWND hwnd)
{
    if (!hwnd || g_OriginalWndProc) return;

    g_OriginalWndProc = (WNDPROC)SetWindowLongPtr(
        hwnd,
        GWLP_WNDPROC,
        (LONG_PTR)CustomWndProc);
}
#endif

#endif

void win_chezze_layer(
#if _WINDOWS
    HWND hwnd
#endif
)
{
#if _WINDOWS
    if (!hwnd) return;

    // 1. Стили окна
    LONG style = GetWindowLong(hwnd, GWL_STYLE);
    style |= WS_THICKFRAME;
    style |= WS_CAPTION;
    SetWindowLong(hwnd, GWL_STYLE, style);

    // 2. DWM-настройки
    EnableDwmRendering(hwnd);
    RemoveDwmBorder(hwnd);
    //ExtendFrame(hwnd);
    MARGINS margins = { 1, 1, 1, 1 };
    DwmExtendFrameIntoClientArea(hwnd, &margins);

    // 3. Субклассируем — перехватываем WM_NCACTIVATE / WM_NCPAINT
    #if custom_proc
    _SubclassWindow(hwnd);
    #endif
    // 4. Применяем
    RefreshFrame(hwnd);
#endif
}