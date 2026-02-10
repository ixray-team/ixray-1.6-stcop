#include "stdafx.h"

#include "device_win_custom.h"

#if _WINDOWS
#include <windows.h>
#include <dwmapi.h>
#pragma comment(lib, "dwmapi.lib")

void EnableDwmRendering(HWND hwnd)
{
	if (!hwnd) return;

	BOOL enable = TRUE;

	DwmSetWindowAttribute(
		hwnd,
		DWMWA_NCRENDERING_ENABLED,
		&enable,
		sizeof(enable));
}

void EnableShadow(HWND hwnd)
{
	if (!hwnd) return;

	MARGINS margins = { 1,1,1,1 };

	DwmExtendFrameIntoClientArea(hwnd, &margins);
}

void RemoveDwmBorder(HWND hwnd)
{
	COLORREF none = DWMWA_COLOR_DEFAULT;

	DwmSetWindowAttribute(
		hwnd,
		DWMWA_BORDER_COLOR,
		&none,
		sizeof(none));
}
void ExtendFrame(HWND hwnd)
{
	MARGINS m = { 1,1,1,1 };
	DwmExtendFrameIntoClientArea(hwnd, &m);
}
void RefreshFrame(HWND hwnd)
{
	SetWindowPos(hwnd, nullptr, 0, 0, 0, 0,
		SWP_FRAMECHANGED |
		SWP_NOMOVE |
		SWP_NOSIZE |
		SWP_NOZORDER);
}

#endif

void win_cheese_layer(
#if _WINDOWS
	HWND hwnd
#endif
)
{
#if _WINDOWS
	if (!hwnd) return;

	EnableDwmRendering(hwnd);

	RemoveDwmBorder(hwnd);
	ExtendFrame(hwnd);

	EnableShadow(hwnd);
	
	LONG style = GetWindowLong(hwnd, GWL_STYLE);
	style |= WS_THICKFRAME;
	style |= WS_CAPTION; 
	SetWindowLong(hwnd, GWL_STYLE, style);

	RefreshFrame(hwnd);
#endif
}