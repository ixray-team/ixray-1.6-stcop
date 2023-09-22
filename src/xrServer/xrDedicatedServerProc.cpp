#include "stdafx.h"
//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Static msg handler which passes messages to the application class.
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT		result;
	if (Device.on_message(hWnd, uMsg, wParam, lParam, result))
		return	(result);

	return		(DefWindowProc(hWnd, uMsg, wParam, lParam));
}