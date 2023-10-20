#include "stdafx.h"
#include "Text_Console.h"

LRESULT CALLBACK TextConsole_WndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
	switch(uMsg)
	{
	case WM_PAINT:
		{
		}break;
	case  WM_ERASEBKGND:
		{
		}break;
	case WM_NCPAINT :
		{
		}break;
	default:
		break;
	}
	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}

LRESULT CALLBACK TextConsole_LogWndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
	switch(uMsg)
	{
	case WM_ERASEBKGND:
		return (LRESULT)1; // Say we handled it.

	case WM_PAINT:
		{
			CTextConsole* pTextConsole = (CTextConsole*)Console;
			pTextConsole->OnPaint();
			return (LRESULT)0; // Say we handled it.
		}break;
	default:
		break;
	}
	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}