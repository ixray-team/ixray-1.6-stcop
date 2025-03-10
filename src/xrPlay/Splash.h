////////////////////////////////////////////////////////////////////////////
//	Name		: Splash.h
//	Created 	: 09.02.2021
////////////////////////////////////////////////////////////////////////////
#pragma once

#define WM_FADIN (WM_USER + 1)
#define WM_FADEOUT (WM_USER + 2)

int WINAPI ShowSplash( HINSTANCE hInstance, int nCmdShow, HANDLE *g_hEventSplashReady, HWND* g_hWndSplash/*, u32 BuildNumber*/);