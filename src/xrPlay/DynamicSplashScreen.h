#pragma once
extern HWND logoWindow;

IStream* CreateStreamOnResource(LPCTSTR lpName, LPCTSTR lpType);
void RegisterWindowClass(HINSTANCE hInst, bool nCmdShow);
HWND CreateSplashWindow(HINSTANCE hInst);
HWND WINAPI ShowSplash(HINSTANCE hInstance, int nCmdShow);