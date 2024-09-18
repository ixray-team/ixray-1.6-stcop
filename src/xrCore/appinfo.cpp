#include "stdafx.h"
#include "appinfo.h"

XRCORE_API CAppInfo g_AppInfo;

bool CAppInfo::IsSecondaryThread() const noexcept
{
	return SecondaryThread == Platform::GetCurrentThread();
}

bool CAppInfo::IsPrimaryThread() const noexcept
{
	return MainThread == Platform::GetCurrentThread();
}

HWND CAppInfo::GetHWND() const noexcept
{
	if (Window == nullptr)
		return nullptr;

	SDL_SysWMinfo wmInfo;
	SDL_VERSION(&wmInfo.version);

	return (SDL_GetWindowWMInfo(Window, &wmInfo) ? wmInfo.info.win.window : nullptr);
}