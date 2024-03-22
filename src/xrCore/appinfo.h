#pragma once

class XRCORE_API CAppInfo
{
public:
	SDL_Window* Window = nullptr;

	HANDLE MainThread = nullptr;
	HANDLE SecondaryThread = nullptr;

public:
	bool IsSecondaryThread() const noexcept;
	bool IsPrimaryThread() const noexcept;
};

extern XRCORE_API CAppInfo g_AppInfo;