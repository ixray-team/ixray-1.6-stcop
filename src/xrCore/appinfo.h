#pragma once

struct SDL_Window;

class XRCORE_API CAppInfo
{
public:
	SDL_Window* Window = nullptr;

	ThreadID MainThread = nullptr;
	ThreadID SecondaryThread = nullptr;

public:
	bool IsSecondaryThread() const noexcept;
	bool IsPrimaryThread() const noexcept;
};

extern XRCORE_API CAppInfo g_AppInfo;