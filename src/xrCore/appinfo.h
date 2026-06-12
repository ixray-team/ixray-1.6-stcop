#pragma once

class XRCORE_API CAppInfo
{
public:
	SDL_Window* Window = nullptr;
#ifdef IXR_WINDOWS
	bool IsLaunchedViaWineOrProton = false;
#endif

	ThreadID MainThread = NULL;
	ThreadID SecondaryThread = NULL;
 

public:
	bool IsSecondaryThread() const noexcept;
	bool IsPrimaryThread() const noexcept;
};

extern XRCORE_API CAppInfo g_AppInfo;