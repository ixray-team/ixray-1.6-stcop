#pragma once

class XRCORE_API CAppInfo
{
public:
	SDL_Window* Window = nullptr;
#ifdef IXR_WINDOWS
	bool IsLaunchedViaWineOrProton = false;
#endif

	ThreadID MainThread = NULL;
	size_t MainThreadId = 0;

public:
	void SetMainThread();
	bool IsPrimaryThread() const noexcept;

	void SetAsyncLoadThread(bool IsActive);
	bool IsAsyncLoadThread() const noexcept;

private:
	std::atomic<size_t> AsyncLoadThreadId = 0;
};

extern XRCORE_API CAppInfo g_AppInfo;
