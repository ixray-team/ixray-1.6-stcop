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
	// The thread that owns the window, its messages and the game's frame; or the one it has handed that frame to and is
	// waiting for (SetPrimaryDelegate), as only one of the two runs then
	bool IsPrimaryThread() const noexcept;
	void SetPrimaryDelegate(bool IsActive);

	// The thread that may touch the graphics device this frame: the primary one unless a render thread has taken it
	void SetRenderThread(bool IsActive);
	bool IsRenderThread() const noexcept;

	void SetAsyncLoadThread(bool IsActive);
	bool IsAsyncLoadThread() const noexcept;

private:
	std::atomic<size_t> AsyncLoadThreadId = 0;
	std::atomic<size_t> RenderThreadId = 0;
	std::atomic<size_t> PrimaryDelegateId = 0;
};

extern XRCORE_API CAppInfo g_AppInfo;
