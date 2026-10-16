#pragma once

extern XRCORE_API int g_BackgroundLoadWorkers;
extern XRCORE_API bool g_BackgroundLoadPriority;

namespace BackgroundLoad
{
	XRCORE_API u32 WorkerCount();

	XRCORE_API bool IsWorker();
}

class XRCORE_API xrBackgroundWorkScope final
{
public:
	xrBackgroundWorkScope();
	~xrBackgroundWorkScope();

	xrBackgroundWorkScope(const xrBackgroundWorkScope&) = delete;
	xrBackgroundWorkScope& operator=(const xrBackgroundWorkScope&) = delete;

private:
	bool WasWorker = false;
	bool IsBackgroundMode = false;
};
