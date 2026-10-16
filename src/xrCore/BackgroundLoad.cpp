#include "stdafx.h"
#include "BackgroundLoad.h"

XRCORE_API int g_BackgroundLoadWorkers = 0;
XRCORE_API bool g_BackgroundLoadPriority = true;

namespace
{
	thread_local bool IsWorkerThread = false;
}

u32 BackgroundLoad::WorkerCount()
{
	if (g_BackgroundLoadWorkers > 0)
	{
		return u32(g_BackgroundLoadWorkers);
	}
	return std::max<u32>(u32(xr_max_concurrency()) / 2, 2);
}

bool BackgroundLoad::IsWorker()
{
	return IsWorkerThread;
}

xrBackgroundWorkScope::xrBackgroundWorkScope()
	: WasWorker(IsWorkerThread)
{
	IsWorkerThread = true;

	if (!WasWorker && g_BackgroundLoadPriority)
	{
		Platform::SetCurrentThreadBackgroundMode(true);
		IsBackgroundMode = true;
	}
}

xrBackgroundWorkScope::~xrBackgroundWorkScope()
{
	if (IsBackgroundMode)
	{
		Platform::SetCurrentThreadBackgroundMode(false);
	}
	IsWorkerThread = WasWorker;
}
