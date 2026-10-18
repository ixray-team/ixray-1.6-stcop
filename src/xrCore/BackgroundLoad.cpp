#include "stdafx.h"
#include "BackgroundLoad.h"

XRCORE_API int g_BackgroundLoadWorkers = 0;
XRCORE_API bool g_BackgroundLoadPriority = false;

namespace
{
	thread_local bool IsWorkerThread = false;
	std::atomic<EBackgroundLoadStage> Stage = EBackgroundLoadStage::Gameplay;
}

u32 BackgroundLoad::WorkerCount()
{
	const u32 Limit = g_BackgroundLoadWorkers > 0 ? u32(g_BackgroundLoadWorkers) : std::max<u32>(u32(xr_max_concurrency()) / 2, 2);
	if (Stage.load(std::memory_order_relaxed) == EBackgroundLoadStage::FinishLoading)
	{
		return std::max<u32>(Limit, u32(xr_max_concurrency()));
	}
	return Limit;
}

void BackgroundLoad::SetStage(EBackgroundLoadStage NewStage)
{
	Stage.store(NewStage, std::memory_order_relaxed);
}

EBackgroundLoadStage BackgroundLoad::GetStage()
{
	return Stage.load(std::memory_order_relaxed);
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
