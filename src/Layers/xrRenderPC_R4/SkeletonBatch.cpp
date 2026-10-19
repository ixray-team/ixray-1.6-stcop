#include "stdafx.h"

#include "SkeletonBatch.h"
#include "SkeletonCustom.h"

// Batches are filled on the rendering thread only
static u32 g_SkeletonBatchMarker = 0;

// Pool threads are registered in the profiler once. A thread scope per task would add a thread
// entry to the capture every time a worker starts.
static thread_local bool g_SkeletonWorkerProfiled = false;

static u32 SkeletonWorkerLimit()
{
	static const u32 Limit = std::max<u32>(u32(xr_max_concurrency()), 2) - 1;
	return Limit;
}

SkeletonBatch::~SkeletonBatch() noexcept
{
	Wait();
}

void SkeletonBatch::Add(CKinematics* Skeleton)
{
	VERIFY(!Running);
	if (Skeletons.empty())
	{
		// Zero is the marker of skeletons that were never added
		if (++g_SkeletonBatchMarker == 0)
		{
			++g_SkeletonBatchMarker;
		}
		Marker = g_SkeletonBatchMarker;
	}

	if (Skeleton->BatchMarker == Marker || Skeleton->UCalc_Time == RDEVICE.dwTimeGlobal)
	{
		return;
	}

	Skeleton->BatchMarker = Marker;
	Skeletons.push_back(Skeleton);
}

void SkeletonBatch::Launch()
{
	VERIFY(!Running);
	if (Skeletons.empty())
	{
		return;
	}

	PROF_EVENT("SkeletonBatch::Launch");
	for (CKinematics* Skeleton : Skeletons)
	{
		Skeleton->PrepareBones();
	}

	NextSkeleton.store(0, std::memory_order_relaxed);
	LaunchThread = std::this_thread::get_id();
	Running = true;

	if (!ps_r__common_flags.test(RFLAG_MT_SKELETONS))
	{
		return;
	}

	// One task on the caller: the workers are spread by the scheduler
	const u32 WorkerCount = std::min(u32(Skeletons.size()), SkeletonWorkerLimit());
	Tasks.run([this, WorkerCount]()
	{
		xr_parallel_for(u32(0), WorkerCount, [this](u32)
		{
			RunWorker();
		});
	});
}

void SkeletonBatch::Wait()
{
	if (!Running)
	{
		return;
	}

	PROF_EVENT("SkeletonBatch::Wait");
	Calculate();
	Tasks.wait();

	Skeletons.clear();
	Running = false;
}

void SkeletonBatch::Calculate()
{
	const u32 Count = u32(Skeletons.size());
	for (u32 Index = NextSkeleton.fetch_add(1, std::memory_order_relaxed); Index < Count; Index = NextSkeleton.fetch_add(1, std::memory_order_relaxed))
	{
		Skeletons[Index]->CalculateBones(true);
	}
}

void SkeletonBatch::RunWorker()
{
	if (NextSkeleton.load(std::memory_order_relaxed) >= u32(Skeletons.size()))
	{
		return;
	}

	// A worker task may run inline on the launching thread inside Wait, which is registered already
	if (!g_SkeletonWorkerProfiled && std::this_thread::get_id() != LaunchThread)
	{
		g_SkeletonWorkerProfiled = true;
		PROF_REGISTER_THREAD("Skeleton worker");
	}

	PROF_EVENT("SkeletonBatch::RunWorker");
	Calculate();
}
