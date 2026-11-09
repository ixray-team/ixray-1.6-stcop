#include "stdafx.h"

#include "SkeletonScheduler.h"
#include "SkeletonCustom.h"

SkeletonScheduler g_SkeletonScheduler;

static thread_local bool g_OnSkeletonThread = false;

void SkeletonScheduler::Track(CKinematics* Skeleton)
{
	if (Device.IsEditorMode())
	{
		return;
	}

	const u32 Frame = RDEVICE.dwFrame;
	if (Skeleton->TrackFrame.load(std::memory_order_relaxed) == Frame)
	{
		return;
	}

	xrCriticalSectionGuard Guard(Lock);
	if (Skeleton->TrackFrame.load(std::memory_order_relaxed) == Frame)
	{
		return;
	}

	Skeleton->TrackFrame.store(Frame, std::memory_order_relaxed);
	Tracked.push_back(Skeleton);
}

void SkeletonScheduler::Forget(CKinematics* Skeleton)
{
	if (Skeleton->TrackFrame.load(std::memory_order_relaxed) == 0 && Skeleton->ScheduledTime.load(std::memory_order_relaxed) == 0)
	{
		return;
	}

	VERIFY2(!g_OnSkeletonThread, "A skeleton is freed while the skeleton thread calculates skeletons");
	if (IsRunning() && !g_OnSkeletonThread)
	{
		Device.SkeletonThread.Wait();
	}

	xrCriticalSectionGuard Guard(Lock);
	if (Skeleton->TrackFrame.load(std::memory_order_relaxed) != 0)
	{
		std::erase(Tracked, Skeleton);
		Skeleton->TrackFrame.store(0, std::memory_order_relaxed);
	}

	if (Skeleton->ScheduledTime.load(std::memory_order_relaxed) != 0)
	{
		std::replace(Work.begin(), Work.end(), Skeleton, static_cast<CKinematics*>(nullptr));
		Skeleton->ScheduledTime.store(0, std::memory_order_relaxed);
	}
}

void SkeletonScheduler::Launch()
{
	VERIFY(!IsRunning());
	VERIFY(!g_AppInfo.MainThread || g_AppInfo.IsPrimaryThread());
	{
		xrCriticalSectionGuard Guard(Lock);
		Work.swap(Tracked);
		Tracked.clear();
		for (CKinematics* Skeleton : Work)
		{
			Skeleton->TrackFrame.store(0, std::memory_order_relaxed);
		}
	}

	if (Work.empty() || Device.IsEditorMode() || !ps_r__common_flags.test(RFLAG_MT_SKELETONS))
	{
		Work.clear();
		return;
	}

	PROF_EVENT("SkeletonScheduler::Launch");
	const u32 Time = RDEVICE.dwTimeGlobal;
	for (CKinematics* Skeleton : Work)
	{
		Skeleton->ScheduledTime.store(Time, std::memory_order_relaxed);
	}

	Running.store(true, std::memory_order_release);

	for (size_t Index = 0; Index < Work.size(); ++Index)
	{
		if (CKinematics* Skeleton = Work[Index])
		{
			Skeleton->PrepareBones();
		}
	}

	Device.SkeletonThread.Run();
}

void SkeletonScheduler::Calculate()
{
	if (!IsRunning())
	{
		return;
	}

	PROF_EVENT("SkeletonScheduler::Calculate");
	g_OnSkeletonThread = true;
	for (CKinematics* Skeleton : Work)
	{
		if (Skeleton != nullptr)
		{
			Skeleton->CalculateBones(true);
		}
	}
	g_OnSkeletonThread = false;
}

void SkeletonScheduler::Wait()
{
	if (!IsRunning())
	{
		return;
	}

	VERIFY(!g_AppInfo.MainThread || g_AppInfo.IsPrimaryThread());
	PROF_EVENT("SkeletonScheduler::Wait");
	Device.SkeletonThread.Wait();

	xrCriticalSectionGuard Guard(Lock);
	for (CKinematics* Skeleton : Work)
	{
		if (Skeleton != nullptr)
		{
			Skeleton->ScheduledTime.store(0, std::memory_order_relaxed);
		}
	}
	Work.clear();
	Running.store(false, std::memory_order_release);
}
