#pragma once

class CKinematics;

class SkeletonScheduler
{
public:
	SkeletonScheduler() = default;
	~SkeletonScheduler() = default;

	SkeletonScheduler(const SkeletonScheduler&) = delete;
	SkeletonScheduler& operator=(const SkeletonScheduler&) = delete;

	void Track(CKinematics* Skeleton);
	void Forget(CKinematics* Skeleton);

	void Launch();
	void Calculate();
	void Wait();

	bool IsRunning() const
	{
		return Running.load(std::memory_order_acquire);
	}

private:
	xrCriticalSection Lock;
	xr_vector<CKinematics*> Tracked;
	xr_vector<CKinematics*> Work;
	xr_atomic_bool Running = false;
};

extern SkeletonScheduler g_SkeletonScheduler;
