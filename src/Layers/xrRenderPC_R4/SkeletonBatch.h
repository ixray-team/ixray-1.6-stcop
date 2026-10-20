#pragma once

class CKinematics;

// Calculates the skeletons of one render pass on worker threads.
// Launch advances animation tracks on the calling thread in submission order, because blend
// callbacks reach game and script code. Bone hierarchies, bone callbacks and visual callbacks
// (IK) of different skeletons then run in parallel until Wait, where the caller takes the rest.
class SkeletonBatch
{
public:
	SkeletonBatch() = default;
	// HACK: must be noexcept - R_dsgraph_structure holds this by value and overrides the noexcept
	// HACK: destructors of IRender_interface/pureFrame on compilers that enforce the override (clang).
	~SkeletonBatch() noexcept;

	SkeletonBatch(const SkeletonBatch&) = delete;
	SkeletonBatch& operator=(const SkeletonBatch&) = delete;

	// Ignores skeletons that are already calculated this frame or already added
	void Add(CKinematics* Skeleton);
	void Launch();
	void Wait();

private:
	void Calculate();
	void RunWorker();

private:
	xr_vector<CKinematics*> Skeletons;
	xr_atomic_u32 NextSkeleton = 0;
	xr_task_group Tasks;
	std::thread::id LaunchThread;
	u32 Marker = 0;
	bool Running = false;
};
