#pragma once
#include <functional>


enum class ETaskPriority : u8
{
	Frame,
	Background
};

enum class EThreadRole : u8
{
	Primary,
	Game,
	PreRender,
	System
};

class xr_task_group;

namespace TaskPool
{
	using TaskBody = std::move_only_function<void()>;
	struct TaskNode;

	XRCORE_API void Initialize();
	XRCORE_API void Shutdown();

	XRCORE_API u32 WorkerCount();
	XRCORE_API bool IsWorker();
	XRCORE_API bool RunsLoopsInPlace();
	XRCORE_API void ApplyThreadRole(EThreadRole Role);

	XRCORE_API void Submit(xr_task_group& Group, TaskBody&& Body);
	XRCORE_API void Wait(xr_task_group& Group);
}

class XRCORE_API xr_task_group
{
public:
	explicit xr_task_group(ETaskPriority InPriority = ETaskPriority::Frame)
		: Priority(InPriority)
	{
	}

	~xr_task_group()
	{
		wait();
	}

	xr_task_group(const xr_task_group&) = delete;
	xr_task_group& operator=(const xr_task_group&) = delete;

	template<typename Callable>
	void run(Callable&& Body)
	{
		TaskPool::Submit(*this, TaskPool::TaskBody(std::forward<Callable>(Body)));
	}

	void wait()
	{
		TaskPool::Wait(*this);
	}

	ETaskPriority GetPriority() const
	{
		return Priority;
	}

private:
	friend struct TaskPool::TaskNode;
	friend class CTaskPool;

	ETaskPriority Priority;
	std::atomic<u32> Pending = 0;
	TaskPool::TaskNode* QueuedHead = nullptr;
	TaskPool::TaskNode* QueuedTail = nullptr;
};

inline size_t xr_max_concurrency()
{
	return size_t(TaskPool::WorkerCount()) + 1;
}

template<typename IndexType, typename Body>
inline void xr_parallel_for(IndexType Begin, IndexType End, IndexType Grain, Body Functor)
{
	if (!(Begin < End))
	{
		return;
	}

	const size_t Count = size_t(End - Begin);
	const size_t Workers = TaskPool::WorkerCount();
	if (Count == 1 || Workers == 0 || TaskPool::RunsLoopsInPlace())
	{
		for (IndexType Index = Begin; Index < End; ++Index)
		{
			Functor(Index);
		}
		return;
	}

	const size_t Chunk = std::max<size_t>(std::max<size_t>(size_t(Grain), 1), Count / ((Workers + 1) * 4));
	const size_t Chunks = (Count + Chunk - 1) / Chunk;
	std::atomic<size_t> Next = 0;
	auto RunChunks = [&]()
	{
		for (size_t Start = Next.fetch_add(Chunk); Start < Count; Start = Next.fetch_add(Chunk))
		{
			const size_t Stop = std::min(Start + Chunk, Count);
			for (size_t Offset = Start; Offset < Stop; ++Offset)
			{
				Functor(IndexType(Begin + IndexType(Offset)));
			}
		}
	};

	xr_task_group Group;
	const size_t Helpers = std::min(Workers, Chunks - 1);
	for (size_t Helper = 0; Helper < Helpers; ++Helper)
	{
		Group.run([&RunChunks]() { RunChunks(); });
	}
	RunChunks();
	Group.wait();
}

template<typename IndexType, typename Body>
inline void xr_parallel_for(IndexType Begin, IndexType End, Body Functor)
{
	xr_parallel_for(Begin, End, IndexType(1), std::move(Functor));
}

template<typename Iterator, typename Body>
inline void xr_parallel_foreach(Iterator Begin, Iterator End, Body Functor)
{
	xr_vector<Iterator> Items;
	for (Iterator Item = Begin; Item != End; ++Item)
	{
		Items.push_back(Item);
	}
	xr_parallel_for(size_t(0), Items.size(), [&](size_t Index) { Functor(*Items[Index]); });
}
