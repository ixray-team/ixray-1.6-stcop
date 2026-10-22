#include "stdafx.h"
#include "TaskPool.h"
#include "BackgroundLoad.h"

struct TaskPool::TaskNode
{
	TaskBody Body;
	xr_task_group* Group = nullptr;
	ETaskPriority Priority = ETaskPriority::Frame;
	TaskNode* Prev = nullptr;
	TaskNode* Next = nullptr;
	TaskNode* GroupPrev = nullptr;
	TaskNode* GroupNext = nullptr;
};

namespace
{
	using TaskPool::TaskNode;

	thread_local bool IsWorkerThread = false;

	struct ThreadAffinity
	{
		bool IsKept = false;
		u64 RoleMasks[3] = {};
		u64 WorkerMask = 0;
		u32 WorkerLogicalCount = 0;
	};

	constexpr u32 MinCoresToKeep = 6;

	const ThreadAffinity& GetThreadAffinity()
	{
		static const ThreadAffinity Affinity = []()
		{
			ThreadAffinity Result;
			Platform::CpuCore Cores[64];
			const u32 CoreCount = Platform::GetCpuCores(Cores, 64);
			u64 AllMask = 0;
			for (u32 Index = 0; Index < CoreCount; ++Index)
			{
				AllMask |= Cores[Index].LogicalMask;
			}

			if (CoreCount >= MinCoresToKeep)
			{
				std::stable_sort(Cores, Cores + CoreCount, [](const Platform::CpuCore& Left, const Platform::CpuCore& Right)
				{
					return Left.EfficiencyClass > Right.EfficiencyClass;
				});

				Result.IsKept = true;
				u64 KeptMask = 0;
				for (u32 Role = 0; Role < 3; ++Role)
				{
					const u64 Mask = Cores[Role].LogicalMask & (~Cores[Role].LogicalMask + 1);
					Result.RoleMasks[Role] = Mask;
					KeptMask |= Mask;
				}
				Result.WorkerMask = AllMask & ~KeptMask;
			}
			else
			{
				Result.WorkerMask = AllMask;
			}

			Result.WorkerLogicalCount = u32(std::popcount(Result.WorkerMask));
			if (Result.WorkerLogicalCount == 0)
			{
				Result.WorkerLogicalCount = std::max(Platform::GetCoresCount(), 1u);
			}
			return Result;
		}();
		return Affinity;
	}
}

class CTaskPool
{
public:
	void Start()
	{
		const ThreadAffinity& Affinity = GetThreadAffinity();
		const u32 Count = std::max<u32>(Affinity.WorkerLogicalCount, 2);
		IsExiting.store(false);
		Workers.reserve(Count);
		for (u32 Index = 0; Index < Count; ++Index)
		{
			Workers.push_back(thread_spawn(&CTaskPool::WorkerEntry, "X-Ray Worker", 0, this));
		}
		WorkerTotal.store(Count, std::memory_order_release);
		Msg("* Task pool: %u workers%s", Count, Affinity.IsKept ? ", 3 cores kept for the game threads" : "");
	}

	void Stop()
	{
		if (Workers.empty())
		{
			return;
		}

		WorkerTotal.store(0, std::memory_order_release);
		IsExiting.store(true);
		Signal.fetch_add(1, std::memory_order_release);
		Signal.notify_all();
		for (ThreadID Worker : Workers)
		{
			Platform::JoinThread(Worker);
		}
		Workers.clear();
	}

	u32 GetWorkerCount() const
	{
		return WorkerTotal.load(std::memory_order_acquire);
	}

	void Submit(xr_task_group& Group, TaskPool::TaskBody&& Body)
	{
		if (GetWorkerCount() == 0)
		{
			Body();
			return;
		}

		TaskNode* Node = new TaskNode();
		Node->Body = std::move(Body);
		Node->Group = &Group;
		Node->Priority = Group.Priority;
		Group.Pending.fetch_add(1, std::memory_order_relaxed);
		{
			xrCriticalSectionGuard Guard(Lock);
			Queue& Target = Queues[u32(Node->Priority)];
			Node->Prev = Target.Tail;
			(Target.Tail ? Target.Tail->Next : Target.Head) = Node;
			Target.Tail = Node;

			Node->GroupPrev = Group.QueuedTail;
			(Group.QueuedTail ? Group.QueuedTail->GroupNext : Group.QueuedHead) = Node;
			Group.QueuedTail = Node;
		}
		Wake();
	}

	void Wait(xr_task_group& Group)
	{
		if (Group.Pending.load(std::memory_order_acquire) == 0)
		{
			return;
		}

		PROF_EVENT("TaskPool::Wait");
		while (true)
		{
			TaskNode* Node = nullptr;
			{
				xrCriticalSectionGuard Guard(Lock);
				Node = Group.QueuedHead;
				if (Node != nullptr)
				{
					Unlink(Node);
				}
			}

			if (Node != nullptr)
			{
				Run(Node, false);
				continue;
			}

			const u32 Pending = Group.Pending.load(std::memory_order_acquire);
			if (Pending == 0)
			{
				break;
			}
			Group.Pending.wait(Pending, std::memory_order_acquire);
		}

		xrCriticalSectionGuard Guard(Lock);
	}

private:
	struct Queue
	{
		TaskNode* Head = nullptr;
		TaskNode* Tail = nullptr;
	};

	static void WorkerEntry(void* Context)
	{
		IsWorkerThread = true;
		static std::atomic<u32> WorkerNumber = 0;
		string64 Name;
		xr_sprintf(Name, "X-Ray Worker %u", ++WorkerNumber);
		Platform::SetThreadName(Name);
		PROF_START_THREAD(Name);
		((CTaskPool*)Context)->WorkerLoop();
		PROF_STOP_THREAD();
	}

	void WorkerLoop()
	{
		while (true)
		{
			const u32 Epoch = Signal.load(std::memory_order_acquire);
			if (TaskNode* Node = Pop())
			{
				Run(Node, Node->Priority == ETaskPriority::Background);
				continue;
			}

			if (IsExiting.load())
			{
				return;
			}
			Signal.wait(Epoch, std::memory_order_acquire);
		}
	}

	void Wake()
	{
		Signal.fetch_add(1, std::memory_order_release);
		Signal.notify_one();
	}

	u32 BackgroundLimit() const
	{
		const u32 Total = GetWorkerCount();
		return std::max<u32>(1, std::min<u32>(BackgroundLoad::WorkerCount(), Total > 1 ? Total - 1 : 1));
	}

	void Unlink(TaskNode* Node)
	{
		Queue& Source = Queues[u32(Node->Priority)];
		(Node->Prev ? Node->Prev->Next : Source.Head) = Node->Next;
		(Node->Next ? Node->Next->Prev : Source.Tail) = Node->Prev;

		xr_task_group& Group = *Node->Group;
		(Node->GroupPrev ? Node->GroupPrev->GroupNext : Group.QueuedHead) = Node->GroupNext;
		(Node->GroupNext ? Node->GroupNext->GroupPrev : Group.QueuedTail) = Node->GroupPrev;
	}

	TaskNode* Pop()
	{
		xrCriticalSectionGuard Guard(Lock);
		if (TaskNode* Node = Queues[u32(ETaskPriority::Frame)].Head)
		{
			Unlink(Node);
			return Node;
		}

		TaskNode* Node = Queues[u32(ETaskPriority::Background)].Head;
		if (Node != nullptr && RunningBackground < BackgroundLimit())
		{
			RunningBackground++;
			Unlink(Node);
			return Node;
		}
		return nullptr;
	}

	void Run(TaskNode* Node, bool IsCountedBackground)
	{
		Node->Body();
		xr_task_group& Group = *Node->Group;
		xr_delete(Node);

		bool IsBackgroundWaiting = false;
		{
			xrCriticalSectionGuard Guard(Lock);
			if (IsCountedBackground)
			{
				RunningBackground--;
				IsBackgroundWaiting = Queues[u32(ETaskPriority::Background)].Head != nullptr;
			}

			if (Group.Pending.fetch_sub(1, std::memory_order_acq_rel) == 1)
			{
				Group.Pending.notify_all();
			}
		}

		if (IsBackgroundWaiting)
		{
			Wake();
		}
	}

	xrCriticalSection Lock;
	Queue Queues[2];
	u32 RunningBackground = 0;
	std::atomic<u32> Signal = 0;
	std::atomic<u32> WorkerTotal = 0;
	std::atomic<bool> IsExiting = false;
	xr_vector<ThreadID> Workers;
};

namespace
{
	CTaskPool& GetPool()
	{
		static CTaskPool Pool;
		return Pool;
	}
}

void TaskPool::Initialize()
{
	ApplyThreadRole(EThreadRole::Primary);
	GetPool().Start();
}

void TaskPool::Shutdown()
{
	GetPool().Stop();
}

u32 TaskPool::WorkerCount()
{
	return GetPool().GetWorkerCount();
}

bool TaskPool::IsWorker()
{
	return IsWorkerThread;
}

bool TaskPool::RunsLoopsInPlace()
{
	return BackgroundLoad::IsWorker();
}

void TaskPool::ApplyThreadRole(EThreadRole Role)
{
	const ThreadAffinity& Affinity = GetThreadAffinity();
	if (!Affinity.IsKept)
	{
		return;
	}

	Platform::SetCurrentThreadAffinity(Role == EThreadRole::System ? Affinity.WorkerMask : Affinity.RoleMasks[u32(Role)]);
}

void TaskPool::Submit(xr_task_group& Group, TaskBody&& Body)
{
	GetPool().Submit(Group, std::move(Body));
}

void TaskPool::Wait(xr_task_group& Group)
{
	GetPool().Wait(Group);
}
