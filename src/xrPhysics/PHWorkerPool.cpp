#include "StdAfx.h"
#include "PHWorkerPool.h"
#include <utility>

CPHWorkerPool::Worker::Worker(CPHWorkerPool& Owner, u32 WorkerIndex) : Pool(Owner), Index(WorkerIndex)
{
	xr_sprintf(Name, "Physics Worker %u", Index + 1);
	Thread = thread_spawn(&ThreadProc, Name, 0, this);
}

CPHWorkerPool::Worker::~Worker()
{
	Exit.store(true, std::memory_order_release);
	Wake();
	if (IsAvailable())
	{
		Platform::JoinThread(Thread);
	}
}

bool CPHWorkerPool::Worker::IsAvailable() const
{
	return Thread != 0;
}

void CPHWorkerPool::Worker::Wake()
{
	Generation.fetch_add(1, std::memory_order_release);
	Generation.notify_one();
}

void CPHWorkerPool::Worker::ThreadProc(void* Context)
{
	Worker& Current = *static_cast<Worker*>(Context);
	PROF_THREAD(Current.Name);
	u32 Observed = 0;
	for (;;)
	{
		const u32 Requested = Current.Generation.load(std::memory_order_acquire);
		if (Requested == Observed)
		{
			Current.Generation.wait(Observed, std::memory_order_acquire);
			continue;
		}
		Observed = Requested;
		if (Current.Exit.load(std::memory_order_acquire))
		{
			return;
		}
		Current.Pool.Execute(Current.Index);
		if (Current.Pool.Pending.fetch_sub(1, std::memory_order_acq_rel) == 1)
		{
			Current.Pool.Pending.notify_one();
		}
	}
}

CPHWorkerPool::~CPHWorkerPool()
{
	Stop();
}

void CPHWorkerPool::RememberFailure()
{
	xrCriticalSectionGuard Guard(FailureLock);
	if (!Failure)
	{
		Failure = std::current_exception();
	}
}

void CPHWorkerPool::Execute(u32 Worker)
{
	try
	{
		Function(Context, Worker);
	}
	catch (...)
	{
		RememberFailure();
	}
}

void CPHWorkerPool::Wait()
{
	u32 Count = Pending.load(std::memory_order_acquire);
	if (!Count)
	{
		return;
	}
	PROF_EVENT("PH Worker Wait");
	do
	{
		Pending.wait(Count, std::memory_order_acquire);
	} while ((Count = Pending.load(std::memory_order_acquire)) != 0);
}

void CPHWorkerPool::Run(u32 Count, WorkFunction Work, void* WorkContext, WorkFunction OwnerWork)
{
	if (!Work || !Count)
	{
		return;
	}
	CHECK_OR_EXIT(!Function && Pending.load(std::memory_order_acquire) == 0, "A physics worker phase cannot be dispatched recursively");
	Count = std::min(Count, MaxWorkers);
	const u32 RequestedBackground = OwnerWork ? Count : Count - 1;
	while (Workers.size() < RequestedBackground)
	{
		auto Current = xr_make_unique<Worker>(*this, (u32)Workers.size());
		if (!Current->IsAvailable())
		{
			break;
		}
		Workers.push_back(std::move(Current));
	}
	const u32 Background = std::min(RequestedBackground, (u32)Workers.size());
	Function = Work;
	Context = WorkContext;
	Failure = nullptr;
	Pending.store(Background, std::memory_order_release);
	for (u32 Index = 0; Index < Background; ++Index)
	{
		Workers[Index]->Wake();
	}
	// The owner contributes work directly instead of entering a task scheduler's wait path.
	if (!OwnerWork || !Background)
	{
		Execute(Background);
	}
	if (OwnerWork)
	{
		try
		{
			OwnerWork(Context, Background);
		}
		catch (...)
		{
			RememberFailure();
		}
	}
	Wait();
	Function = nullptr;
	Context = nullptr;
	if (Failure)
	{
		std::exception_ptr Error = std::exchange(Failure, nullptr);
		std::rethrow_exception(Error);
	}
}

void CPHWorkerPool::Stop()
{
	if (Workers.empty())
	{
		return;
	}
	Wait();
	Workers.clear();
}
