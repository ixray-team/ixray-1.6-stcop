#pragma once

#include <exception>

// A synchronous phase borrows its context until Run returns. Only the physics owner dispatches work.
class CPHWorkerPool final
{
public:
	static constexpr u32 MaxWorkers = 64;
	using WorkFunction = void (*)(void* Context, u32 Worker);

private:
	class Worker final
	{
		CPHWorkerPool& Pool;
		u32 Index;
		string64 Name{};
		ThreadID Thread = 0;
		xr_atomic_u32 Generation{0};
		xr_atomic_bool Exit{false};

		static void ThreadProc(void* Context);

	public:
		Worker(CPHWorkerPool& Owner, u32 WorkerIndex);
		~Worker();
		bool IsAvailable() const;
		void Wake();
	};

	xr_vector<xr_unique_ptr<Worker>> Workers;
	xr_atomic_u32 Pending{0};
	WorkFunction Function = nullptr;
	void* Context = nullptr;
	xrCriticalSection FailureLock;
	std::exception_ptr Failure;

	void Execute(u32 Worker);
	void RememberFailure();
	void Wait();

public:
	CPHWorkerPool() = default;
	~CPHWorkerPool();
	CPHWorkerPool(const CPHWorkerPool&) = delete;
	CPHWorkerPool& operator=(const CPHWorkerPool&) = delete;

	void Run(u32 Count, WorkFunction Work, void* WorkContext, WorkFunction OwnerWork = nullptr);
	void Stop();
};
