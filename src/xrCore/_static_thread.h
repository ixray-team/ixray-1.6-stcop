#pragma once

#include <thread>
#include <functional>
#include <mutex>

class XRCORE_API XRayWorkerThread
{
public:
	using Callback = std::function<void()>;

public:
	explicit XRayWorkerThread(Callback InCallback, const char* TN, EThreadRole InRole = EThreadRole::System);
	~XRayWorkerThread();

	XRayWorkerThread(const XRayWorkerThread&) = delete;
	XRayWorkerThread& operator=(const XRayWorkerThread&) = delete;

public:
	void Run();
	void Wait();
	void Stop();

private:
	static void ThreadProc(void*);

private:
	Callback Function;

	volatile bool MustExit = false;

	xr_string ThreadName;
	EThreadRole Role = EThreadRole::System;
	xr_atomic_u32 Counter = 0;
	ThreadID ThreadHandle = 0;
};