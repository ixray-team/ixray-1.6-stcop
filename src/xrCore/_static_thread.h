#pragma once

#include <thread>
#include <functional>
#include <mutex>

class XRCORE_API XRayWorkerThread
{
public:
	using Callback = std::function<void()>;

public:
	explicit XRayWorkerThread(Callback InCallback, const char* TN);
	~XRayWorkerThread();

	XRayWorkerThread(const XRayWorkerThread&) = delete;
	XRayWorkerThread& operator=(const XRayWorkerThread&) = delete;

public:
	void Run();
	void Wait();
	void Stop();

private:
	void ThreadProc();

private:
	std::thread Worker;
	Callback Function;

	bool MustExit = false;

	xr_string ThreadName;
	xr_atomic_u32 Counter = 0;
};