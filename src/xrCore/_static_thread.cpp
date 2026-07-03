#include "stdafx.h"

XRayWorkerThread::XRayWorkerThread(Callback InCallback, const char* TN)
	: Function(std::move(InCallback)), ThreadName(TN)
{
	Worker = std::thread(&XRayWorkerThread::ThreadProc, this);
}

XRayWorkerThread::~XRayWorkerThread()
{
	Stop();
}

void XRayWorkerThread::Run()
{
	Counter.fetch_add(1);
}

void XRayWorkerThread::Wait()
{
	while (Counter.load(std::memory_order_acquire) > 0)
	{
		std::this_thread::yield();
	}
}

void XRayWorkerThread::Stop()
{
	if (!Worker.joinable())
	{
		return;
	}

	MustExit = true;

	Counter.fetch_add(1, std::memory_order_release);
	Worker.join();
}

void XRayWorkerThread::ThreadProc()
{
	Platform::SetThreadName(ThreadName.c_str());
	PROF_START_THREAD(ThreadName.c_str());

	while (true)
	{
		if (Counter.load(std::memory_order_acquire) == 0)
		{
			std::this_thread::yield();
			continue;
		}

		if (MustExit)
		{
			break;
		}

		Function();

		Counter.fetch_sub(1, std::memory_order_release);
	}

	PROF_STOP_THREAD();
}