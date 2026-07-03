#include "stdafx.h"

XRayWorkerThread::XRayWorkerThread(Callback InCallback, const char* TN)
	: Function(std::move(InCallback)), ThreadName(TN)
{
	thread_spawn(&XRayWorkerThread::ThreadProc, ThreadName.c_str(), 0, this);
}

XRayWorkerThread::~XRayWorkerThread()
{
	Stop();
}

void XRayWorkerThread::Run()
{
	Counter.fetch_add(1, std::memory_order_release);
	Counter.notify_one();
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
	MustExit = true;
	Counter.fetch_add(1, std::memory_order_release);
	Counter.notify_one();
}

void XRayWorkerThread::ThreadProc(void* InThis)
{
	XRayWorkerThread* This = (XRayWorkerThread*)InThis;
	PROF_START_THREAD(This->ThreadName.c_str());

	while (true)
	{
		while (This->Counter.load(std::memory_order_acquire) == 0)
		{
			This->Counter.wait(0);
		}

		if (This->MustExit)
		{
			break;
		}

		This->Function();

		This->Counter.fetch_sub(1, std::memory_order_release);
	}

	PROF_STOP_THREAD();
}