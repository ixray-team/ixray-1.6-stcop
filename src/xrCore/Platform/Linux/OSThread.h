#pragma once
#include <pthread.h>
#include <chrono>
#include <thread>

using ThreadID = pthread_t;

inline void Sleep(size_t Time)
{
    std::this_thread::sleep_for(std::chrono::milliseconds(Time));
}

namespace Platform
{
	unsigned int GetCoresCount();
	ThreadID GetCurrentThread();
    size_t GetThreadId(ThreadID ID);
}