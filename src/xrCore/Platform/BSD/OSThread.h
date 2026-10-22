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
	struct CpuCore
	{
		unsigned long long LogicalMask;
		unsigned char EfficiencyClass;
	};
	inline unsigned int GetCpuCores(CpuCore*, unsigned int) { return 0; }
	inline void SetCurrentThreadAffinity(unsigned long long) {}

	unsigned int GetCoresCount();
	ThreadID GetCurrentThread();
    size_t GetThreadId(ThreadID ID);
    void SetThreadName(const char* name);

    inline void SetCurrentThreadBackgroundMode(bool)
    {
    }
}
