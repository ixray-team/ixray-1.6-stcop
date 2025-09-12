#pragma once
#include <pthread.h>
#include <chrono>
#include <thread>
#include <sched.h>

using ThreadID = pthread_t;

inline void Sleep(size_t Time)
{
    std::this_thread::sleep_for(std::chrono::milliseconds(Time));
}

namespace Platform
{
	XRCORE_API unsigned int GetCoresCount();
	XRCORE_API ThreadID GetCurrentThread();
    XRCORE_API size_t GetThreadId(ThreadID ID);
    XRCORE_API void SetThreadName(const char* name);
    XRCORE_API long AtomicCompareExchange(long* ptr, long expected, long desired);

    inline void SetCurrentThreadHighPriority()
    {
        pthread_t thread = pthread_self();

        sched_param sch_params;
        sch_params.sched_priority = sched_get_priority_max(SCHED_FIFO);

        if (pthread_setschedparam(thread, SCHED_FIFO, &sch_params) != 0)
        {
        }
    }
}