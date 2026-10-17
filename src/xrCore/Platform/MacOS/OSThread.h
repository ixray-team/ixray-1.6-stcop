#pragma once
#include <pthread.h>
#include <chrono>
#include <thread>
#include <sched.h>

// AI: CarbonCore (CoreServices, pulled in by filewatch.hpp) declares "typedef unsigned long ThreadID";
// AI: a different-type alias here would fail to compile in every TU that sees both. Match its type -
// AI: pthread handles still fit (the pointer and unsigned long are both 64-bit on arm64).
using ThreadID = unsigned long;
using ProcessID = pid_t;

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
    inline void SetCurrentThreadNormalPriority()
    {
        pthread_t this_thread = pthread_self();
        sched_param params;
        int policy;

        if (pthread_getschedparam(this_thread, &policy, &params) == 0)
        {
            params.sched_priority = params.sched_priority - 1;
            pthread_setschedparam(this_thread, policy, &params);
        }
    }
    
    inline ProcessID GetCurrentProcessId()
    {
        return getpid();
    }

    inline void WaitForSingleObject(ThreadID ID)
    {
        // AI: the handle came in as the Carbon-compatible integer ThreadID (see the alias above),
        // AI: cast it back to pthread_t for the join.
        pthread_join((pthread_t)ID, nullptr);
    }
}