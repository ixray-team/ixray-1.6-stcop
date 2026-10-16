#include "stdafx.h"

u32 Platform::GetCoresCount()
{
	return sysconf(_SC_NPROCESSORS_ONLN);;
}

ThreadID Platform::GetCurrentThread()
{
	return pthread_self();
}

size_t Platform::GetThreadId(ThreadID ID)
{
    return (size_t)ID;
}

void Platform::SetThreadName(const char* name)
{
    // AI: unlike Linux, the macOS variant of pthread_setname_np takes a single argument
    // AI: and only names the calling thread.
    pthread_setname_np(name);
}

long Platform::AtomicCompareExchange(long* ptr, long expected, long desired)
{
    long old = expected;
    __atomic_compare_exchange_n
    (
        ptr,
        &old,
        desired,
        false,
        __ATOMIC_SEQ_CST,
        __ATOMIC_SEQ_CST
    );

    return old;
}