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
    pthread_setname_np(pthread_self(), name);
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