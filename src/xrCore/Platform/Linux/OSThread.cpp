#include "stdafx.h"

unsigned int Platform::GetCpuCores(CpuCore* Cores, unsigned int MaxCount)
{
	const long Logical = std::min<long>(sysconf(_SC_NPROCESSORS_ONLN), 64);
	unsigned int Count = 0;
	for (long Index = 0; Index < Logical && Count < MaxCount; ++Index)
	{
		Cores[Count].LogicalMask = 1ull << Index;
		Cores[Count].EfficiencyClass = 0;
		Count++;
	}
	return Count;
}

void Platform::SetCurrentThreadAffinity(unsigned long long LogicalMask)
{
	if (LogicalMask == 0)
	{
		return;
	}

	cpu_set_t Set;
	CPU_ZERO(&Set);
	for (int Index = 0; Index < 64; ++Index)
	{
		if (LogicalMask & (1ull << Index))
		{
			CPU_SET(Index, &Set);
		}
	}
	pthread_setaffinity_np(pthread_self(), sizeof(Set), &Set);
}

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

unsigned int Platform::AtomicCompareExchangeU32(unsigned int* ptr, unsigned int expected, unsigned int desired)
{
    u32 old = expected;
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