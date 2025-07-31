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