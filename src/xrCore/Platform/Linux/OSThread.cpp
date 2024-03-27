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
    pthread_id_np_t tid;
    pthread_getunique_np(&ID, &tid);

    return (size_t)tid;
}