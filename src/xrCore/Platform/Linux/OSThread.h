#pragma once
#include <pthread.h>

using ThreadID = pthread_t;

namespace Platform
{
	unsigned int GetCoresCount();
	ThreadID GetCurrentThread();
    size_t GetThreadId(ThreadID ID);
}