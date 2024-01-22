#pragma once
using ThreadID = HANDLE;

using thread_type = void;
#define THREADEXIT return

namespace Platform
{
	unsigned int GetCoresCount();
	XRCORE_API ThreadID GetCurrentThread();
    size_t GetThreadId(ThreadID ID);
    void SetThreadName(const char* Name);
}