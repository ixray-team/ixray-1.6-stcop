#pragma once
using ThreadID = HANDLE;

namespace Platform
{
	unsigned int GetCoresCount();
	ThreadID GetCurrentThread();
    size_t GetThreadId(ThreadID ID);
    void SetThreadName(const char* Name);
}