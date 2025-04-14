#pragma once
using ThreadID = HANDLE;

namespace Platform
{
	unsigned int GetCoresCount();
	XRCORE_API ThreadID GetCurrentThread();
    size_t GetThreadId(ThreadID ID);
	XRCORE_API void SetThreadName(const char* Name);
}