#pragma once
using ThreadID = HANDLE;
using ProcessID = DWORD;

namespace Platform
{
	XRCORE_API unsigned int GetCoresCount();
	XRCORE_API ThreadID GetCurrentThread();
	XRCORE_API size_t GetThreadId(ThreadID ID);
	XRCORE_API void SetThreadName(const char* Name);
	XRCORE_API long AtomicCompareExchange(long* ptr, long expected, long desired);

	inline void SetCurrentThreadHighPriority()
	{
		SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
	}

	inline void SetCurrentThreadNormalPriority()
	{
		SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	}

	inline ProcessID GetCurrentProcessId()
	{
		return ::GetCurrentProcessId();
	}
	
    inline void WaitForSingleObject(ThreadID ID)
    {
    	::WaitForSingleObject(ID, INFINITE);
    }
}