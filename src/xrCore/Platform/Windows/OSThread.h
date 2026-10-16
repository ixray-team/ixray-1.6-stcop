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
	// 32-bit variant: 'long' is 8 bytes on Darwin/Linux, so CAS on u32 fields must not go through 'long'.
	XRCORE_API unsigned int AtomicCompareExchangeU32(unsigned int* ptr, unsigned int expected, unsigned int desired);

	inline void SetCurrentThreadHighPriority()
	{
		SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
	}

	inline void SetCurrentThreadNormalPriority()
	{
		SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	}

	inline void SetCurrentThreadBackgroundMode(bool Enable)
	{
		SetThreadPriority(::GetCurrentThread(), Enable ? THREAD_MODE_BACKGROUND_BEGIN : THREAD_MODE_BACKGROUND_END);
	}

	inline ProcessID GetCurrentProcessId()
	{
		return ::GetCurrentProcessId();
	}
	
    inline void WaitForSingleObject(ThreadID ID)
    {
    	::WaitForSingleObject(ID, INFINITE);
    }

	inline void JoinThread(ThreadID ID)
	{
		if (ID != nullptr && ID != INVALID_HANDLE_VALUE)
		{
			::WaitForSingleObject(ID, INFINITE);
			::CloseHandle(ID);
		}
	}

	inline void CloseThreadHandle(ThreadID ID)
	{
		if (ID != nullptr && ID != INVALID_HANDLE_VALUE)
		{
			::CloseHandle(ID);
		}
	}
}
