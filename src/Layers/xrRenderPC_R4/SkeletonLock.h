#pragma once

inline thread_local u32 g_SkeletonUpdateDepth = 0;

class SkeletonLock
{
public:
	SkeletonLock(xrCriticalSection& InMutex, bool InUpdate) :
		Mutex(InMutex),
		Update(InUpdate)
	{
		if (g_SkeletonUpdateDepth == 0)
		{
			Mutex.Enter();
			Locked = true;
		}
		else
		{
			Locked = Mutex.TryEnter();
		}

		if (Locked && Update)
		{
			++g_SkeletonUpdateDepth;
		}
	}

	~SkeletonLock()
	{
		if (!Locked)
		{
			return;
		}

		if (Update)
		{
			--g_SkeletonUpdateDepth;
		}
		Mutex.Leave();
	}

	SkeletonLock(const SkeletonLock&) = delete;
	SkeletonLock& operator=(const SkeletonLock&) = delete;

	bool IsLocked() const
	{
		return Locked;
	}

private:
	xrCriticalSection& Mutex;
	bool Update;
	bool Locked = false;
};
