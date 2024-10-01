#pragma once
#include <mutex>
#include <shared_mutex>

// Desc: Simple wrapper for critical section
class XRCORE_API xrCriticalSection
{
private:
	xrCriticalSection(xrCriticalSection const & copy) {};
#ifdef IXR_WINDOWS
	CRITICAL_SECTION pmutex;
#else
    std::recursive_mutex pmutex;
#endif
public:
    xrCriticalSection	();
    ~xrCriticalSection	();

    void				Enter	();
    void				Leave	();
	BOOL				TryEnter();
};


class XRCORE_API xrCriticalSectionGuard
{
public:
	xrCriticalSectionGuard(xrCriticalSection*);
	xrCriticalSectionGuard(xrCriticalSection&);
	~xrCriticalSectionGuard();

private:
	xrCriticalSection* critical_section;
};


class XRCORE_API xrSRWLock
{
private:

#ifdef IXR_WINDOWS
    SRWLOCK smutex;
#else
    std::shared_mutex smutex;
#endif

public:
    xrSRWLock();
    ~xrSRWLock() {};

    void AcquireExclusive();
    void ReleaseExclusive();

    void AcquireShared();
    void ReleaseShared();

    BOOL TryAcquireExclusive();
    BOOL TryAcquireShared();
};
//Write functions guard: lock.AcquireExclusive(); ... lock.ReleaseExclusive();
//Read functions guard: lock.AcquireShared(); ... lock.ReleaseShared();

class XRCORE_API xrSRWLockGuard
{
public:
    xrSRWLockGuard(xrSRWLock& lock, bool shared = false);
    xrSRWLockGuard(xrSRWLock* lock, bool shared = false);
    ~xrSRWLockGuard();

private:
    xrSRWLock* lock;
    bool shared;
};
//Write functions guard: xrSRWLockGuard guard(lock); ...
//Read functions guard: xrSRWLockGuard guard(lock, true); ...