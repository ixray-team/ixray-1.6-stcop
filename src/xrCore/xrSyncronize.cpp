#include "stdafx.h"

xrCriticalSection::xrCriticalSection()
{
#ifdef IXR_WINDOWS
	InitializeCriticalSection((CRITICAL_SECTION*)&pmutex);
#endif
}

xrCriticalSection::~xrCriticalSection()
{
#ifdef IXR_WINDOWS
	DeleteCriticalSection((CRITICAL_SECTION*)&pmutex);
#endif
}

void xrCriticalSection::Enter()
{
	PROF_EVENT("xrCriticalSection::Enter");
#ifdef IXR_WINDOWS
	EnterCriticalSection((CRITICAL_SECTION*)&pmutex);
#else
    pmutex.lock();
#endif
}

void xrCriticalSection::Leave()
{
	PROF_EVENT("xrCriticalSection::Leave");
#ifdef IXR_WINDOWS
	LeaveCriticalSection((CRITICAL_SECTION*)&pmutex);
#else
    pmutex.unlock();
#endif
}

BOOL xrCriticalSection::TryEnter()
{
	PROF_EVENT("xrCriticalSection::TryEnter");
#ifdef IXR_WINDOWS
	return TryEnterCriticalSection((CRITICAL_SECTION*)&pmutex);
#else
    return pmutex.try_lock();
#endif
}

xrCriticalSectionGuard::xrCriticalSectionGuard(xrCriticalSection* in_critical_section)
	: critical_section(in_critical_section)
{
	VERIFY(critical_section);
	critical_section->Enter();
}

xrCriticalSectionGuard::xrCriticalSectionGuard(xrCriticalSection& in_critical_section)
	: critical_section(&in_critical_section)
{
	VERIFY(critical_section);
	critical_section->Enter();
}

xrCriticalSectionGuard::~xrCriticalSectionGuard()
{
	critical_section->Leave();
}

xrSRWLock::xrSRWLock()
{
#ifdef IXR_WINDOWS
    InitializeSRWLock(&smutex);
#endif
}

void xrSRWLock::AcquireExclusive()
{
	PROF_EVENT("xrSRWLock::AcquireExclusive");
#ifdef IXR_WINDOWS
    AcquireSRWLockExclusive(&smutex);
#else
    smutex.lock();
#endif
}

void xrSRWLock::ReleaseExclusive()
{
	PROF_EVENT("xrSRWLock::ReleaseExclusive");
#ifdef IXR_WINDOWS
    ReleaseSRWLockExclusive(&smutex);
#else
    smutex.unlock();
#endif
}

void xrSRWLock::AcquireShared()
{
	PROF_EVENT("xrSRWLock::AcquireShared");
#ifdef IXR_WINDOWS
    AcquireSRWLockShared(&smutex);
#else
    smutex.lock_shared();
#endif
}

void xrSRWLock::ReleaseShared()
{
	PROF_EVENT("xrSRWLock::ReleaseShared");
#ifdef IXR_WINDOWS
    ReleaseSRWLockShared(&smutex);
#else
    smutex.unlock_shared();
#endif
}

BOOL xrSRWLock::TryAcquireExclusive()
{
#ifdef IXR_WINDOWS
    return TryAcquireSRWLockExclusive(&smutex);
#else
    return smutex.try_lock();
#endif
}

BOOL xrSRWLock::TryAcquireShared()
{
#ifdef IXR_WINDOWS
    return TryAcquireSRWLockShared(&smutex);
#else
    return smutex.try_lock_shared();
#endif
}


xrSRWLockGuard::xrSRWLockGuard(xrSRWLock* lock, bool shared)
    : lock(lock), shared(shared)
{
    if (shared)
        lock->AcquireShared();
    else
        lock->AcquireExclusive();
}

xrSRWLockGuard::xrSRWLockGuard(xrSRWLock& lock, bool shared)
    : lock(&lock), shared(shared)
{
    if (shared)
        lock.AcquireShared();
    else
        lock.AcquireExclusive();
}

xrSRWLockGuard::~xrSRWLockGuard()
{
    if (shared)
        lock->ReleaseShared();
    else
        lock->ReleaseExclusive();
}