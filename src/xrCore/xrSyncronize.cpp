#include "stdafx.h"

xrCriticalSection::xrCriticalSection()
{
#ifdef IXR_WINDOWS
	InitializeCriticalSection(&pmutex);
#elif defined(IXR_LINUX)
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&pmutex, &attr);
	pthread_mutexattr_destroy(&attr); 
#endif
}

xrCriticalSection::~xrCriticalSection()
{
#ifdef IXR_WINDOWS
	DeleteCriticalSection(&pmutex);
#elif defined(IXR_LINUX)
	pthread_mutex_destroy(&pmutex);
#endif
}

void xrCriticalSection::Enter()
{
	PROF_EVENT("xrCriticalSection::Enter");
#ifdef IXR_WINDOWS
	EnterCriticalSection(&pmutex);
#elif defined(IXR_LINUX)
	pthread_mutex_lock(&pmutex);
#else
    pmutex.lock();
#endif
}

void xrCriticalSection::Leave()
{
	PROF_EVENT("xrCriticalSection::Leave");
#ifdef IXR_WINDOWS
	LeaveCriticalSection(&pmutex);
#elif defined(IXR_LINUX)
	pthread_mutex_unlock(&pmutex);
#else
    pmutex.unlock();
#endif
}

bool xrCriticalSection::TryEnter()
{
	PROF_EVENT("xrCriticalSection::TryEnter");
#ifdef IXR_WINDOWS
	return TryEnterCriticalSection(&pmutex);
#elif defined(IXR_LINUX)
	return (pthread_mutex_trylock(&pmutex) == 0) ? TRUE : FALSE;
#else
	return pmutex.try_lock() ? TRUE : FALSE;
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
#elif defined(IXR_LINUX)
	pthread_rwlock_init(&smutex, nullptr);
#endif
}

xrSRWLock::~xrSRWLock()
{
#ifdef IXR_LINUX
	pthread_rwlock_destroy(&smutex);
#endif
}

void xrSRWLock::AcquireExclusive()
{
	PROF_EVENT("xrSRWLock::AcquireExclusive");
#ifdef IXR_WINDOWS
    AcquireSRWLockExclusive(&smutex);
#elif defined(IXR_LINUX)
	pthread_rwlock_wrlock(&smutex);
#else
    smutex.lock();
#endif
}

void xrSRWLock::ReleaseExclusive()
{
	PROF_EVENT("xrSRWLock::ReleaseExclusive");
#ifdef IXR_WINDOWS
    ReleaseSRWLockExclusive(&smutex);
#elif defined(IXR_LINUX)
	pthread_rwlock_unlock(&smutex);
#else
    smutex.unlock();
#endif
}

void xrSRWLock::AcquireShared()
{
	PROF_EVENT("xrSRWLock::AcquireShared");
#ifdef IXR_WINDOWS
    AcquireSRWLockShared(&smutex);
#elif defined(IXR_LINUX)
	pthread_rwlock_rdlock(&smutex);
#else
    smutex.lock_shared();
#endif
}

void xrSRWLock::ReleaseShared()
{
	PROF_EVENT("xrSRWLock::ReleaseShared");
#ifdef IXR_WINDOWS
    ReleaseSRWLockShared(&smutex);
#elif defined(IXR_LINUX)
	pthread_rwlock_unlock(&smutex);
#else
    smutex.unlock_shared();
#endif
}

bool xrSRWLock::TryAcquireExclusive()
{
#ifdef IXR_WINDOWS
	return TryAcquireSRWLockExclusive(&smutex) ? TRUE : FALSE;
#elif defined(IXR_LINUX)
	return (pthread_rwlock_trywrlock(&smutex) == 0) ? TRUE : FALSE;
#else
	return smutex.try_lock() ? TRUE : FALSE;
#endif
}

bool xrSRWLock::TryAcquireShared()
{
#ifdef IXR_WINDOWS
	return TryAcquireSRWLockShared(&smutex) ? TRUE : FALSE;
#elif defined(IXR_LINUX)
	return (pthread_rwlock_tryrdlock(&smutex) == 0) ? TRUE : FALSE;
#else
	return smutex.try_lock_shared() ? TRUE : FALSE;
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