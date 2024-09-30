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
