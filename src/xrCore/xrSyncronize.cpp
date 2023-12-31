#include "stdafx.h"

xrCriticalSection::xrCriticalSection()
{
	InitializeCriticalSection((CRITICAL_SECTION*)&pmutex);
}

xrCriticalSection::~xrCriticalSection()
{
	DeleteCriticalSection((CRITICAL_SECTION*)&pmutex);
}

void xrCriticalSection::Enter()
{
	EnterCriticalSection((CRITICAL_SECTION*)&pmutex);
}

void xrCriticalSection::Leave()
{
	LeaveCriticalSection((CRITICAL_SECTION*)&pmutex);
}

BOOL xrCriticalSection::TryEnter()
{
	return TryEnterCriticalSection((CRITICAL_SECTION*)&pmutex);
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