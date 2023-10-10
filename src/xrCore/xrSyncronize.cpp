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

xrCriticalSection::raii::raii(xrCriticalSection* critical_section)
	: critical_section(critical_section)
{
	VERIFY(critical_section);
	critical_section->Enter();
}

xrCriticalSection::raii::~raii()
{
	critical_section->Leave();
}
