#pragma once

// Desc: Simple wrapper for critical section
class XRCORE_API xrCriticalSection
{
private:
	xrCriticalSection(xrCriticalSection const & copy) {};
	CRITICAL_SECTION pmutex;

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