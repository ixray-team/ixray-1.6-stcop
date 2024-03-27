#pragma once
#include <mutex>

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
