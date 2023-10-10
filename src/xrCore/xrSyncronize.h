#pragma once

// Desc: Simple wrapper for critical section
class XRCORE_API xrCriticalSection
{
public:
	class XRCORE_API raii
	{
	public:
		raii(xrCriticalSection*);
	   ~raii();

	private:
		xrCriticalSection* critical_section;
	};

private:
	xrCriticalSection(xrCriticalSection const & copy) {}; //noncopyable
	CRITICAL_SECTION pmutex;

public:
    xrCriticalSection	();
    ~xrCriticalSection	();

    void				Enter	();
    void				Leave	();
	BOOL				TryEnter();
};
