#pragma once

class RHI_API CPixEvent
{
public:
	CPixEvent(LPCWSTR wszName);
	~CPixEvent();
};

#ifdef	DEBUG
#define PIX_EVENT(Name)	CPixEvent	pixEvent##Name(L#Name)
#else
#define PIX_EVENT(Name)
#endif