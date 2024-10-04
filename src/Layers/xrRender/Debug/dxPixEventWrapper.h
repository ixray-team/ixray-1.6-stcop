#ifndef	dxPixEventWrapper_included
#define	dxPixEventWrapper_included
#pragma once

#ifdef	DEBUG_DRAW

#ifdef IXRAY_PROFILER
#define PIX_EVENT(Name)	PixEventWrapper	pixEvent##Name(L#Name); PROF_EVENT(#Name)
#else
#define PIX_EVENT(Name)	PixEventWrapper	pixEvent##Name(L#Name)
#endif

class PixEventWrapper
{
public:
	PixEventWrapper(LPCWSTR wszName);
	~PixEventWrapper();
};
#else	//	DEBUG

#ifdef IXRAY_PROFILER
#define PIX_EVENT(Name) PROF_EVENT(#Name)
#else
#define PIX_EVENT(Name)	{;}
#endif
#endif	//	DEBUG

#endif	//	dxPixEventWrapper_included