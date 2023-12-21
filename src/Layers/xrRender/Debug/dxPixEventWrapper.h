#pragma once

#ifdef USE_OPTICK
#include <optick.h>
#define _X(Name) #Name
#define PIX_EVENT(Name) SCOPE_EVENT_NAME(_X(Name))
#else
#ifdef	DEBUG
#define PIX_EVENT(Name)	dxPixEventWrapper	pixEvent##Name(L#Name)

class dxPixEventWrapper
{
public:
	dxPixEventWrapper(LPCWSTR wszName) { D3DPERF_BeginEvent(color_rgba(127,0,0,255), wszName );}
	~dxPixEventWrapper() {D3DPERF_EndEvent();}
};

#else	//	DEBUG
#define PIX_EVENT(Name)	{;}
#endif	//	DEBUG
#endif