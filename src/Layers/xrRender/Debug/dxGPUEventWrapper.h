#ifndef	dxGPUEventWrapper_included
#define	dxGPUEventWrapper_included
#pragma once
#include "dxGPUEvents.h"

#ifdef	DEBUG_DRAW

#ifdef IXRAY_PROFILER
#define GPU_EVENT(Name)	GPUEventWrapper	pixEvent##Name(#Name, L#Name); PROF_EVENT(#Name)
#else
#define GPU_EVENT(Name)	GPUEventWrapper	pixEvent##Name(#Name, L#Name)
#endif

class GPUEventWrapper
{
private:
	int _index;

public:
	GPUEventWrapper(const char* name, const wchar_t* wname);
	~GPUEventWrapper();
};
#else	//	DEBUG

#ifdef IXRAY_PROFILER
#define GPU_EVENT(Name) PROF_EVENT(#Name)
#else
#define GPU_EVENT(Name)	{;}
#endif
#endif	//	DEBUG

#endif	//	dxGPUEventWrapper_included