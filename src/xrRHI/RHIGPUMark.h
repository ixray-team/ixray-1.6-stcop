#pragma once
#include "../xrCore/xrCore.h"

extern RHI_API void* g_pAnnotation;

class RHI_API CRHIGPUMark
{
public:
	CRHIGPUMark(const char* name, const wchar_t* wname, const void* profiler_location = nullptr);
	virtual ~CRHIGPUMark();

private:
	void* Annotation = nullptr;
};

#define QUERY_MAX_COUNT 1024

struct RHI_GPU_EVENT_STATS
{
	u64 freq;
	u64 begin;
	u64 end;
	u64 stack;
	shared_str name;
};

struct RHI_GPU_EVENT
{
	u64 count;
	xr_array<RHI_GPU_EVENT_STATS, QUERY_MAX_COUNT> events;
};

#ifdef IXR_WINDOWS
#	ifdef DEBUG_DRAW
#		ifdef IXRAY_PROFILER
#			define GPU_EVENT(Name)	CRHIGPUMark	pixEvent##Name(#Name, L#Name); PROF_EVENT(#Name)
#		elifdef IXRAY_PROFILER_TRACY
#			include <tracy/Tracy.hpp>
#			define GPU_EVENT(Name) static constexpr tracy::SourceLocationData pixSource##Name { #Name, TracyFunction, TracyFile, (u32)TracyLine, 0 }; CRHIGPUMark pixEvent##Name(#Name, L#Name, &pixSource##Name);
#		else
#			define GPU_EVENT(Name)	CRHIGPUMark pixEvent##Name(#Name, L#Name);
#		endif
#	else
#		define GPU_EVENT(Name)	CRHIGPUMark pixEvent##Name(#Name, L#Name);
#	endif
#else
#	define GPU_EVENT(name)
#endif