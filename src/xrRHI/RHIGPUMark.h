/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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