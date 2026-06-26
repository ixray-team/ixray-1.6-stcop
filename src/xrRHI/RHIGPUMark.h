#pragma once
#include "../xrCore/xrCore.h"
#include "../xrCore/_types.h"

extern RHI_API void* g_pAnnotation;

class RHI_API CRHIGPUMark
{
public:
	CRHIGPUMark(const char* name, const wchar_t* wname);
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
#			define PROF_GPU_CTX_CREATE(Device, DeviceContext)
#			define PROF_GPU_CTX_COLLECT()
#			define PROF_GPU_CTX_DESTROY()
#			define GPU_EVENT(Name)	CRHIGPUMark	pixEvent##Name(#Name, L#Name); PROF_EVENT(#Name)
#		elifdef IXRAY_PROFILER_TRACY
#			include <tracy/TracyD3D11.hpp>
			extern RHI_API TracyD3D11Ctx g_tracyD3D11GPUContext;
#			define PROF_GPU_CTX_CREATE(Device, DeviceContext) TracyD3D11Context(Device, DeviceContext);
#			define PROF_GPU_CTX_COLLECT() TracyD3D11Collect(g_tracyD3D11GPUContext);
#			define PROF_GPU_CTX_DESTROY() TracyD3D11Destroy(g_tracyD3D11GPUContext);
#			define GPU_EVENT(Name) TracyD3D11Zone(g_tracyD3D11GPUContext, #Name); CRHIGPUMark pixEvent##Name(#Name, L#Name);
#		else
#			define PROF_GPU_CTX_CREATE(Device, DeviceContext)
#			define PROF_GPU_CTX_COLLECT()
#			define PROF_GPU_CTX_DESTROY()
#			define GPU_EVENT(Name)	CRHIGPUMark pixEvent##Name(#Name, L#Name);
#		endif
#	else
#		define PROF_GPU_CTX_CREATE(Device, DeviceContext)
#		define PROF_GPU_CTX_COLLECT()
#		define PROF_GPU_CTX_DESTROY()
#		define GPU_EVENT(Name)	CRHIGPUMark pixEvent##Name(#Name, L#Name);
#	endif
#else
#	define PROF_GPU_CTX_CREATE(Device, DeviceContext)
#	define PROF_GPU_CTX_COLLECT()
#	define PROF_GPU_CTX_DESTROY()
#	define GPU_EVENT(name)
#endif