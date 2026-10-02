#include "Device.h"
#include "DX11GPUEvents.h"
#include "DX11GPUEventWrapper.h"

InternalDX11GPUEventWrapper::InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname, const void* profiler_location)
{
#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
    if (profiler_location && g_tracyD3D11GPUContext)
    {
#if defined(TRACY_HAS_CALLSTACK) && defined(TRACY_CALLSTACK)
        profiler_zone.emplace(g_tracyD3D11GPUContext, static_cast<const tracy::SourceLocationData*>(profiler_location), TRACY_CALLSTACK, true);
#else
        profiler_zone.emplace(g_tracyD3D11GPUContext, static_cast<const tracy::SourceLocationData*>(profiler_location), true);
#endif
    }
#endif
#ifdef IXR_WINDOWS
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->BeginEvent(wname);
    }

    if (GRHI->GPUStatsEnable)
    {
        _index = GPUEvents_PushEvent(name);
    }
#endif
}

InternalDX11GPUEventWrapper::~InternalDX11GPUEventWrapper()
{
#ifdef IXR_WINDOWS
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->EndEvent();
    }

    if (GRHI->GPUStatsEnable && _index != -1)
    {
        GPUEvents_PopEvent(_index);
    }
#endif
}
