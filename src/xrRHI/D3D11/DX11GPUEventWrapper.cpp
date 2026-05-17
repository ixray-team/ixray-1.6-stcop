#include "Device.h"
#include "DX11GPUEvents.h"
#include "DX11GPUEventWrapper.h"

InternalDX11GPUEventWrapper::InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname)
{
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
