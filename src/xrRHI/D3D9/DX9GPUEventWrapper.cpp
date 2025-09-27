#include "../RHI.h"
#include "DX9GPUEventWrapper.h"

#include <d3d9.h>

InternalDX9GPUEventWrapper::InternalDX9GPUEventWrapper(const char* name, const wchar_t* wname)
{
    D3DPERF_BeginEvent(color_rgba(127, 0, 0, 255), wname);
}

InternalDX9GPUEventWrapper::~InternalDX9GPUEventWrapper()
{
    D3DPERF_EndEvent();
}