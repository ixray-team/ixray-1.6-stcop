#include "stdafx.h"

#include "../xrRender/Debug/dxGPUEventWrapper.h"

#ifdef DEBUG_DRAW

GPUEventWrapper::GPUEventWrapper(const char* name, const wchar_t* wname)
{
    D3DPERF_BeginEvent(color_rgba(127, 0, 0, 255), wname);
}

GPUEventWrapper::~GPUEventWrapper()
{
    D3DPERF_EndEvent();
}

#endif	//	DEBUG