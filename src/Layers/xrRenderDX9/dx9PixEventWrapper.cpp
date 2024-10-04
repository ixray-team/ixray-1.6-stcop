#include "stdafx.h"

#include "../xrRender/Debug/dxPixEventWrapper.h"

#ifdef DEBUG_DRAW

PixEventWrapper::PixEventWrapper(LPCWSTR wszName)
{
    D3DPERF_BeginEvent(color_rgba(127, 0, 0, 255), wszName);
}

PixEventWrapper::~PixEventWrapper()
{
    D3DPERF_EndEvent();
}

#endif	//	DEBUG