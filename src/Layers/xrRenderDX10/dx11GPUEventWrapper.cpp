#include "stdafx.h"

#include "../xrRender/Debug/dxGPUEventWrapper.h"

#ifdef DEBUG_DRAW

GPUEventWrapper::GPUEventWrapper(const char* name, const wchar_t* wname)
{
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->BeginEvent(wname);
    }

#ifdef USE_DX11
    if (Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Shaders)])
    {
        _index = GPUEvents_PushEvent(name);
    }
#endif
}

GPUEventWrapper::~GPUEventWrapper()
{
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->EndEvent();
    }

#ifdef USE_DX11
    if (Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Shaders)] && _index != -1)
    {
        GPUEvents_PopEvent(_index);
    }
#endif
}

#endif	//	DEBUG