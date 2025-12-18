#include "Device.h"
#include "DX11GPUEvents.h"
#include "DX11GPUEventWrapper.h"

InternalDX11GPUEventWrapper::InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname)
{
    //if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Shaders)])
    //{
    //    return;
    //}
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->BeginEvent(wname);
    }

    _index = GPUEvents_PushEvent(name);
}

InternalDX11GPUEventWrapper::~InternalDX11GPUEventWrapper()
{
    //if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Shaders)])
    //{
    //    return;
    //}

    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->EndEvent();
    }

    GPUEvents_PopEvent(_index);
}