#include "stdafx.h"

#include "../../../Include/xrRender/TiramisuEditorRendererFactory.h"
#include "TiramisuEditorRenderBridge.h"

extern "C" bool CreateTiramisuEditorRenderer(
    SDL_Window* Window,
    const ETiramisuEditorGraphicsApi Api,
    const FRenderDeterministicTestPolicy& DeterministicTest,
    FTiramisuEditorRendererInstance& OutInstance)
{
    if (OutInstance.OpaqueInstance)
        return false;

    auto* Backend = new TiramisuEditorRenderBridge(
        Window, Api, DeterministicTest);
    OutInstance.UiBackend = Backend;
    OutInstance.EditorBackend = Backend;
    OutInstance.MaterialPreviewRenderer = Backend;
    OutInstance.OpaqueInstance = Backend;
    return true;
}

extern "C" void DestroyTiramisuEditorRenderer(
    FTiramisuEditorRendererInstance& Instance)
{
    delete static_cast<TiramisuEditorRenderBridge*>(
        Instance.OpaqueInstance);
    Instance = {};
}

