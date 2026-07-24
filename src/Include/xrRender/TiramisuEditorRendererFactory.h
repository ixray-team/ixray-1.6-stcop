#pragma once

#include "EditorRenderer.h"
#include "EditorUIRenderer.h"
#include "../../xrCore/RenderTestPolicy.h"

struct SDL_Window;

enum class ETiramisuEditorGraphicsApi : u8
{
    Vulkan,
    D3D12
};

// Невладеющие интерфейсы одного renderer-owned backend. Lifetime хранится
// внутри OpaqueInstance и завершается только DestroyTiramisuEditorRenderer.
struct FTiramisuEditorRendererInstance
{
    IXrUIRendererBackend* UiBackend = nullptr;
    IEditorRenderBackend* EditorBackend = nullptr;
    IMaterialPreviewRenderer* MaterialPreviewRenderer = nullptr;
    void* OpaqueInstance = nullptr;

    [[nodiscard]] bool IsValid() const noexcept
    {
        return UiBackend && EditorBackend && MaterialPreviewRenderer &&
            OpaqueInstance;
    }
};

#if defined(_WIN32)
#   if defined(xrRenderTiramisu_EXPORTS)
#       define XR_TIRAMISU_RENDER_API __declspec(dllexport)
#   else
#       define XR_TIRAMISU_RENDER_API __declspec(dllimport)
#   endif
#else
#   define XR_TIRAMISU_RENDER_API
#endif

// Создаёт Tiramisu backend внутри xrRenderTiramisu. LevelEditor получает
// только renderer-neutral интерфейсы и не создаёт NRI device самостоятельно.
extern "C" XR_TIRAMISU_RENDER_API bool CreateTiramisuEditorRenderer(
    SDL_Window* Window,
    ETiramisuEditorGraphicsApi Api,
    const FRenderDeterministicTestPolicy& DeterministicTest,
    FTiramisuEditorRendererInstance& OutInstance);

// Уничтожает backend в том же DLL, где он был создан.
extern "C" XR_TIRAMISU_RENDER_API void DestroyTiramisuEditorRenderer(
    FTiramisuEditorRendererInstance& Instance);

