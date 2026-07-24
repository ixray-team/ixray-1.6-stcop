#pragma once

#include <cstdint>

struct ImDrawData;

enum class EXrUIRendererPlatform : std::uint8_t
{
    D3D,
    Vulkan,
    Other
};

// Минимальный контракт между xrEUI и выбранным рендерером.
// Реализация и все API-объекты принадлежат renderer DLL.
class IXrUIRendererBackend
{
public:
    virtual ~IXrUIRendererBackend() = default;

    [[nodiscard]] virtual EXrUIRendererPlatform GetPlatform()
        const noexcept = 0;
    [[nodiscard]] virtual bool SupportsPlatformViewports()
        const noexcept = 0;
    [[nodiscard]] virtual bool OwnsMainPresentation()
        const noexcept = 0;
    [[nodiscard]] virtual bool Initialize() = 0;
    virtual void Shutdown() = 0;
    virtual void BeginFrame() = 0;
    virtual void RenderDrawData(ImDrawData& DrawData) = 0;
    virtual void InvalidateDeviceObjects() = 0;
    virtual void CreateDeviceObjects() = 0;
};
