#pragma once

#include "TiramisuRenderTypes.h"
#include "Extensions/NRIDeviceCreation.h"
#include "Extensions/NRIImgui.h"
#include "Extensions/NRIStreamer.h"

// Владеет единственным NRI-устройством процесса Tiramisu и его очередями.
// Игра и редактор используют этот объект, а не создают отдельные устройства.
class TiramisuRenderDevice
{
public:
    TiramisuRenderDevice();
    ~TiramisuRenderDevice();

    // Выбирает API из параметров командной строки игры.
    void Initialize();

    // Создаёт устройство для явно выбранного API. Callback используется
    // редактором для вывода диагностик NRI в общий журнал.
    void Initialize(
        nri::GraphicsAPI GraphicsApi,
        const nri::CallbackInterface& CallbackInterface);

    // Освобождает устройство после остановки render thread и всех клиентов.
    void Destroy();

    [[nodiscard]] static bool FindBestAdapterDescription(
        nri::GraphicsAPI GraphicsApi,
        nri::AdapterDesc& Result);

    [[nodiscard]] bool IsInitialized() const noexcept
    {
        return Device != nullptr;
    }

    [[nodiscard]] bool HasAsyncComputeQueue() const noexcept
    {
        return ComputeQueue != nullptr;
    }

    nri::Device* Device = nullptr;
    nri::Queue* GraphicsQueue = nullptr;
    nri::Queue* ComputeQueue = nullptr;
    nri::Queue* CopyQueue = nullptr;
    nri::Streamer* Streamer = nullptr;
    nri::StreamerInterface StreamerInterface = {};

    nri::CoreInterface CoreInterface = {};
    nri::SwapChainInterface SwapChainInterface = {};
    nri::HelperInterface HelperInterface = {};
    nri::ImguiInterface ImGuiInterface = {};

    // Смещения должны совпадать с настройками компиляции HLSL/SPIR-V.
    static constexpr nri::VKBindingOffsets VK_BINDING_OFFSETS =
        {0, 128, 32, 64};

    nri::GraphicsAPI GraphicsApi = nri::GraphicsAPI::VK;
    nri::DeviceDesc DeviceDescription = {};

private:
    nri::AllocationCallbacks AllocationCallbacks = {};
};

extern TiramisuRenderDevice GRenderDevice;