#pragma once
#include <optional>
#if defined(IXRAY_PROFILER_TRACY)
#include <tracy/TracyD3D12.hpp>
extern TracyD3D12Ctx g_tracyD3D12GPUContext;
#define DX12_GPU_EVENT(command_list, name) TracyD3D12Zone(g_tracyD3D12GPUContext, command_list, name)
#else
#define DX12_GPU_EVENT(command_list, name)
#endif

class InternalDX12GPUEventWrapper
{
public:
    InternalDX12GPUEventWrapper(const char* name, const void* location);
    ~InternalDX12GPUEventWrapper();

private:
    int _index = -1;
    void* _optick = nullptr;
#if defined(IXRAY_PROFILER_TRACY)
    std::optional<tracy::D3D12ZoneScope> _zone;
#endif
};
