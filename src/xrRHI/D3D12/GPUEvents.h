/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
