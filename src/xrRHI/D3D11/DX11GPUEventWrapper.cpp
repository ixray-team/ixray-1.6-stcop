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
#include "Device.h"
#include "DX11GPUEvents.h"
#include "DX11GPUEventWrapper.h"

#ifdef IXR_WINDOWS
#include "../Nri/NriDevice.h"

// D3D11 keeps no record of what the GPU ran. With -gpu_dump the end of every pass waits for the GPU,
// so a lost device is noticed in the pass that lost it; without it only the passes last submitted are known.
namespace GpuTrail
{
constexpr u32 MaxDepth = 32;
constexpr u32 HistorySize = 64;

const char* Stack[MaxDepth];
u32 Depth = 0;
const char* History[HistorySize];
u32 Written = 0;
ID3D11Query* Marker = nullptr;

void Report(bool IsExact)
{
    xr_string Text = IsExact ? "Lost while the GPU ran: " : "Passes open when the loss was noticed: ";

    for (u32 Level = 0; Level < std::min(Depth, MaxDepth); ++Level)
    {
        Text += Level ? "/" : "";
        Text += Stack[Level];
    }

    Text += "\r\nLast passes submitted, newest first:";

    for (u32 Age = 1; Age <= std::min(Written, HistorySize); ++Age)
    {
        Text += " ";
        Text += History[(Written - Age) % HistorySize];
    }

    InternalDevice11& Device = *static_cast<InternalDevice11*>(GRHI->DevicePtr);
    GRHI->Nri->ReportLoss(u32(static_cast<ID3D11Device*>(Device.RawDevice)->GetDeviceRemovedReason()), Text.c_str());
}

void Wait()
{
    InternalDevice11& Device = *static_cast<InternalDevice11*>(GRHI->DevicePtr);
    ID3D11Device* Native = static_cast<ID3D11Device*>(Device.RawDevice);

    if (!Marker)
    {
        const D3D11_QUERY_DESC Desc = { D3D11_QUERY_EVENT, 0 };
        Native->CreateQuery(&Desc, &Marker);
    }

    if (!Marker)
    {
        return;
    }

    Device.HWRenderContext->End(Marker);
    HRESULT Result = S_FALSE;

    while (Result == S_FALSE)
    {
        Result = Device.HWRenderContext->GetData(Marker, nullptr, 0, 0);
    }

    if (FAILED(Result) || FAILED(Native->GetDeviceRemovedReason()))
    {
        Report(true);
        R_CHK(Native->GetDeviceRemovedReason());
    }
}
} // namespace GpuTrail

void ReportGpuTrail()
{
    GpuTrail::Report(false);
}
#endif

InternalDX11GPUEventWrapper::InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname, const void* profiler_location)
{
#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
    if (profiler_location && g_tracyD3D11GPUContext)
    {
#if defined(TRACY_HAS_CALLSTACK) && defined(TRACY_CALLSTACK)
        profiler_zone.emplace(g_tracyD3D11GPUContext, static_cast<const tracy::SourceLocationData*>(profiler_location), TRACY_CALLSTACK, true);
#else
        profiler_zone.emplace(g_tracyD3D11GPUContext, static_cast<const tracy::SourceLocationData*>(profiler_location), true);
#endif
    }
#endif
#ifdef IXR_WINDOWS
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->BeginEvent(wname);
    }

    if (GpuTrail::Depth < GpuTrail::MaxDepth)
    {
        GpuTrail::Stack[GpuTrail::Depth] = name;
    }

    GpuTrail::Depth++;
    GpuTrail::History[GpuTrail::Written++ % GpuTrail::HistorySize] = name;

    if (GRHI->GPUStatsEnable)
    {
        _index = GPUEvents_PushEvent(name);
    }
#endif
}

InternalDX11GPUEventWrapper::~InternalDX11GPUEventWrapper()
{
#ifdef IXR_WINDOWS
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->EndEvent();
    }

    if (Core.ParamsData.test(ECoreParams::gpu_dump))
    {
        GpuTrail::Wait();
    }

    GpuTrail::Depth--;

    if (GRHI->GPUStatsEnable && _index != -1)
    {
        GPUEvents_PopEvent(_index);
    }
#endif
}
