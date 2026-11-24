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
#include "../RHI.h"
#ifdef IXR_WINDOWS
#include "DX11GPUEvents.h"
#include <d3d11_1.h>

#   include <wrl/client.h>

using Microsoft::WRL::ComPtr;

#define MAX_STACK_COUNT 64
// Frames between issuing a query and reading it back without a flush
#define FRAME_LATENCY 4
#define DX11Device ((ID3D11Device*)GRHI->DevicePtr->RawDevice)
#define DX11Context ((ID3D11DeviceContext*)GRHI->GetContext())

static bool RenderingAtFrame = false;

struct gpu_event_item
{
    u32 begin_offset;
    u32 end_offset;
};

struct gpu_event_state
{
    u32 parent;
    u32 stack_idx;
    shared_str name;
    ComPtr<ID3D11Query> disjoint;
    ComPtr<ID3D11Query> begin;
    ComPtr<ID3D11Query> end;
};

struct gpu_frame_state 
{
    u32 counter;
    xr_vector<gpu_event_state> events;
};

struct gpu_events_state
{
    u64 frame;
    u64 stack_idx;
    xr_array<u32, MAX_STACK_COUNT> stack;
    gpu_frame_state states[FRAME_LATENCY];
    RHI_GPU_EVENT perf[2];
    u32 perf_idx;
};

static gpu_events_state events_state;

static void InvalidateQueries()
{
    for (auto& state : events_state.states)
    {
        if (state.events.size() < QUERY_MAX_COUNT)
        {
            state.events.clear();
            state.events.resize(QUERY_MAX_COUNT);

            for (size_t i = 0; i < QUERY_MAX_COUNT; i++)
            {
                auto& event = state.events[i];

                D3D11_QUERY_DESC desc = { D3D11_QUERY_TIMESTAMP_DISJOINT };

                R_CHK(DX11Device->CreateQuery(&desc, event.disjoint.GetAddressOf()));
                desc.Query = D3D11_QUERY_TIMESTAMP;
                R_CHK(DX11Device->CreateQuery(&desc, event.begin.GetAddressOf()));
                R_CHK(DX11Device->CreateQuery(&desc, event.end.GetAddressOf()));
            }
        }
    }
}

void GPUEvents_BeginRendering()
{
    InvalidateQueries();

    events_state.frame++;
    auto& curr_state = events_state.states[events_state.frame % FRAME_LATENCY];

    if (events_state.frame >= FRAME_LATENCY)
    {
        // A frame is published whole or not at all: the previous result stays until then
        RHI_GPU_EVENT& perf = events_state.perf[events_state.perf_idx ^ 1];
        u32 ready = 0;
        for (; ready < curr_state.counter; ready++)
        {
            u64 begin = 0, end = 0;
            D3D11_QUERY_DATA_TIMESTAMP_DISJOINT disjoint_data = {};
            auto& event = curr_state.events[ready];

            const u32 Flags = D3D11_ASYNC_GETDATA_DONOTFLUSH;
            if (DX11Context->GetData(event.begin.Get(), &begin, sizeof(begin), Flags) != S_OK || DX11Context->GetData(event.end.Get(), &end, sizeof(end), Flags) != S_OK || DX11Context->GetData(event.disjoint.Get(), &disjoint_data, sizeof(disjoint_data), Flags) != S_OK)
            {
                break;
            }

            auto& perf_event = perf.events[ready];
            perf_event.begin = begin;
            perf_event.end = end;
            perf_event.freq = disjoint_data.Frequency;
            perf_event.stack = event.stack_idx;
            perf_event.name = event.name;
        }

        if (ready != 0 && ready == curr_state.counter)
        {
            perf.count = ready;
            events_state.perf_idx ^= 1;
        }
    }

    curr_state.counter = 0;

    RenderingAtFrame = true;
    GPUEvents_PushEvent("Frame");
}

int GPUEvents_PushEvent(const char* name)
{
    // The statistics may be switched on in the middle of a frame, which then has no queries to write into
    if (!RenderingAtFrame)
    {
        return -1;
    }

    auto& curr_state = events_state.states[events_state.frame % FRAME_LATENCY];
    u32 index = curr_state.counter; R_ASSERT(curr_state.counter < QUERY_MAX_COUNT);
    auto& event = curr_state.events[index];

    event.name = name;
    event.stack_idx = events_state.stack_idx;
    event.parent = events_state.stack[events_state.stack_idx++];
    events_state.stack[events_state.stack_idx] = index;

    DX11Context->Begin(event.disjoint.Get());
    DX11Context->End(event.begin.Get());

    curr_state.counter++;
    return index;
}

void GPUEvents_PopEvent(int index)
{
    auto& curr_state = events_state.states[events_state.frame % FRAME_LATENCY];
    R_ASSERT(index < QUERY_MAX_COUNT);
    auto& event = curr_state.events[index];

    DX11Context->End(event.end.Get());
    DX11Context->End(event.disjoint.Get());

    R_ASSERT(events_state.stack_idx > 0);
    events_state.stack_idx--;
}

void GPUEvents_EndRendering()
{
    if (RenderingAtFrame)
    {
        GPUEvents_PopEvent(0);
        RenderingAtFrame = false;
    }
}

const RHI_GPU_EVENT& GPUEvents_Statistics()
{
    return events_state.perf[events_state.perf_idx];
}
#endif