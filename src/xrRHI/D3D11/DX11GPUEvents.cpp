#include "../RHI.h"
#ifdef IXR_WINDOWS
#include "DX11GPUEvents.h"
#include <d3d11_1.h>

#   include <wrl/client.h>

using Microsoft::WRL::ComPtr;

#define MAX_STACK_COUNT 64
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
    gpu_frame_state states[2];
    RHI_GPU_EVENT perf;
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
    auto& curr_state = events_state.states[events_state.frame % 2];

    if (events_state.frame >= 2)
    {
        events_state.perf.count = 0;

        for (size_t i = 0; i < curr_state.counter; i++)
        {
            u64 begin = 0, end = 0;
            D3D11_QUERY_DATA_TIMESTAMP_DISJOINT disjoint_data = {};
            auto& event = curr_state.events[i];

            while (DX11Context->GetData(event.begin.Get(), &begin, sizeof(begin), 0) != S_OK);
            while (DX11Context->GetData(event.end.Get(), &end, sizeof(end), 0) != S_OK);
            while (DX11Context->GetData(event.disjoint.Get(), &disjoint_data, sizeof(disjoint_data), 0) != S_OK);

            auto& perf_event = events_state.perf.events[events_state.perf.count];
            perf_event.begin = begin;
            perf_event.end = end;
            perf_event.freq = disjoint_data.Frequency;
            perf_event.stack = event.stack_idx;
            perf_event.name = event.name;

            events_state.perf.count++;
        }
    }

    curr_state.counter = 0;

    GPUEvents_PushEvent("Frame");
    RenderingAtFrame = true;
}

int GPUEvents_PushEvent(const char* name)
{
    auto& curr_state = events_state.states[events_state.frame % 2];
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
    auto& curr_state = events_state.states[events_state.frame % 2];
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
    return events_state.perf;
}
#endif