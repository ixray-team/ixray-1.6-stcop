#include "Device.h"
#include "GPUEvents.h"

#if defined(IXRAY_PROFILER_TRACY)
TracyD3D12Ctx g_tracyD3D12GPUContext = nullptr;
#endif

InternalDX12GPUEventWrapper::InternalDX12GPUEventWrapper(const char* name, const void* location)
{
    auto& device = *static_cast<InternalDevice12*>(GRHI->DevicePtr);
    InternalDevice12::ContextLock guard(device);
    auto commands = device.Commands();
    device.BeginMarker(name);
    _index = device.PushGPUEvent(name);
#if defined(IXRAY_PROFILER)
    if (Core.ParamsData.test(ECoreParams::prof_gpu))
    {
        _optick = Optick::GPUEvent::Start(*Optick::EventDescription::CreateShared(name));
    }
#endif
#if defined(IXRAY_PROFILER_TRACY)
    if (location && g_tracyD3D12GPUContext)
    {
#if defined(TRACY_HAS_CALLSTACK) && defined(TRACY_CALLSTACK)
        _zone.emplace(g_tracyD3D12GPUContext, commands, static_cast<const tracy::SourceLocationData*>(location), TRACY_CALLSTACK, true);
#else
        _zone.emplace(g_tracyD3D12GPUContext, commands, static_cast<const tracy::SourceLocationData*>(location), true);
#endif
    }
#endif
}

InternalDX12GPUEventWrapper::~InternalDX12GPUEventWrapper()
{
    auto& device = *static_cast<InternalDevice12*>(GRHI->DevicePtr);
    InternalDevice12::ContextLock guard(device);
#if defined(IXRAY_PROFILER_TRACY)
    _zone.reset();
#endif
    device.PopGPUEvent(_index);
    device.EndMarker();
#if defined(IXRAY_PROFILER)
    if (_optick)
    {
        Optick::GPUEvent::Stop(*static_cast<Optick::EventData*>(_optick));
    }
#endif
}

int InternalDevice12::PushGPUEvent(const char* name)
{
    ContextLock guard(*this);
    if (!_statsActive || !GRHI->GPUStatsEnable)
    {
        return -1;
    }
    auto& frame = _frames[_frame];
    if (frame.Events.count == QUERY_MAX_COUNT)
    {
        return -1;
    }
    const u32 index = u32(frame.Events.count++);
    auto& event = frame.Events.events[index];
    event = { _statsFrequency, 0, 0, _statsStack++, name };
    Commands()->EndQuery(frame.Timestamps, D3D12_QUERY_TYPE_TIMESTAMP, index * 2);
    return int(_frame * QUERY_MAX_COUNT + index);
}

void InternalDevice12::PopGPUEvent(int index)
{
    ContextLock guard(*this);
    if (index < 0)
    {
        return;
    }
    auto& frame = _frames[u32(index) / QUERY_MAX_COUNT];
    const u32 slot = u32(index) % QUERY_MAX_COUNT;
    Commands()->EndQuery(frame.Timestamps, D3D12_QUERY_TYPE_TIMESTAMP, slot * 2 + 1);
    _commands->ResolveQueryData(frame.Timestamps, D3D12_QUERY_TYPE_TIMESTAMP, slot * 2, 2,
        frame.TimestampReadback, u64(slot) * 2 * sizeof(u64));
    frame.StatsFence = PendingFence();
    R_ASSERT(_statsStack);
    --_statsStack;
}

void InternalDevice12::BeginGPUStats()
{
    ContextLock guard(*this);
    GPUStats();
    auto& frame = _frames[_frame];
    if (frame.StatsFence)
    {
        Wait(frame.StatsFence);
    }
    if (!frame.Timestamps)
    {
        D3D12_QUERY_HEAP_DESC desc = {};
        desc.Type = D3D12_QUERY_HEAP_TYPE_TIMESTAMP;
        desc.Count = QUERY_MAX_COUNT * 2;
        R_CHK(GetDevice()->CreateQueryHeap(&desc, IID_PPV_ARGS(&frame.Timestamps)));
        frame.TimestampReadback = CreateNativeBuffer(QUERY_MAX_COUNT * 2 * sizeof(u64), D3D12_HEAP_TYPE_READBACK);
    }
    frame.Events.count = 0;
    frame.StatsFence = 0;
    frame.StatsSerial = ++_statsSerial;
    _statsStack = 0;
    _statsActive = true;
    _frameEvent = PushGPUEvent("Frame");
}

void InternalDevice12::EndGPUStats()
{
    ContextLock guard(*this);
    if (!_statsActive)
    {
        return;
    }
    PopGPUEvent(_frameEvent);
    _frameEvent = -1;
    _statsActive = false;
}

const RHI_GPU_EVENT& InternalDevice12::GPUStats()
{
    ContextLock guard(*this);
    for (auto& frame : _frames)
    {
        if (!frame.StatsFence || frame.StatsSerial <= _lastStatsSerial || !IsComplete(frame.StatsFence))
        {
            continue;
        }
        const u64* timestamps = nullptr;
        D3D12_RANGE range = { 0, size_t(frame.Events.count) * 2 * sizeof(u64) };
        R_CHK(frame.TimestampReadback->Map(0, &range, (void**)&timestamps));
        _lastStats = frame.Events;
        for (u32 event_idx = 0; event_idx < _lastStats.count; ++event_idx)
        {
            _lastStats.events[event_idx].begin = timestamps[event_idx * 2];
            _lastStats.events[event_idx].end = timestamps[event_idx * 2 + 1];
        }
        D3D12_RANGE written = {};
        frame.TimestampReadback->Unmap(0, &written);
        _lastStatsSerial = frame.StatsSerial;
    }
    return _lastStats;
}

void InternalDevice12::RecordMarker(const char* name)
{
    R_ASSERT(name && _isRecording);
    u64 data[64] = {};
    data[0] = u64(2) << 10;
    data[1] = 0xffffffff;
    data[2] = (u64(8) << 55) | (u64(1) << 54);
    const size_t length = strnlen(name, sizeof(data) - 5 * sizeof(u64) - 1);
    memcpy(data + 3, name, length);
    const size_t stringSize = (length + 1 + sizeof(u64) - 1) & ~(sizeof(u64) - 1);
    _commands->BeginEvent(2, data, UINT(3 * sizeof(u64) + stringSize));
}

void InternalDevice12::BeginMarker(const char* name)
{
    ContextLock guard(*this);
    Commands();
    RecordMarker(name);
    _markers.push_back(name);
}

void InternalDevice12::EndMarker()
{
    ContextLock guard(*this);
    R_ASSERT(!_markers.empty());
    Commands()->EndEvent();
    _markers.pop_back();
}
