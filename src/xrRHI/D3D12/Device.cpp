#include "Device.h"
#include "GPUEvents.h"
#include "../RHITopologyUtils.h"
#include <d3d12sdklayers.h>

static void CALLBACK DebugMessage(D3D12_MESSAGE_CATEGORY, D3D12_MESSAGE_SEVERITY severity,
    D3D12_MESSAGE_ID id, LPCSTR description, void*)
{
    if (severity <= D3D12_MESSAGE_SEVERITY_ERROR)
    {
        Msg("! D3D12 message %u: %s", u32(id), description);
        xrLogger::FlushLog();
    }
}

InternalDevice12::ContextLock::ContextLock(const InternalDevice12& device) : _device(&device)
{
    device._contextMutex.Enter();
}

InternalDevice12::ContextLock::~ContextLock()
{
    _device->_contextMutex.Leave();
}

u64 InternalDevice12::Tick() const
{
    LARGE_INTEGER tick;
    QueryPerformanceCounter(&tick);
    return u64(tick.QuadPart);
}

bool InternalDevice12::StateCovers(D3D12_RESOURCE_STATES before, D3D12_RESOURCE_STATES after) const
{
    constexpr D3D12_RESOURCE_STATES writes = D3D12_RESOURCE_STATE_RENDER_TARGET | D3D12_RESOURCE_STATE_UNORDERED_ACCESS |
        D3D12_RESOURCE_STATE_DEPTH_WRITE | D3D12_RESOURCE_STATE_STREAM_OUT | D3D12_RESOURCE_STATE_COPY_DEST |
        D3D12_RESOURCE_STATE_RESOLVE_DEST;
    return (before & after) == after && (before & writes) == (after & writes);
}

InternalDevice12::InternalDevice12()
{
    _images.resize(StaticResourceDescriptors, nullptr);
    if (Core.ParamsData.test(ECoreParams::dxdebug))
    {
        ID3D12DeviceRemovedExtendedDataSettings* dred = nullptr;
        if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&dred))))
        {
            dred->SetAutoBreadcrumbsEnablement(D3D12_DRED_ENABLEMENT_FORCED_ON);
            dred->SetPageFaultEnablement(D3D12_DRED_ENABLEMENT_FORCED_ON);
            dred->Release();
        }
        ID3D12Debug* debug = nullptr;
        if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug))))
        {
            debug->EnableDebugLayer();
            ID3D12Debug1* validation = nullptr;
            if (SUCCEEDED(debug->QueryInterface(IID_PPV_ARGS(&validation))))
            {
                validation->SetEnableGPUBasedValidation(TRUE);
                validation->SetEnableSynchronizedCommandQueueValidation(TRUE);
                validation->Release();
                Msg("* D3D12 GPU validation enabled");
            }
            debug->Release();
        }
        else
        {
            Msg("! D3D12 debug layer unavailable");
        }
    }
    R_CHK(CreateDXGIFactory1(IID_PPV_ARGS(&_factory)));
    IDXGIAdapter1* adapter = nullptr;
    for (u32 adapter_idx = 0; _factory->EnumAdapters1(adapter_idx, &adapter) != DXGI_ERROR_NOT_FOUND; ++adapter_idx)
    {
        DXGI_ADAPTER_DESC1 adapterDesc = {};
        R_CHK(adapter->GetDesc1(&adapterDesc));
        if (!(adapterDesc.Flags & DXGI_ADAPTER_FLAG_SOFTWARE) &&
            SUCCEEDED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_11_0, __uuidof(ID3D12Device), &RawDevice)))
        {
            Msg("* D3D12 adapter: %ls", adapterDesc.Description);
            adapter->Release();
            adapter = nullptr;
            break;
        }
        adapter->Release();
        adapter = nullptr;
    }
    R_ASSERT2(RawDevice, "Unable to create a native D3D12 device");
    if (Core.ParamsData.test(ECoreParams::dxdebug))
    {
        ID3D12InfoQueue* messages = nullptr;
        if (SUCCEEDED(GetDevice()->QueryInterface(IID_PPV_ARGS(&messages))))
        {
            messages->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_CORRUPTION, TRUE);
            messages->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_ERROR, TRUE);
            messages->Release();
        }
        if (SUCCEEDED(GetDevice()->QueryInterface(IID_PPV_ARGS(&_debugQueue))))
        {
            if (FAILED(_debugQueue->RegisterMessageCallback(DebugMessage, D3D12_MESSAGE_CALLBACK_FLAG_NONE,
                nullptr, &_debugCallback)))
            {
                _debugQueue->Release();
                _debugQueue = nullptr;
                Msg("! D3D12 debug message callback unavailable");
            }
        }
    }
    FeatureLevel = RHI_FEATURE_LEVEL_11_1;
    VertexCache = 32;
    D3D12_FEATURE_DATA_D3D12_OPTIONS2 options = {};
    _canUseDepthBounds = SUCCEEDED(GetDevice()->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS2, &options, sizeof(options))) &&
        options.DepthBoundsTestSupported;
    D3D12_COMMAND_QUEUE_DESC queueDesc = {};
    queueDesc.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
    R_CHK(GetDevice()->CreateCommandQueue(&queueDesc, IID_PPV_ARGS(&_queue)));
    _queue->SetName(L"xrRHI D3D12 graphics queue");
    R_CHK(_queue->GetTimestampFrequency(&_statsFrequency));
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(&frequency);
    _tickFrequency = u64(frequency.QuadPart);
#if defined(IXRAY_PROFILER)
    ID3D12CommandQueue* queues[] = { _queue };
    Optick::InitGpuD3D12(GetDevice(), queues, 1);
#endif
    D3D12_QUERY_HEAP_DESC occlusion = {};
    occlusion.Type = D3D12_QUERY_HEAP_TYPE_OCCLUSION;
    occlusion.Count = QuerySlots;
    R_CHK(GetDevice()->CreateQueryHeap(&occlusion, IID_PPV_ARGS(&_occlusionHeap)));
    _occlusionReadback = CreateNativeBuffer(QuerySlots * sizeof(u64), D3D12_HEAP_TYPE_READBACK);
    for (u32 slot = 0; slot < QuerySlots; ++slot)
    {
        _occlusionFree[_occlusionFreeCount++] = slot;
    }
#if defined(IXRAY_PROFILER_TRACY)
    g_tracyD3D12GPUContext = TracyD3D12Context(GetDevice(), _queue);
    TracyD3D12ContextName(g_tracyD3D12GPUContext, "D3D12 graphics", sizeof("D3D12 graphics") - 1);
#endif
    for (u32 frame_idx = 0; frame_idx < FrameCount; ++frame_idx)
    {
        R_CHK(GetDevice()->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&_frames[frame_idx].Allocator)));
    }
    R_CHK(GetDevice()->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, _frames[0].Allocator, nullptr, IID_PPV_ARGS(&_commands)));
    _commands->SetName(L"xrRHI D3D12 graphics commands");
    R_CHK(_commands->Close());
    _commands->QueryInterface(IID_PPV_ARGS(&_commands1));
    R_CHK(GetDevice()->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&_fence)));
    _fence->SetName(L"xrRHI D3D12 graphics fence");
    _fenceEvent = CreateEventW(nullptr, FALSE, FALSE, nullptr);
    R_ASSERT(_fenceEvent);
    const u32 descriptorCounts[4] = { ResourceDescriptorCount, SamplerDescriptorCount, 65536, 4096 };
    for (u32 type_idx = 0; type_idx < 4; ++type_idx)
    {
        _descriptorStride[type_idx] = GetDevice()->GetDescriptorHandleIncrementSize((D3D12_DESCRIPTOR_HEAP_TYPE)type_idx);
    }
    for (u32 heap_idx = 0; heap_idx < 4; ++heap_idx)
    {
        D3D12_DESCRIPTOR_HEAP_DESC heapDesc = {};
        heapDesc.Type = (D3D12_DESCRIPTOR_HEAP_TYPE)heap_idx;
        heapDesc.NumDescriptors = descriptorCounts[heap_idx];
        heapDesc.Flags = heap_idx < 2 ? D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE : D3D12_DESCRIPTOR_HEAP_FLAG_NONE;
        ID3D12DescriptorHeap** heaps[4] = { &_resourceHeap, &_samplerHeap, &_rtvHeap, &_dsvHeap };
        R_CHK(GetDevice()->CreateDescriptorHeap(&heapDesc, IID_PPV_ARGS(heaps[heap_idx])));
        if (heap_idx < 2)
        {
            heapDesc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_NONE;
            heapDesc.NumDescriptors = heap_idx ? StaticSamplerDescriptors : StaticResourceDescriptors;
            R_CHK(GetDevice()->CreateDescriptorHeap(&heapDesc, IID_PPV_ARGS(&_storageHeaps[heap_idx])));
        }
    }
    _nullTarget = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    D3D12_RENDER_TARGET_VIEW_DESC nullTarget = {};
    nullTarget.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    nullTarget.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE2D;
    GetDevice()->CreateRenderTargetView(nullptr, &nullTarget, _nullTarget.Cpu);
    NullSampler();
    CreateRootSignatures();
    HWND window = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
    R_ASSERT(window);
    DXGI_SWAP_CHAIN_DESC1 swapDesc = {};
    swapDesc.Width = psCurrentVidMode[0];
    swapDesc.Height = psCurrentVidMode[1];
    swapDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    swapDesc.SampleDesc.Count = 1;
    swapDesc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    swapDesc.BufferCount = FrameCount;
    swapDesc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
    IDXGISwapChain1* swapchain = nullptr;
    R_CHK(_factory->CreateSwapChainForHwnd(_queue, window, &swapDesc, nullptr, nullptr, &swapchain));
    R_CHK(swapchain->QueryInterface(IID_PPV_ARGS(&_swapchain)));
    swapchain->Release();
    R_CHK(_factory->MakeWindowAssociation(window, DXGI_MWA_NO_ALT_ENTER));
    _frame = _swapchain->GetCurrentBackBufferIndex();
    _textureFactory = new DX12TextureFactory(*this);
    TextureFactory = _textureFactory;
    UpdateBuffers();
}

InternalDevice12::~InternalDevice12()
{
    Flush();
#if defined(IXRAY_PROFILER_TRACY)
    if (g_tracyD3D12GPUContext)
    {
        TracyD3D12Collect(g_tracyD3D12GPUContext);
        TracyD3D12Destroy(g_tracyD3D12GPUContext);
        g_tracyD3D12GPUContext = nullptr;
    }
#endif
    ReleaseBuffers();
    xr_delete(_textureFactory);
    TextureFactory = nullptr;
    for (auto& pipeline : _graphicsPipelines)
    {
        pipeline.Pipeline->Release();
    }
    for (auto& pipeline : _computePipelines)
    {
        pipeline.Pipeline->Release();
    }
    for (auto state : _rasterStates)
    {
        xr_delete(state);
    }
    for (auto state : _depthStates)
    {
        xr_delete(state);
    }
    for (auto state : _blendStates)
    {
        xr_delete(state);
    }
    CollectRetired();
    for (auto& frame : _frames)
    {
        Wait(frame.Fence);
        for (auto& chunk : frame.Uploads)
        {
            chunk.Resource->Unmap(0, nullptr);
            chunk.Resource->Release();
        }
        if (frame.Timestamps)
        {
            frame.Timestamps->Release();
            frame.TimestampReadback->Release();
        }
        frame.Allocator->Release();
    }
    for (auto& stream : _streamPool)
    {
        stream.Resource->Unmap(0, nullptr);
        stream.Resource->Release();
    }
    if (_commands1)
    {
        _commands1->Release();
    }
    for (auto& pipeline : _mipPipelines)
    {
        pipeline.Pipeline->Release();
    }
    if (_mipRoot)
    {
        _mipRoot->Release();
    }
    if (_occlusionHeap)
    {
        _occlusionHeap->Release();
        _occlusionReadback->Release();
    }
    _commands->Release();
    _graphicsRoot->Release();
    _computeRoot->Release();
    _storageHeaps[0]->Release();
    _storageHeaps[1]->Release();
    _resourceHeap->Release();
    _samplerHeap->Release();
    _rtvHeap->Release();
    _dsvHeap->Release();
    _swapchain->Release();
    _queue->Release();
    _fence->Release();
    CloseHandle(_fenceEvent);
    if (_debugQueue)
    {
        _debugQueue->UnregisterMessageCallback(_debugCallback);
        _debugQueue->Release();
    }
    GetDevice()->Release();
    RawDevice = nullptr;
    _factory->Release();
}

ID3D12DescriptorHeap* InternalDevice12::Heap(D3D12_DESCRIPTOR_HEAP_TYPE type) const
{
    ID3D12DescriptorHeap* heaps[4] = { _resourceHeap, _samplerHeap, _rtvHeap, _dsvHeap };
    R_ASSERT((u32)type < 4);
    return heaps[type];
}

DX12Descriptor InternalDevice12::AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE type)
{
    ContextLock guard(*this);
    const u32 limits[4] = { StaticResourceDescriptors, StaticSamplerDescriptors, 65536, 4096 };
    DX12Descriptor descriptor;
    descriptor.Type = type;
    descriptor.Generation = _nextDescriptor++;
    auto& free = _freeDescriptors[type];
    if (free.empty())
    {
        R_ASSERT2(_descriptorUsed[type] < limits[type], "D3D12 descriptor storage exhausted");
        descriptor.Index = _descriptorUsed[type]++;
    }
    else
    {
        descriptor.Index = free.back();
        free.pop_back();
    }
    const u32 stride = _descriptorStride[type];
    descriptor.Cpu = (type < D3D12_DESCRIPTOR_HEAP_TYPE_RTV ? _storageHeaps[type] : Heap(type))->GetCPUDescriptorHandleForHeapStart();
    descriptor.Cpu.ptr += u64(descriptor.Index) * stride;
    if (type < D3D12_DESCRIPTOR_HEAP_TYPE_RTV)
    {
        descriptor.Gpu = Heap(type)->GetGPUDescriptorHandleForHeapStart();
        descriptor.Gpu.ptr += u64(descriptor.Index) * stride;
    }
    return descriptor;
}

D3D12_CPU_DESCRIPTOR_HANDLE InternalDevice12::VisibleCPU(const DX12Descriptor& descriptor) const
{
    R_ASSERT(descriptor.Type < D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    auto cpu = Heap(descriptor.Type)->GetCPUDescriptorHandleForHeapStart();
    cpu.ptr += u64(descriptor.Index) * _descriptorStride[descriptor.Type];
    return cpu;
}

void InternalDevice12::PublishDescriptor(const DX12Descriptor& descriptor)
{
    ContextLock guard(*this);
    GetDevice()->CopyDescriptorsSimple(1, VisibleCPU(descriptor), descriptor.Cpu, descriptor.Type);
}

void InternalDevice12::Retire(IUnknown* object)
{
    ContextLock guard(*this);
    if (object)
    {
        _retiredObjects.push_back({ _isRecording ? _nextFence : _nextFence - 1, object });
    }
}

void InternalDevice12::Retire(const DX12Descriptor& descriptor)
{
    ContextLock guard(*this);
    if (descriptor.Index != UINT32_MAX)
    {
        _retiredDescriptors.push_back({ _isRecording ? _nextFence : _nextFence - 1, descriptor });
    }
}

void InternalDevice12::ReportDeviceRemoval() const
{
    Msg("! D3D12 device removed: 0x%08x", u32(GetDevice()->GetDeviceRemovedReason()));
    ID3D12InfoQueue* messages = nullptr;
    if (SUCCEEDED(GetDevice()->QueryInterface(IID_PPV_ARGS(&messages))))
    {
        const u64 count = messages->GetNumStoredMessagesAllowedByRetrievalFilter();
        xr_vector<u8> storage;
        for (u64 message_idx = count > 32 ? count - 32 : 0; message_idx < count; ++message_idx)
        {
            SIZE_T size = 0;
            if (FAILED(messages->GetMessage(message_idx, nullptr, &size)))
            {
                continue;
            }
            storage.resize(size);
            auto message = reinterpret_cast<D3D12_MESSAGE*>(storage.data());
            if (SUCCEEDED(messages->GetMessage(message_idx, message, &size)))
            {
                Msg("! D3D12 message %u: %s", u32(message->ID), message->pDescription);
            }
        }
        messages->Release();
    }
    ID3D12DeviceRemovedExtendedData* dred = nullptr;
    if (SUCCEEDED(GetDevice()->QueryInterface(IID_PPV_ARGS(&dred))))
    {
        D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT breadcrumbs = {};
        if (SUCCEEDED(dred->GetAutoBreadcrumbsOutput(&breadcrumbs)))
        {
            for (auto node = breadcrumbs.pHeadAutoBreadcrumbNode; node; node = node->pNext)
            {
                const u32 completed = node->pLastBreadcrumbValue ? *node->pLastBreadcrumbValue : 0;
                if (completed < node->BreadcrumbCount)
                {
                    Msg("! D3D12 unfinished command list %p: %u/%u, next operation %u",
                        node->pCommandList, completed, node->BreadcrumbCount,
                        node->pCommandHistory ? u32(node->pCommandHistory[completed]) : UINT32_MAX);
                }
            }
        }
        D3D12_DRED_PAGE_FAULT_OUTPUT fault = {};
        if (SUCCEEDED(dred->GetPageFaultAllocationOutput(&fault)) && fault.PageFaultVA)
        {
            Msg("! D3D12 GPU page fault: 0x%llx", fault.PageFaultVA);
            for (auto node = fault.pHeadRecentFreedAllocationNode; node; node = node->pNext)
            {
                Msg("! D3D12 freed allocation: %s, type %u", node->ObjectNameA ? node->ObjectNameA : "<unnamed>",
                    u32(node->AllocationType));
            }
        }
        dred->Release();
    }
    xrLogger::FlushLog();
}

bool InternalDevice12::IsComplete(u64 fence) const
{
    const u64 completed = _fence->GetCompletedValue();
    if (completed == UINT64_MAX)
    {
        ReportDeviceRemoval();
        R_ASSERT2(false, "D3D12 device removed; see removal reason and GPU diagnostics in the log");
        return false;
    }
    return completed >= fence;
}

void InternalDevice12::Wait(u64 fence)
{
    if (!IsComplete(fence))
    {
        PROF_EVENT("D3D12: WaitFence");
        R_CHK(_fence->SetEventOnCompletion(fence, _fenceEvent));
        R_ASSERT(WaitForSingleObject(_fenceEvent, INFINITE) == WAIT_OBJECT_0);
        R_ASSERT(IsComplete(fence));
    }
}

void InternalDevice12::CollectRetired()
{
    PROF_EVENT("D3D12: CollectRetired");
    const u64 completed = _fence->GetCompletedValue();
    if (completed == UINT64_MAX)
    {
        IsComplete(0);
        return;
    }
    for (size_t object_idx = 0; object_idx < _retiredObjects.size();)
    {
        if (completed < _retiredObjects[object_idx].Fence)
        {
            ++object_idx;
            continue;
        }
        _retiredObjects[object_idx].Object->Release();
        _retiredObjects[object_idx] = _retiredObjects.back();
        _retiredObjects.pop_back();
    }
    for (size_t descriptor_idx = 0; descriptor_idx < _retiredDescriptors.size();)
    {
        auto& retired = _retiredDescriptors[descriptor_idx];
        if (completed < retired.Fence)
        {
            ++descriptor_idx;
            continue;
        }
        _freeDescriptors[retired.Descriptor.Type].push_back(retired.Descriptor.Index);
        retired = _retiredDescriptors.back();
        _retiredDescriptors.pop_back();
    }
}

DX12Upload InternalDevice12::AcquireStream(u64 size)
{
    DX12Upload upload;
    upload.Size = size;
    upload.Owned = true;
    const u64 completed = _fence->GetCompletedValue();
    if (completed == UINT64_MAX)
    {
        IsComplete(0);
    }
    for (size_t stream_idx = 0; stream_idx < _streamPool.size(); ++stream_idx)
    {
        auto& stream = _streamPool[stream_idx];
        if (stream.Size == size && completed >= stream.Fence)
        {
            upload.Resource = stream.Resource;
            upload.Data = stream.Data;
            stream = _streamPool.back();
            _streamPool.pop_back();
            return upload;
        }
    }
    upload.Resource = CreateNativeBuffer(size, D3D12_HEAP_TYPE_UPLOAD);
    D3D12_RANGE range = {};
    R_CHK(upload.Resource->Map(0, &range, (void**)&upload.Data));
    return upload;
}

void InternalDevice12::RecycleStream(const DX12Upload& upload)
{
    if (_streamPool.size() >= 64)
    {
        upload.Resource->Unmap(0, nullptr);
        Retire(upload.Resource);
        return;
    }
    _streamPool.push_back({ upload.Resource, upload.Data, upload.Size, _isRecording ? _nextFence : _nextFence - 1 });
}

ID3D12GraphicsCommandList* InternalDevice12::Commands()
{
    ContextLock guard(*this);
    if (!_isRecording)
    {
        Frame& frame = _frames[_frame];
        Wait(frame.Fence);
        R_CHK(frame.Allocator->Reset());
        R_CHK(_commands->Reset(frame.Allocator, nullptr));
        _isRecording = true;
        ++_epoch;
        for (auto name : _markers)
        {
            RecordMarker(name);
        }
        InvalidateBindings();
        CollectRetired();
#if defined(IXRAY_PROFILER)
        if (_gpuContextLive)
        {
            Optick::SetGpuContext(Optick::GPUContext(_commands));
        }
#endif
    }
    if (!_draining && !_preparing)
    {
        DrainUploads();
    }
    return _commands;
}

ID3D12GraphicsCommandList* InternalDevice12::ActiveCommands()
{
    if (_isRecording && (_draining || _preparing || !_pendingCount.load(std::memory_order_relaxed)))
    {
        return _commands;
    }
    return Commands();
}

u64 InternalDevice12::GetEpoch()
{
    ContextLock guard(*this);
    Commands();
    return _epoch;
}

u64 InternalDevice12::Submit()
{
    PROF_EVENT("D3D12: Submit");
    DrainUploads();
    if (!_isRecording)
    {
        return _nextFence - 1;
    }
    ResolveQueries();
    for (size_t marker_idx = _markers.size(); marker_idx; --marker_idx)
    {
        _commands->EndEvent();
    }
    R_CHK(_commands->Close());
    ID3D12CommandList* lists[] = { _commands };
    _queue->ExecuteCommandLists(1, lists);
    const u64 submitted = _nextFence++;
    R_CHK(_queue->Signal(_fence, submitted));
    _frames[_frame].Fence = submitted;
    _isRecording = false;
    return submitted;
}

void InternalDevice12::Flush()
{
    PROF_EVENT("D3D12: Flush");
    ContextLock guard(*this);
    Wait(Submit());
    CollectRetired();
}

void InternalDevice12::BeginFrame()
{
    ContextLock guard(*this);
    Commands();
#if defined(IXRAY_PROFILER)
    if (_gpuContextLive)
    {
        Optick::SetGpuContext(Optick::GPUContext(_prevGpuCommand, (Optick::GPUQueueType)_prevGpuQueue, _prevGpuNode));
    }
    const auto previous = Optick::SetGpuContext(Optick::GPUContext(_commands));
    _prevGpuCommand = previous.cmdBuffer;
    _prevGpuQueue = u32(previous.queue);
    _prevGpuNode = previous.node;
    _gpuContextLive = true;
#endif
#if defined(IXRAY_PROFILER_TRACY)
    TracyD3D12NewFrame(g_tracyD3D12GPUContext);
#endif
}

void* InternalDevice12::GetContext()
{
    ContextLock guard(*this);
    return Commands();
}

void* InternalDevice12::GetSwapchain()
{
    return _swapchain;
}

DX12Upload InternalDevice12::AllocateUpload(u64 size, u64 alignment)
{
    R_ASSERT(size && alignment && !(alignment & (alignment - 1)) && size <= UINT64_MAX - alignment);
    _uploadLock.Enter();
    Frame& frame = _frames[_frame];
    for (auto& chunk : frame.Uploads)
    {
        const u64 offset = (chunk.Used + alignment - 1) & ~(alignment - 1);
        if (offset <= chunk.Size && size <= chunk.Size - offset)
        {
            chunk.Used = offset + size;
            DX12Upload upload = { chunk.Resource, chunk.Data + offset, offset, size, frame.UploadSerial, _frame, false };
            _uploadLock.Leave();
            _counters.UploadBytes.fetch_add(size, std::memory_order_relaxed);
            return upload;
        }
    }
    UploadChunk chunk;
    chunk.Size = std::max(u64(4 * 1024 * 1024), (size + alignment - 1) & ~(alignment - 1));
    chunk.Resource = CreateNativeBuffer(chunk.Size, D3D12_HEAP_TYPE_UPLOAD);
    D3D12_RANGE range = {};
    R_CHK(chunk.Resource->Map(0, &range, (void**)&chunk.Data));
    chunk.Used = size;
    frame.Uploads.push_back(chunk);
    DX12Upload upload = { chunk.Resource, chunk.Data, 0, size, frame.UploadSerial, _frame, false };
    _uploadLock.Leave();
    _counters.UploadBytes.fetch_add(size, std::memory_order_relaxed);
    return upload;
}

ID3D12Resource* InternalDevice12::CreateNativeBuffer(u64 size, D3D12_HEAP_TYPE heap, D3D12_RESOURCE_FLAGS flags)
{
    D3D12_HEAP_PROPERTIES properties = {};
    properties.Type = heap;
    D3D12_RESOURCE_DESC desc = {};
    desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    desc.Width = size;
    desc.Height = 1;
    desc.DepthOrArraySize = 1;
    desc.MipLevels = 1;
    desc.SampleDesc.Count = 1;
    desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    desc.Flags = flags;
    ID3D12Resource* resource = nullptr;
    D3D12_RESOURCE_STATES state = heap == D3D12_HEAP_TYPE_UPLOAD ? D3D12_RESOURCE_STATE_GENERIC_READ :
        heap == D3D12_HEAP_TYPE_READBACK ? D3D12_RESOURCE_STATE_COPY_DEST : D3D12_RESOURCE_STATE_COMMON;
    R_CHK(GetDevice()->CreateCommittedResource(&properties, D3D12_HEAP_FLAG_NONE, &desc, state, nullptr, IID_PPV_ARGS(&resource)));
    _counters.Allocations.fetch_add(1, std::memory_order_relaxed);
    return resource;
}

void InternalDevice12::Transition(DX12Resource& resource, D3D12_RESOURCE_STATES state, u32 subresource)
{
    if (resource.Uniform && StateCovers(resource.UniformState, state))
    {
        return;
    }
    ContextLock guard(*this);
    R_ASSERT(resource.Native && !resource.States.empty());
    D3D12_RESOURCE_BARRIER batch[BarrierBatch];
    u32 count = 0;
    auto flush = [&]()
    {
        if (!count)
        {
            return;
        }
        Commands()->ResourceBarrier(count, batch);
        _counters.Barriers.fetch_add(count, std::memory_order_relaxed);
        ++_barrierEpoch;
        count = 0;
    };
    auto push = [&](u32 subresource_idx)
    {
        const auto before = resource.States[subresource_idx];
        if (StateCovers(before, state))
        {
            return;
        }
        if (count == BarrierBatch)
        {
            flush();
        }
        auto& barrier = batch[count++];
        barrier = {};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Transition.pResource = resource.Native;
        barrier.Transition.Subresource = subresource_idx;
        barrier.Transition.StateBefore = before;
        barrier.Transition.StateAfter = state;
        resource.States[subresource_idx] = state;
        resource.Uniform = false;
    };
    if (subresource == D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES &&
        std::all_of(resource.States.begin(), resource.States.end(),
            [&](D3D12_RESOURCE_STATES before) { return before == resource.States.front(); }))
    {
        if (!StateCovers(resource.States.front(), state))
        {
            D3D12_RESOURCE_BARRIER barrier = {};
            barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
            barrier.Transition.pResource = resource.Native;
            barrier.Transition.StateBefore = resource.States.front();
            barrier.Transition.StateAfter = state;
            barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
            Commands()->ResourceBarrier(1, &barrier);
            _counters.Barriers.fetch_add(1, std::memory_order_relaxed);
            ++_barrierEpoch;
            if ((barrier.Transition.StateBefore & D3D12_RESOURCE_STATE_UNORDERED_ACCESS) &&
                !(state & D3D12_RESOURCE_STATE_UNORDERED_ACCESS))
            {
                resource.UAVPendingEpoch = 0;
            }
            std::fill(resource.States.begin(), resource.States.end(), state);
            resource.UniformState = state;
        }
        else
        {
            resource.UniformState = resource.States.front();
        }
        resource.Uniform = true;
        return;
    }
    if (subresource == D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES)
    {
        for (u32 subresource_idx = 0; subresource_idx < resource.States.size(); ++subresource_idx)
        {
            push(subresource_idx);
        }
    }
    else
    {
        R_ASSERT(subresource < resource.States.size());
        push(subresource);
    }
    flush();
    if (subresource == D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES)
    {
        resource.Uniform = std::all_of(resource.States.begin(), resource.States.end(),
            [&](D3D12_RESOURCE_STATES before) { return before == resource.States.front(); });
        if (resource.Uniform)
        {
            resource.UniformState = resource.States.front();
        }
    }
}

void InternalDevice12::TransitionView(DX12Surface& surface, D3D12_RESOURCE_STATES state, const DX12View& view)
{
    auto& resource = surface.GetResource();
    if (resource.Uniform && StateCovers(resource.UniformState, state))
    {
        return;
    }
    const bool volume = surface.GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE3D;
    const u32 mips = surface.GetMipLevels();
    const u32 layers = volume ? 1u : surface.GetArraySize();
    const u32 planes = view.Planes ? view.Planes : 1u;
    const u32 mipCount = view.MipCount ? view.MipCount : 1u;
    const u32 sliceCount = volume ? 1u : (view.SliceCount ? view.SliceCount : 1u);
    const bool full = !view.Mip && mipCount >= mips && !view.FirstSlice && sliceCount >= layers &&
        !view.Plane && planes * mips * layers == resource.States.size();
    if (full)
    {
        Transition(resource, state);
        return;
    }
    ContextLock guard(*this);
    D3D12_RESOURCE_BARRIER batch[BarrierBatch];
    u32 count = 0;
    auto flush = [&]()
    {
        if (!count)
        {
            return;
        }
        Commands()->ResourceBarrier(count, batch);
        _counters.Barriers.fetch_add(count, std::memory_order_relaxed);
        ++_barrierEpoch;
        count = 0;
    };
    auto push = [&](u32 subresource_idx)
    {
        R_ASSERT(subresource_idx < resource.States.size());
        const auto before = resource.States[subresource_idx];
        if (StateCovers(before, state))
        {
            return;
        }
        if (count == BarrierBatch)
        {
            flush();
        }
        auto& barrier = batch[count++];
        barrier = {};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Transition.pResource = resource.Native;
        barrier.Transition.Subresource = subresource_idx;
        barrier.Transition.StateBefore = before;
        barrier.Transition.StateAfter = state;
        resource.States[subresource_idx] = state;
        resource.Uniform = false;
    };
    for (u32 plane_idx = 0; plane_idx < planes; ++plane_idx)
    {
        const u32 planeBase = (view.Plane + plane_idx) * mips * layers;
        if (volume)
        {
            for (u32 mip_idx = 0; mip_idx < mipCount; ++mip_idx)
            {
                push(planeBase + view.Mip + mip_idx);
            }
        }
        else
        {
            for (u32 slice_idx = 0; slice_idx < sliceCount; ++slice_idx)
            {
                for (u32 mip_idx = 0; mip_idx < mipCount; ++mip_idx)
                {
                    push(planeBase + (view.FirstSlice + slice_idx) * mips + view.Mip + mip_idx);
                }
            }
        }
    }
    flush();
}

void InternalDevice12::UAVBarrier(ID3D12Resource* resource)
{
    ContextLock guard(*this);
    D3D12_RESOURCE_BARRIER barrier = {};
    barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_UAV;
    barrier.UAV.pResource = resource;
    Commands()->ResourceBarrier(1, &barrier);
    _counters.Barriers.fetch_add(1, std::memory_order_relaxed);
}

void InternalDevice12::EnqueueCopy(PendingCopy&& copy)
{
    if (copy.Surface)
    {
        copy.Surface->AddRef();
    }
    if (copy.Buffer)
    {
        copy.Buffer->AddRef();
    }
    const u64 waitStart = Tick();
    _uploadLock.Enter();
    const u64 held = Tick();
    _counters.UploadWait.fetch_add(held - waitStart, std::memory_order_relaxed);
    _pending.push_back(std::move(copy));
    _pendingCount.store(u32(_pending.size()), std::memory_order_release);
    _counters.UploadHold.fetch_add(Tick() - held, std::memory_order_relaxed);
    _uploadLock.Leave();
}

void InternalDevice12::DrainUploads()
{
    if (_draining || !_pendingCount.load(std::memory_order_acquire))
    {
        return;
    }
    xr_vector<PendingCopy> batch;
    {
        const u64 waitStart = Tick();
        _uploadLock.Enter();
        const u64 held = Tick();
        _counters.UploadWait.fetch_add(held - waitStart, std::memory_order_relaxed);
        batch.swap(_pending);
        _pendingCount.store(0, std::memory_order_release);
        _counters.UploadHold.fetch_add(Tick() - held, std::memory_order_relaxed);
        _uploadLock.Leave();
    }
    if (batch.empty())
    {
        return;
    }
    _draining = true;
    if (!_isRecording)
    {
        Commands();
    }
    for (auto& copy : batch)
    {
        if (!copy.Source.Owned && copy.Source.Frame < FrameCount && copy.Source.Frame != _frame)
        {
            auto& owner = _frames[copy.Source.Frame];
            owner.Fence = std::max(owner.Fence, _nextFence);
        }
        if (copy.Surface)
        {
            Transition(copy.Surface->GetResource(), D3D12_RESOURCE_STATE_COPY_DEST, copy.Subresource);
            D3D12_TEXTURE_COPY_LOCATION source = {};
            source.pResource = copy.Source.Resource;
            source.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
            source.PlacedFootprint = copy.Footprint;
            source.PlacedFootprint.Offset += copy.Source.Offset;
            D3D12_TEXTURE_COPY_LOCATION destination = {};
            destination.pResource = copy.Surface->GetResource().Native;
            destination.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
            destination.SubresourceIndex = copy.Subresource;
            Commands()->CopyTextureRegion(&destination, copy.DestX, copy.DestY, copy.DestZ, &source, copy.HasBox ? &copy.Box : nullptr);
            copy.Surface->Release();
        }
        else
        {
            R_ASSERT(copy.Buffer && copy.Buffer->GetResource().Native);
            Transition(copy.Buffer->GetResource(), D3D12_RESOURCE_STATE_COPY_DEST);
            Commands()->CopyBufferRegion(copy.Buffer->GetResource().Native, copy.DestOffset, copy.Source.Resource,
                copy.Source.Offset, copy.CopySize);
            copy.Buffer->Release();
        }
    }
    _draining = false;
}

DX12Descriptor InternalDevice12::NullSampler()
{
    if (_nullSampler.Index == UINT32_MAX)
    {
        _nullSampler = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER);
        D3D12_SAMPLER_DESC desc = {};
        desc.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
        desc.AddressU = desc.AddressV = desc.AddressW = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
        desc.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
        desc.MaxAnisotropy = 1;
        desc.MaxLOD = D3D12_FLOAT32_MAX;
        GetDevice()->CreateSampler(&desc, _nullSampler.Cpu);
        _counters.SamplerCreates.fetch_add(1, std::memory_order_relaxed);
    }
    return _nullSampler;
}

static DXGI_FORMAT NullFormat(u32 returnType)
{
    const auto type = D3D_RESOURCE_RETURN_TYPE(returnType);
    return type == D3D_RETURN_TYPE_UINT ? DXGI_FORMAT_R32G32B32A32_UINT :
        type == D3D_RETURN_TYPE_SINT ? DXGI_FORMAT_R32G32B32A32_SINT : DXGI_FORMAT_R32G32B32A32_FLOAT;
}

static void NullSrvShape(D3D12_SHADER_RESOURCE_VIEW_DESC& desc)
{
    switch (desc.ViewDimension)
    {
    case D3D12_SRV_DIMENSION_BUFFER: desc.Buffer.NumElements = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURE1D: desc.Texture1D.MipLevels = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURE1DARRAY: desc.Texture1DArray.MipLevels = 1; desc.Texture1DArray.ArraySize = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURE2DARRAY: desc.Texture2DArray.MipLevels = 1; desc.Texture2DArray.ArraySize = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURE2DMS: break;
    case D3D12_SRV_DIMENSION_TEXTURE2DMSARRAY: desc.Texture2DMSArray.ArraySize = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURE3D: desc.Texture3D.MipLevels = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURECUBE: desc.TextureCube.MipLevels = 1; break;
    case D3D12_SRV_DIMENSION_TEXTURECUBEARRAY: desc.TextureCubeArray.MipLevels = 1; desc.TextureCubeArray.NumCubes = 1; break;
    default: desc.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D; desc.Texture2D.MipLevels = 1; break;
    }
}

DX12Descriptor InternalDevice12::NullSrv(u32 dimension, u32 returnType, u8 kind)
{
    if (dimension == D3D12_SRV_DIMENSION_UNKNOWN || dimension > D3D12_SRV_DIMENSION_TEXTURECUBEARRAY)
    {
        dimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    }
    const u32 key = dimension | (returnType << 8) | (u32(kind) << 16);
    for (const auto& cached : _nullSrvs)
    {
        if (cached.Key == key)
        {
            return cached.Descriptor;
        }
    }
    NullDescriptor created;
    created.Key = key;
    created.Descriptor = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    D3D12_SHADER_RESOURCE_VIEW_DESC desc = {};
    desc.Format = NullFormat(returnType);
    desc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    desc.ViewDimension = (D3D12_SRV_DIMENSION)dimension;
    if (desc.ViewDimension == D3D12_SRV_DIMENSION_BUFFER)
    {
        desc.Buffer.NumElements = 1;
        if (kind == DX12KindRaw)
        {
            desc.Format = DXGI_FORMAT_R32_TYPELESS;
            desc.Buffer.Flags = D3D12_BUFFER_SRV_FLAG_RAW;
        }
        else if (kind == DX12KindStructured)
        {
            desc.Format = DXGI_FORMAT_UNKNOWN;
            desc.Buffer.StructureByteStride = 16;
        }
        else
        {
            desc.Format = returnType == D3D_RETURN_TYPE_UINT ? DXGI_FORMAT_R32_UINT :
                returnType == D3D_RETURN_TYPE_SINT ? DXGI_FORMAT_R32_SINT : DXGI_FORMAT_R32_FLOAT;
        }
    }
    else
    {
        NullSrvShape(desc);
    }
    GetDevice()->CreateShaderResourceView(nullptr, &desc, created.Descriptor.Cpu);
    _counters.SrvCreates.fetch_add(1, std::memory_order_relaxed);
    _nullSrvs.push_back(created);
    return created.Descriptor;
}

DX12Descriptor InternalDevice12::NullUav(u32 dimension, u8 kind)
{
    if (dimension > D3D12_UAV_DIMENSION_TEXTURE3D)
    {
        dimension = D3D12_UAV_DIMENSION_TEXTURE2D;
    }
    const u32 key = dimension | (u32(kind) << 8);
    for (const auto& cached : _nullUavs)
    {
        if (cached.Key == key)
        {
            return cached.Descriptor;
        }
    }
    NullDescriptor created;
    created.Key = key;
    created.Descriptor = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    D3D12_UNORDERED_ACCESS_VIEW_DESC desc = {};
    desc.ViewDimension = D3D12_UAV_DIMENSION(dimension);
    desc.Format = DXGI_FORMAT_R32G32B32A32_FLOAT;
    switch (desc.ViewDimension)
    {
    case D3D12_UAV_DIMENSION_BUFFER:
        desc.Buffer.NumElements = 1;
        if (kind == DX12KindRaw)
        {
            desc.Format = DXGI_FORMAT_R32_TYPELESS;
            desc.Buffer.Flags = D3D12_BUFFER_UAV_FLAG_RAW;
        }
        else if (kind == DX12KindStructured)
        {
            desc.Format = DXGI_FORMAT_UNKNOWN;
            desc.Buffer.StructureByteStride = 16;
        }
        else
        {
            desc.Format = DXGI_FORMAT_R32_FLOAT;
        }
        break;
    case D3D12_UAV_DIMENSION_TEXTURE1D: break;
    case D3D12_UAV_DIMENSION_TEXTURE1DARRAY: desc.Texture1DArray.ArraySize = 1; break;
    case D3D12_UAV_DIMENSION_TEXTURE2DARRAY: desc.Texture2DArray.ArraySize = 1; break;
    case D3D12_UAV_DIMENSION_TEXTURE3D: desc.Texture3D.WSize = 1; break;
    default: desc.ViewDimension = D3D12_UAV_DIMENSION_TEXTURE2D; break;
    }
    GetDevice()->CreateUnorderedAccessView(nullptr, nullptr, &desc, created.Descriptor.Cpu);
    _counters.UavCreates.fetch_add(1, std::memory_order_relaxed);
    _nullUavs.push_back(created);
    return created.Descriptor;
}

void InternalDevice12::DumpCounters()
{
    const auto us = [&](u64 ticks) { return _tickFrequency ? ticks * 1000000 / _tickFrequency : 0; };
    const auto& frame = _frames[_frame];
    Msg("* D3D12 desc-copy %llu cbv %llu srv %llu samp %llu uav %llu table %llu/%llu root %llu barrier %llu upload %llu copied %llu alloc %llu discard %llu pso %llu/%llu flush %llu heap %u/%u mutex %llu/%llu us upload-lock %llu/%llu us",
        _counters.DescriptorCopies.load(std::memory_order_relaxed),
        _counters.CbvCreates.load(std::memory_order_relaxed),
        _counters.SrvCreates.load(std::memory_order_relaxed),
        _counters.SamplerCreates.load(std::memory_order_relaxed),
        _counters.UavCreates.load(std::memory_order_relaxed),
        _counters.TableHits.load(std::memory_order_relaxed),
        _counters.TableMisses.load(std::memory_order_relaxed),
        _counters.RootBinds.load(std::memory_order_relaxed),
        _counters.Barriers.load(std::memory_order_relaxed),
        _counters.UploadBytes.load(std::memory_order_relaxed),
        _counters.CopiedBytes.load(std::memory_order_relaxed),
        _counters.Allocations.load(std::memory_order_relaxed),
        _counters.Discards.load(std::memory_order_relaxed),
        _counters.PipelineHits.load(std::memory_order_relaxed),
        _counters.PipelineMisses.load(std::memory_order_relaxed),
        _counters.DescriptorFlushes.load(std::memory_order_relaxed),
        frame.ResourcesUsed, frame.SamplersUsed,
        us(_counters.MutexWait.load(std::memory_order_relaxed)),
        us(_counters.MutexHold.load(std::memory_order_relaxed)),
        us(_counters.UploadWait.load(std::memory_order_relaxed)),
        us(_counters.UploadHold.load(std::memory_order_relaxed)));
}

void InternalDevice12::UpdateBuffers()
{
    RHITextureDesc backDesc(psCurrentVidMode[0], psCurrentVidMode[1], ERHI_FORMAT::B8G8R8A8_UNORM);
    backDesc.BindFlags = ERHI_BIND_FLAG::RENDER_TARGET;
    for (u32 buffer_idx = 0; buffer_idx < FrameCount; ++buffer_idx)
    {
        ID3D12Resource* resource = nullptr;
        R_CHK(_swapchain->GetBuffer(buffer_idx, IID_PPV_ARGS(&resource)));
        _backbuffers[buffer_idx] = new DX12Surface(*this, backDesc, D3D12_RESOURCE_DIMENSION_TEXTURE2D, resource, D3D12_RESOURCE_STATE_PRESENT);
        _backbufferViews[buffer_idx] = static_cast<DX12RenderTargetView*>(_textureFactory->CreateRenderTargetView(_backbuffers[buffer_idx]));
    }
    SwapChainRTV = _backbufferViews[_frame];
    RHITextureDesc renderDesc = backDesc;
    renderDesc.Format = ERHI_FORMAT::B8G8R8X8_UNORM;
    renderDesc.BindFlags = (ERHI_BIND_FLAG)(ERHI_BIND_FLAG::RENDER_TARGET | ERHI_BIND_FLAG::SHADER_RESOURCE);
    _renderSurface = static_cast<DX12Surface*>(_textureFactory->CreateRenderTarget(renderDesc));
    RenderRTV = _textureFactory->CreateRenderTargetView(_renderSurface);
    _renderView = static_cast<DX12ShaderResourceView*>(_textureFactory->CreateShaderResourceView(_renderSurface, nullptr));
    RenderSRV = _renderView->GetRawSRV();
    RenderTexture = reinterpret_cast<IRHIRenderTargetView*>(_renderSurface->GetRawTexture());
    RHITextureDesc depthDesc = backDesc;
    depthDesc.Width = u32(depthDesc.Width * RenderScale);
    depthDesc.Height = u32(depthDesc.Height * RenderScale);
    depthDesc.Width += depthDesc.Width % 2;
    depthDesc.Height += depthDesc.Height % 2;
    depthDesc.Format = ERHI_FORMAT::D24_UNORM_S8_UINT;
    depthDesc.BindFlags = ERHI_BIND_FLAG::DEPTH_STENCIL;
    HalfTarget.set((int)depthDesc.Width, (int)depthDesc.Height);
    _depthSurface = static_cast<DX12Surface*>(_textureFactory->CreateDepthStencil(depthDesc));
    RenderDSV = _textureFactory->CreateDepthStencilView(_depthSurface);
}

void InternalDevice12::ReleaseBuffers()
{
    for (u32 buffer_idx = 0; buffer_idx < FrameCount; ++buffer_idx)
    {
        if (_backbufferViews[buffer_idx])
        {
            _backbufferViews[buffer_idx]->Release();
            _backbufferViews[buffer_idx] = nullptr;
        }
        if (_backbuffers[buffer_idx])
        {
            _backbuffers[buffer_idx]->Release();
            _backbuffers[buffer_idx] = nullptr;
        }
    }
    if (RenderRTV)
    {
        RenderRTV->Release();
        RenderRTV = nullptr;
    }
    if (_renderView)
    {
        _renderView->Release();
        _renderView = nullptr;
    }
    if (RenderDSV)
    {
        RenderDSV->Release();
        RenderDSV = nullptr;
    }
    if (_renderSurface)
    {
        _renderSurface->Release();
        _renderSurface = nullptr;
    }
    if (_depthSurface)
    {
        _depthSurface->Release();
        _depthSurface = nullptr;
    }
    SwapChainRTV = nullptr;
    RenderSRV = nullptr;
    RenderTexture = nullptr;
    ZeroMemory(_targets, sizeof(_targets));
    _depth = nullptr;
    _pipelineDirty = true;
    MarkViewsDirty();
}

void InternalDevice12::ResizeBuffers(u32 width, u32 height)
{
    ContextLock guard(*this);
    if (!width || !height)
    {
        return;
    }
    Flush();
    for (auto& frame : _frames)
    {
        Wait(frame.Fence);
    }
    ReleaseBuffers();
    CollectRetired();
    R_CHK(_swapchain->ResizeBuffers(FrameCount, width, height, DXGI_FORMAT_B8G8R8A8_UNORM, 0));
    psCurrentVidMode[0] = width;
    psCurrentVidMode[1] = height;
    _frame = _swapchain->GetCurrentBackBufferIndex();
    UpdateBuffers();
}

void InternalDevice12::Present()
{
    PROF_EVENT("D3D12: Present");
    ContextLock guard(*this);
    Transition(_backbuffers[_frame]->GetResource(), D3D12_RESOURCE_STATE_PRESENT);
    Submit();
#if defined(IXRAY_PROFILER)
    if (_gpuContextLive)
    {
        Optick::SetGpuContext(Optick::GPUContext(_prevGpuCommand, (Optick::GPUQueueType)_prevGpuQueue, _prevGpuNode));
        _gpuContextLive = false;
    }
    Optick::GpuFlip(_swapchain);
#endif
    R_CHK(_swapchain->Present(psDeviceFlags.test(rsVSync) ? 1 : 0, 0));
    const u32 next = _swapchain->GetCurrentBackBufferIndex();
    Frame& frame = _frames[next];
    Wait(frame.Fence);
    _uploadLock.Enter();
    for (auto& chunk : frame.Uploads)
    {
        chunk.Used = 0;
    }
    ++frame.UploadSerial;
    _frame = next;
    _uploadLock.Leave();
    if ((++_presentCount & 255) == 0)
    {
        DumpCounters();
    }
    frame.ResourcesUsed = 0;
    frame.SamplersUsed = 0;
    frame.Tables.clear();
    std::fill(std::begin(frame.TableBuckets), std::end(frame.TableBuckets), 0);
    SwapChainRTV = _backbufferViews[_frame];
}

IRHITextureFactory* InternalDevice12::GetTextureFactory()
{
    return _textureFactory;
}

void InternalDevice12::SetTextureFactory(IRHITextureFactory* factory)
{
    R_ASSERT(factory == _textureFactory);
}

IRHIBuffer* InternalDevice12::CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* data)
{
    return new DX12Buffer(*this, desc, data);
}

IRHISurface* InternalDevice12::CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data)
{
    return CreateTexture(desc, D3D12_RESOURCE_DIMENSION_TEXTURE1D, data.Data ? &data : nullptr, data.Data ? 1 : 0);
}

void InternalDevice12::ClearTarget(void* target, ERTColor color)
{
    ContextLock guard(*this);
    const float colors[][4] = { { 0, 0, 0, 0 }, { 0.5f, 0.5f, 0.5f, 0.5f }, { 1, 1, 1, 1 } };
    R_ASSERT((u32)color < std::size(colors));
    ClearTarget(target, colors[(u32)color]);
}

void InternalDevice12::ClearTarget(void* target, const float* color)
{
    ContextLock guard(*this);
    auto& view = *static_cast<DX12View*>(target);
    TransitionView(*view.Surface, D3D12_RESOURCE_STATE_RENDER_TARGET, view);
    Commands()->ClearRenderTargetView(view.Descriptor.Cpu, color, 0, nullptr);
}

void InternalDevice12::ClearDepthStencil(IRHIDepthStencilView* target, ERHI_CLEAR_TARGET flags, float depth, u8 stencil)
{
    ContextLock guard(*this);
    if (!target)
    {
        return;
    }
    auto& view = static_cast<DX12DepthStencilView*>(target)->View;
    TransitionView(*view.Surface, D3D12_RESOURCE_STATE_DEPTH_WRITE, view);
    Commands()->ClearDepthStencilView(view.Descriptor.Cpu, (D3D12_CLEAR_FLAGS)flags, depth, stencil, 0, nullptr);
}

void InternalDevice12::SetViewport(RHIViewport& viewport)
{
    ContextLock guard(*this);
    _viewport = viewport;
}

void InternalDevice12::SetScissorRect(Irect* rect)
{
    ContextLock guard(*this);
    _hasScissor = rect != nullptr;
    if (rect)
    {
        _scissor = *rect;
    }
}

void InternalDevice12::SetRenderTargets(u32 count, IRHIRenderTargetView* const* targets, IRHIUnorderedAccessView* const* uavs)
{
    ContextLock guard(*this);
    R_ASSERT(count <= 8);
    for (u32 target_idx = 0; target_idx < 8; ++target_idx)
    {
        _targets[target_idx] = target_idx < count ? static_cast<DX12RenderTargetView*>(targets[target_idx]) : nullptr;
        _renderUAVs[target_idx] = uavs && target_idx < count ? static_cast<DX12UnorderedAccessView*>(uavs[target_idx]) : nullptr;
    }
    _pipelineDirty = true;
    MarkViewsDirty();
}

void InternalDevice12::SetDSV(IRHIDepthStencilView* view)
{
    ContextLock guard(*this);
    _depth = static_cast<DX12DepthStencilView*>(view);
    _pipelineDirty = true;
    MarkViewsDirty();
}

void InternalDevice12::CopySurface(IRHISurface* destination, IRHISurface* source)
{
    ContextLock guard(*this);
    auto& destinationResource = static_cast<DX12Surface*>(destination)->GetResource();
    auto& sourceResource = static_cast<DX12Surface*>(source)->GetResource();
    if (source->GetSampleDescCount() > 1 && destination->GetSampleDescCount() == 1)
    {
        R_ASSERT(source->GetMipLevels() == 1 && destination->GetMipLevels() == 1);
        Transition(destinationResource, D3D12_RESOURCE_STATE_RESOLVE_DEST);
        Transition(sourceResource, D3D12_RESOURCE_STATE_RESOLVE_SOURCE);
        for (u32 layer_idx = 0; layer_idx < source->GetArraySize(); ++layer_idx)
        {
            Commands()->ResolveSubresource(destinationResource.Native, layer_idx, sourceResource.Native, layer_idx,
                (DXGI_FORMAT)destination->GetFormat());
        }
        return;
    }
    Transition(destinationResource, D3D12_RESOURCE_STATE_COPY_DEST);
    Transition(sourceResource, D3D12_RESOURCE_STATE_COPY_SOURCE);
    Commands()->CopyResource(destinationResource.Native, sourceResource.Native);
}

void InternalDevice12::CopySurface(IRHIRenderTargetView* destination, IRHIRenderTargetView* source)
{
    ContextLock guard(*this);
    CopySurface(destination->GetSurface(), source->GetSurface());
}

void InternalDevice12::CopySwapchain(IRHISurface* destination)
{
    ContextLock guard(*this);
    CopySurface(destination, _backbuffers[_frame]);
}

bool InternalDevice12::SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags)
{
    D3D12_FEATURE_DATA_FORMAT_SUPPORT support = { (DXGI_FORMAT)format };
    const HRESULT result = GetDevice()->CheckFeatureSupport(D3D12_FEATURE_FORMAT_SUPPORT, &support, sizeof(support));
    out_flags = (u32)support.Support1;
    const u32 required = D3D12_FORMAT_SUPPORT1_SHADER_LOAD | D3D12_FORMAT_SUPPORT1_SHADER_SAMPLE;
    return SUCCEEDED(result) && (out_flags & required) == required;
}

void InternalDevice12::SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology)
{
    ContextLock guard(*this);
    if (_topology != topology)
    {
        _topology = topology;
        _pipelineDirty = true;
    }
}

void InternalDevice12::Draw(u32 startVertex, u32 primitiveCount)
{
    ContextLock guard(*this);
    if (!primitiveCount)
    {
        return;
    }
    if (!PrepareDraw(false))
    {
        return;
    }
    DX12_GPU_EVENT(_commands, "D3D12: Draw");
    _commands->DrawInstanced(RHITopologyUtils::GetVertexCount(primitiveCount, _topology), 1, startVertex, 0);
    FinishDraw();
}

void InternalDevice12::DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount)
{
    ContextLock guard(*this);
    DrawIndexedInstanced(baseVertex, startVertex, vertexCount, startIndex, primitiveCount, 1, 0);
}

void InternalDevice12::DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation)
{
    ContextLock guard(*this);
    if (!primitiveCount || !instanceCount)
    {
        return;
    }
    if (!PrepareDraw(false))
    {
        return;
    }
    DX12_GPU_EVENT(_commands, "D3D12: DrawIndexed");
    _commands->DrawIndexedInstanced(RHITopologyUtils::GetIndexCount(primitiveCount, _topology), instanceCount, startIndex, (INT)baseVertex, startInstanceLocation);
    FinishDraw();
}

void InternalDevice12::DrawNoInputAssembly(u32 vertexCount)
{
    ContextLock guard(*this);
    if (!vertexCount)
    {
        return;
    }
    if (!PrepareDraw(false))
    {
        return;
    }
    DX12_GPU_EVENT(_commands, "D3D12: DrawNoInputAssembly");
    _commands->DrawInstanced(vertexCount, 1, 0, 0);
    FinishDraw();
}

void InternalDevice12::Dispatch(u32 x, u32 y, u32 z)
{
    PROF_EVENT("D3D12: Dispatch");
    ContextLock guard(*this);
    if (!PrepareDraw(true))
    {
        return;
    }
    DX12_GPU_EVENT(_commands, "D3D12: Dispatch");
    _commands->Dispatch(x, y, z);
    // UAV barriers are deferred: the next UAV binding of the resource in this list emits one (UAVTable),
    // and a state transition away from UNORDERED_ACCESS orders the writes by itself.
    for (u32 view_idx = 0; view_idx < 8; ++view_idx)
    {
        auto view = _computeUAVs[view_idx];
        if (view && _computeShader && (_computeShader->UAVMask & (1u << view_idx)))
        {
            auto& resource = view->View.Surface ? view->View.Surface->GetResource() : view->View.Buffer->GetResource();
            resource.UAVPendingEpoch = _epoch;
        }
    }
}

void InternalDevice12::FinishDraw()
{
    auto shader = _graphicsState.Shaders[0];
    if (!shader || !shader->UAVMask)
    {
        return;
    }
    for (u32 view_idx = 0; view_idx < 8; ++view_idx)
    {
        auto view = _renderUAVs[view_idx];
        if (view && (shader->UAVMask & (1u << view_idx)))
        {
            auto& resource = view->View.Surface ? view->View.Surface->GetResource() : view->View.Buffer->GetResource();
            resource.UAVPendingEpoch = _epoch;
        }
    }
}
