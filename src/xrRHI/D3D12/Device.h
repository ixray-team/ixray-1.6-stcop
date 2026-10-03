#pragma once
#include "Resources.h"
#include <atomic>
#include <d3dcommon.h>

struct ID3D12InfoQueue1;

struct DX12Shader
{
    u64 Id = 0;
    u32 ConstantMask = 0;
    u32 SamplerMask = 0;
    u32 UAVMask = 0;
    D3D12_SRV_DIMENSION Dimensions[16] = {};
    D3D12_UAV_DIMENSION UAVDimensions[8] = {};
    D3D_RESOURCE_RETURN_TYPE ReturnTypes[16] = {};
    u8 ResourceKinds[16] = {};
    u8 UAVKinds[8] = {};
    D3D12_CPU_DESCRIPTOR_HANDLE NullSrvs[16] = {};
    bool NullReady = false;
    xr_vector<u8> Code;
};

struct DX12InputLayout
{
    u64 Id = 0;
    xr_vector<D3D12_INPUT_ELEMENT_DESC> Elements;
    xr_vector<shared_str> Semantics;
};

struct DX12Sampler
{
    InternalDevice12* Device = nullptr;
    DX12Descriptor Descriptor;
    RHISampleDesc Desc = {};
};

struct DX12Query
{
    InternalDevice12* Device = nullptr;
    u32 Slot = 0;
    u64 Fence = 0;
    bool IsPending = false;
};

class InternalDevice12 final : public IRHIDevice
{
public:
    InternalDevice12();
    ~InternalDevice12() override;
    void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology) override;
    void DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount) override;
    void Draw(u32 startVertex, u32 primitiveCount) override;
    void DrawIndexedInstanced(
		u32 baseVertex, u32 startVertex, u32 vertexCount,
		u32 startIndex, u32 primitiveCount,
		u32 instanceCount, u32 startInstanceLocation) override;
    void DrawNoInputAssembly(u32 vertexCount) override;
    void ResizeBuffers(u32 Width, u32 Height) override;
    void ClearTarget(void* Target, ERTColor Transparent) override;
    void ClearTarget(void* Target, const float* Color) override;
    void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) override;
    void GenerateMips(IRHIShaderResourceView* SRV) override;
    void Present() override;
    void CopySurface(IRHISurface* Dest, IRHISurface* Source) override;
    void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) override;
    IRHITextureFactory* GetTextureFactory() override;
    void SetTextureFactory(IRHITextureFactory* factory) override;
    void SetViewport(RHIViewport& VP) override;
    IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr) override;
    void SetScissorRect(Irect* R) override;
    bool ReadRenderTargetPixels(IRHIRenderTargetView* Rtv, void* Dst, u32 DstSize, u32& OutWidth, u32& OutHeight, u32& OutRowPitch) override;
    void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIUnorderedAccessView* const* ppRenderUAViews) override;
    void SetDSV(IRHIDepthStencilView* pDepthStencilView) override;
    void* GetContext() override;
    void* GetSwapchain() override;
    void BeginFrame() override;
    IRHIShaderDeclaration* CreateDecl(const RHIInputElementDesc* Desc, size_t DeclSize) override;
    IRHIShaderResourceView* CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc) override;
    void SetConstantBuffers(u32 Start, u32 Count, IRHIBuffer* const* Buffers, ERHI_SHADER_TYPE Type) override;
    void ClearVertexBuffer(u32 vb_stride) override;
    void ClearIndexBuffer() override;
    void SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type) override;
    HRESULT LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bind_flags, ERHI_CPU_ACCESS_FLAG cpu_flags, int& lod, bool fallback, IRHISurface** out_surface) override;
    HRESULT CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader) override;
    HRESULT ReplaceShader(RHIObject* shader, const void* code, size_t size) override;
    void Flush() override;
    HRESULT CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout) override;
    void SetInputLayout(RHIObject* layout) override;
    void Dispatch(u32 x, u32 y, u32 z) override;
    HRESULT CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state) override;
    void SetSamplers(u32 start, u32 count, RHIObject* const* states, ERHI_SHADER_TYPE type) override;
    void SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views) override;
    void SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts) override;
    HRESULT CreateOcclusionQuery(RHIObject** out_query) override;
    HRESULT GetQueryData(RHIObject* query, void* data, u32 size, u32 flags) override;
    void BeginQuery(RHIObject* query) override;
    void EndQuery(RHIObject* query) override;
    IRHISurface* CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data) override;
    void CopySwapchain(IRHISurface* dest) override;
    bool SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags) override;
    void* GetState(const RHIRasterizerDesc& desc) override;
    void* GetState(const RHIDepthStencilDesc& desc) override;
    void* GetState(const RHIBlendDesc& desc) override;
    HRESULT CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state) override;
    void SetBlendState(RHIObject* state, const float* factor, u32 mask) override;
    void SetRawBlendState(void* state, const float* factor, u32 mask) override;
    ID3D12Device* GetDevice() const { return static_cast<ID3D12Device*>(RawDevice); }
    ID3D12CommandQueue* GetQueue() const { return _queue; }
    xrCriticalSection& ContextMutex() const { return _contextMutex; }
    ID3D12GraphicsCommandList* Commands();
    ID3D12DescriptorHeap* GetResourceHeap() const { return _resourceHeap; }
    DX12Descriptor AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE type);
    D3D12_CPU_DESCRIPTOR_HANDLE VisibleCPU(const DX12Descriptor& descriptor) const;
    void PublishDescriptor(const DX12Descriptor& descriptor);
    void Retire(IUnknown* object);
    void Retire(const DX12Descriptor& descriptor);
    void Transition(DX12Resource& resource, D3D12_RESOURCE_STATES state, u32 subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES);
    void UAVBarrier(ID3D12Resource* resource);
    DX12Upload AllocateUpload(u64 size, u64 alignment = 256);
    u64 GetEpoch();
    u64 PendingFence() const { return _nextFence; }
    bool IsComplete(u64 fence) const;
    void InvalidateBindings();
    void RecycleQuery(DX12Query& query);
    void DrainUploads();
    void TransitionView(DX12Surface& surface, D3D12_RESOURCE_STATES state, const DX12View& view);
    class ContextLock
    {
    public:
        explicit ContextLock(const InternalDevice12& device);
        ~ContextLock();
        ContextLock(const ContextLock&) = delete;
        ContextLock& operator=(const ContextLock&) = delete;

    private:
        const InternalDevice12* _device;
    };
    DX12Surface* CreateTexture(const RHITextureDesc& desc, D3D12_RESOURCE_DIMENSION dimension, const RHISubResource* data, u32 count);
    bool UploadTexture(DX12Surface& surface, u32 subresource, const RHISubResource& data, const RHIBox* box = nullptr);
    bool ReadTexture(DX12Surface& surface, u32 subresource, ID3D12Resource** out_readback, D3D12_PLACED_SUBRESOURCE_FOOTPRINT& out_footprint);
    ID3D12Resource* CreateNativeBuffer(u64 size, D3D12_HEAP_TYPE heap, D3D12_RESOURCE_FLAGS flags = D3D12_RESOURCE_FLAG_NONE);
    void BindVertexBuffer(DX12Buffer* buffer, u32 slot, u32 stride, u32 offset);
    void BindIndexBuffer(DX12Buffer* buffer, bool is32Bit, u32 offset);
    void BindResource(u32 stage, u32 slot, IRHIShaderResourceView* resource);
    IRHIStateManager* CreateStateManager();
    IRHIShaderResourceStateCache* CreateResourceCache();
    bool SetDepthBounds(bool enable, float minimum, float maximum) override;
    void RegisterSurface(DX12Surface* surface);
    void UnregisterSurface(DX12Surface* surface);
    DX12Surface* FindSurface(ID3D12Resource* resource);
    void RegisterImage(DX12ShaderResourceView* view);
    void UnregisterImage(DX12ShaderResourceView* view);
    void PrepareImage(u64 texture);
    void* GetImageHandle(u32 index);
    u64 ResolveImageHandle(u64 texture) const;
    void PrepareImGuiTarget();
    void PrepareUpscale(IRHISurface* surface, bool output);
    void FinishExternalWork();
    void BeginMarker(const char* name);
    void EndMarker();
    void BeginGPUStats();
    void EndGPUStats();
    const RHI_GPU_EVENT& GPUStats();
    int PushGPUEvent(const char* name);
    void PopGPUEvent(int index);

private:
    mutable xrCriticalSection _contextMutex;
    static constexpr u32 FrameCount = 3;
    static constexpr u32 RootConstantSlots = 4;
    static constexpr u32 TableConstantCount = 14 - RootConstantSlots;
    static constexpr u32 TablesPerStage = 3;
    static constexpr u32 GraphicsTableCount = 5 * TablesPerStage + 1;
    static constexpr u32 ComputeTableCount = TablesPerStage + 1;
    static_assert(GraphicsTableCount + 5 * RootConstantSlots * 2 <= 64, "graphics root signature exceeds 64 DWORDs");
    static_assert(ComputeTableCount + RootConstantSlots * 2 <= 64, "compute root signature exceeds 64 DWORDs");
    static constexpr u32 ResourceDescriptorCount = 1000000;
    static constexpr u32 StaticResourceDescriptors = 65536;
    static constexpr u32 SamplerDescriptorCount = 2048;
    static constexpr u32 StaticSamplerDescriptors = 512;
    struct UploadChunk
    {
        ID3D12Resource* Resource = nullptr;
        u8* Data = nullptr;
        u64 Size = 0;
        u64 Used = 0;
    };
    struct PendingCopy
    {
        DX12Surface* Surface = nullptr;
        DX12Buffer* Buffer = nullptr;
        u32 Subresource = 0;
        u32 DestX = 0;
        u32 DestY = 0;
        u32 DestZ = 0;
        u64 DestOffset = 0;
        u64 CopySize = 0;
        DX12Upload Source;
        D3D12_PLACED_SUBRESOURCE_FOOTPRINT Footprint = {};
        D3D12_BOX Box = {};
        bool HasBox = false;
    };
    struct NullDescriptor
    {
        u32 Key = 0;
        DX12Descriptor Descriptor;
    };
    struct GraphicsPipelineKey
    {
        u64 ShaderIds[5] = {};
        u64 LayoutId = 0;
        D3D12_RASTERIZER_DESC Rasterizer = {};
        D3D12_DEPTH_STENCIL_DESC Depth = {};
        D3D12_BLEND_DESC Blend = {};
        DXGI_FORMAT Formats[8] = {};
        DXGI_FORMAT DepthFormat = DXGI_FORMAT_UNKNOWN;
        u32 TargetCount = 0;
        u32 Samples = 1;
        u32 SampleMask = UINT32_MAX;
        u32 Topology = 0;
        u32 DepthBounds = 0;
    };
    struct Counters
    {
        std::atomic<u64> DescriptorCopies{ 0 };
        std::atomic<u64> CbvCreates{ 0 };
        std::atomic<u64> SrvCreates{ 0 };
        std::atomic<u64> SamplerCreates{ 0 };
        std::atomic<u64> UavCreates{ 0 };
        std::atomic<u64> RootBinds{ 0 };
        std::atomic<u64> Barriers{ 0 };
        std::atomic<u64> UploadBytes{ 0 };
        std::atomic<u64> CopiedBytes{ 0 };
        std::atomic<u64> Allocations{ 0 };
        std::atomic<u64> Discards{ 0 };
        std::atomic<u64> TableHits{ 0 };
        std::atomic<u64> TableMisses{ 0 };
        std::atomic<u64> PipelineHits{ 0 };
        std::atomic<u64> PipelineMisses{ 0 };
        std::atomic<u64> DescriptorFlushes{ 0 };
        std::atomic<u64> MutexWait{ 0 };
        std::atomic<u64> MutexHold{ 0 };
        std::atomic<u64> UploadWait{ 0 };
        std::atomic<u64> UploadHold{ 0 };
    };
    struct DescriptorTable
    {
        u64 Keys[32] = {};
        u32 Count = 0;
        u32 Next = 0;
        bool IsSampler = false;
        D3D12_GPU_DESCRIPTOR_HANDLE Handle = {};
    };
    struct ConstantCache
    {
        u64 Keys[TableConstantCount] = {};
        D3D12_GPU_DESCRIPTOR_HANDLE Handle = {};
        u64 Generation = 0;
    };
    struct ViewCache
    {
        u64 Keys[16] = {};
        D3D12_GPU_DESCRIPTOR_HANDLE Handle = {};
        u64 ShaderId = 0;
        u64 Generation = 0;
        u64 Epoch = 0;
    };
    struct SamplerCache
    {
        u64 Keys[16] = {};
        D3D12_GPU_DESCRIPTOR_HANDLE Handle = {};
        u64 ShaderId = 0;
        u64 Generation = 0;
    };
    struct UAVCache
    {
        D3D12_GPU_DESCRIPTOR_HANDLE Handle = {};
        u64 ShaderId = 0;
        u64 Generation = 0;
    };
    struct Frame
    {
        ID3D12CommandAllocator* Allocator = nullptr;
        u64 Fence = 0;
        u32 ResourcesUsed = 0;
        u32 SamplersUsed = 0;
        u64 UploadSerial = 1;
        xr_vector<UploadChunk> Uploads;
        xr_vector<DescriptorTable> Tables;
        u32 TableBuckets[4096] = {};
        ID3D12QueryHeap* Timestamps = nullptr;
        ID3D12Resource* TimestampReadback = nullptr;
        RHI_GPU_EVENT Events = {};
        u64 StatsFence = 0;
        u64 StatsSerial = 0;
    };
    struct PooledStream
    {
        ID3D12Resource* Resource;
        u8* Data;
        u64 Size;
        u64 Fence;
    };
    struct RetiredObject
    {
        u64 Fence;
        IUnknown* Object;
    };
    struct RetiredDescriptor
    {
        u64 Fence;
        DX12Descriptor Descriptor;
    };
    struct VertexBinding
    {
        DX12Buffer* Buffer = nullptr;
        u32 Stride = 0;
        u32 Offset = 0;
    };
    struct DrawBindings
    {
        D3D12_VIEWPORT Viewport = {};
        D3D12_RECT Scissor = {};
        D3D12_CPU_DESCRIPTOR_HANDLE Targets[8] = {};
        D3D12_CPU_DESCRIPTOR_HANDLE Depth = {};
        D3D12_VERTEX_BUFFER_VIEW Vertices[32] = {};
        D3D12_INDEX_BUFFER_VIEW Index = {};
        float BlendFactor[4] = {};
        u32 VertexCount = 0;
        u32 TargetCount = 0;
        u32 StencilRef = 0;
        D3D_PRIMITIVE_TOPOLOGY Topology = D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;
    };
    struct GraphicsState
    {
        DX12Shader* Shaders[5] = {};
        u64 ShaderIds[5] = {};
        u64 LayoutId = 0;
        DX12InputLayout* Layout = nullptr;
        D3D12_RASTERIZER_DESC Rasterizer = {};
        D3D12_DEPTH_STENCIL_DESC Depth = {};
        D3D12_BLEND_DESC Blend = {};
        DXGI_FORMAT Formats[8] = {};
        DXGI_FORMAT DepthFormat = DXGI_FORMAT_UNKNOWN;
        u32 TargetCount = 0;
        u32 Samples = 1;
        u32 SampleMask = UINT32_MAX;
        D3D12_PRIMITIVE_TOPOLOGY_TYPE Topology = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        bool DepthBounds = false;
    };
    struct GraphicsPipeline
    {
        GraphicsPipelineKey Key;
        u32 Next = 0;
        ID3D12PipelineState* Pipeline = nullptr;
    };
    struct MipPipeline
    {
        DXGI_FORMAT Format;
        D3D12_RESOURCE_DIMENSION Dimension;
        ID3D12PipelineState* Pipeline;
    };
    struct ComputePipeline
    {
        DX12Shader* Shader;
        u64 ShaderId;
        ID3D12PipelineState* Pipeline;
    };
    ID3D12InfoQueue1* _debugQueue = nullptr;
    DWORD _debugCallback = 0;
    IDXGIFactory4* _factory = nullptr;
    IDXGISwapChain3* _swapchain = nullptr;
    ID3D12CommandQueue* _queue = nullptr;
    ID3D12GraphicsCommandList* _commands = nullptr;
    ID3D12GraphicsCommandList1* _commands1 = nullptr;
    ID3D12Fence* _fence = nullptr;
    HANDLE _fenceEvent = nullptr;
    ID3D12DescriptorHeap* _resourceHeap = nullptr;
    ID3D12DescriptorHeap* _samplerHeap = nullptr;
    ID3D12DescriptorHeap* _storageHeaps[2] = {};
    ID3D12DescriptorHeap* _rtvHeap = nullptr;
    ID3D12DescriptorHeap* _dsvHeap = nullptr;
    ID3D12RootSignature* _graphicsRoot = nullptr;
    ID3D12RootSignature* _computeRoot = nullptr;
    ID3D12RootSignature* _mipRoot = nullptr;
    DX12Descriptor _nullTarget;
    Frame _frames[FrameCount];
    DX12Surface* _backbuffers[FrameCount] = {};
    DX12RenderTargetView* _backbufferViews[FrameCount] = {};
    DX12Surface* _renderSurface = nullptr;
    DX12ShaderResourceView* _renderView = nullptr;
    DX12Surface* _depthSurface = nullptr;
    DX12TextureFactory* _textureFactory = nullptr;
    u32 _frame = 0;
    u64 _nextFence = 1;
    u64 _epoch = 0;
    u64 _nextObject = 1;
    u64 _nextDescriptor = 1;
    bool _isRecording = false;
    bool _canUseDepthBounds = false;
    bool _statsActive = false;
    int _frameEvent = -1;
    u64 _statsStack = 0;
    u64 _statsSerial = 0;
    u64 _statsFrequency = 0;
    u64 _lastStatsSerial = 0;
    RHI_GPU_EVENT _lastStats = {};
    u32 _descriptorUsed[4] = {};
    u32 _descriptorStride[4] = {};
    xr_vector<u32> _freeDescriptors[4];
    xr_vector<PooledStream> _streamPool;
    xr_vector<RetiredObject> _retiredObjects;
    xr_vector<RetiredDescriptor> _retiredDescriptors;
    xr_vector<GraphicsPipeline> _graphicsPipelines;
    xr_vector<ComputePipeline> _computePipelines;
    xr_vector<MipPipeline> _mipPipelines;
    xr_vector<DX12Surface*> _surfaces;
    xr_vector<DX12Buffer*> _buffers;
    xr_vector<const char*> _markers;
    xr_vector<DX12ShaderResourceView*> _images;
    float _depthMinimum = 0;
    float _depthMaximum = 1;
    xr_vector<RHIRasterizerDesc*> _rasterStates;
    xr_vector<RHIDepthStencilDesc*> _depthStates;
    xr_vector<RHIBlendDesc*> _blendStates;
    bool _boundHeaps = false;
    bool _boundGraphicsRoot = false;
    bool _boundComputeRoot = false;
    bool _drawBindingsValid = false;
    bool _draining = false;
    bool _gpuContextLive = false;
    D3D12_GPU_DESCRIPTOR_HANDLE _boundGraphicsTables[GraphicsTableCount] = {};
    D3D12_GPU_DESCRIPTOR_HANDLE _boundComputeTables[ComputeTableCount] = {};
    bool _boundGraphicsPipeline = false;
    bool _pipelineDirty = true;
    bool _viewsDirty[6] = { true, true, true, true, true, true };
    bool _samplersDirty[6] = { true, true, true, true, true, true };
    u32 _vertexMask = 0;
    u64 _tableGeneration = 1;
    u64 _rootConstants[6][RootConstantSlots] = {};
    u64 _nullConstants = 0;
    u64 _nullConstantsEpoch = 0;
    u64 _barrierEpoch = 1;
    u64 _attachmentEpoch = 0;
    ConstantCache _constantCaches[6];
    ViewCache _viewCaches[6];
    DX12Descriptor _nullCbv = {};
    SamplerCache _samplerCaches[6];
    UAVCache _uavCaches[2];
    std::atomic<u32> _pendingCount{ 0 };
    ID3D12PipelineState* _boundPipeline = nullptr;
    u64 _boundPipelineHash = 0;
    GraphicsPipelineKey _boundKey;
    DrawBindings _drawBindings;
    GraphicsState _graphicsState;
    DX12Shader* _computeShader = nullptr;
    DX12InputLayout* _layout = nullptr;
    DX12Sampler* _samplers[6][16] = {};
    DX12Buffer* _constants[6][14] = {};
    DX12ShaderResourceView* _resources[6][16] = {};
    DX12UnorderedAccessView* _computeUAVs[8] = {};
    DX12UnorderedAccessView* _renderUAVs[8] = {};
    DX12RenderTargetView* _targets[8] = {};
    DX12DepthStencilView* _depth = nullptr;
    VertexBinding _vertices[32];
    DX12Buffer* _index = nullptr;
    bool _index32 = false;
    u32 _indexOffset = 0;
    ERHI_PRIMITIVE_TOPOLOGY _topology = ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST;
    u32 _stencilRef = 0;
    float _blendFactor[4] = {};
    RHIViewport _viewport;
    Irect _scissor;
    bool _hasScissor = false;
    bool _scissorEnabled = false;

    void Wait(u64 fence);
    u32 ImageIndex(u64 texture) const;
    u64 Submit();
    void CollectRetired();
    DX12Upload AcquireStream(u64 size);
    void RecycleStream(const DX12Upload& upload);
    void UpdateBuffers();
    void ReleaseBuffers();
    void CreateRootSignatures();
    bool PrepareDraw(bool compute);
    bool PreparePipeline();
    void DropShaderPipelines(u64 shaderId);
    void BindNullConstants(bool compute);
    void BindRootConstants(bool compute);
    void FinishDraw();
    u32 RootConstantParameter(bool compute, u32 stage, u32 slot) const;
    D3D12_GPU_DESCRIPTOR_HANDLE ConstantTable(u32 stage);
    D3D12_GPU_DESCRIPTOR_HANDLE ViewTable(u32 stage);
    D3D12_GPU_DESCRIPTOR_HANDLE SamplerTable(u32 stage);
    D3D12_GPU_DESCRIPTOR_HANDLE UAVTable(bool compute);
    D3D12_GPU_DESCRIPTOR_HANDLE FindTable(const DescriptorTable& table);
    void CacheTable(DescriptorTable& table);
    DX12Descriptor AllocateTable(u32 count, bool sampler);
    void MarkViewsDirty() { std::fill(std::begin(_viewsDirty), std::end(_viewsDirty), true); }
    ID3D12DescriptorHeap* Heap(D3D12_DESCRIPTOR_HEAP_TYPE type) const;
    friend class DX12StateManager;
    friend class DX12Buffer;
    friend class DX12TextureFactory;
    void ReportDeviceRemoval() const;
    void RecordMarker(const char* name);
    void EnqueueCopy(PendingCopy&& copy);
    DX12Descriptor NullSrv(u32 dimension, u32 returnType, u8 kind);
    DX12Descriptor NullUav(u32 dimension, u8 kind);
    DX12Descriptor NullSampler();
    void DumpCounters();
    u64 Tick() const;
    bool StateCovers(D3D12_RESOURCE_STATES before, D3D12_RESOURCE_STATES after) const;
    void EmitBarrier(DX12Resource& resource, D3D12_RESOURCE_STATES after, u32 subresource);
    static constexpr u32 QuerySlots = 256;
    static constexpr u32 BarrierBatch = 32;
    xrCriticalSection _uploadLock;
    mutable Counters _counters;
    u64 _tickFrequency = 0;
    u32 _presentCount = 0;
    void* _prevGpuCommand = nullptr;
    u32 _prevGpuQueue = 0;
    int _prevGpuNode = 0;
    xr_vector<PendingCopy> _pending;
    xr_vector<NullDescriptor> _nullSrvs;
    xr_vector<NullDescriptor> _nullUavs;
    DX12Descriptor _nullSampler;
    u32 _pipelineBuckets[256] = {};
    ID3D12QueryHeap* _occlusionHeap = nullptr;
    ID3D12Resource* _occlusionReadback = nullptr;
    u64 _occlusionFence[QuerySlots] = {};
    u32 _occlusionFree[QuerySlots] = {};
    u32 _occlusionFreeCount = 0;
};
