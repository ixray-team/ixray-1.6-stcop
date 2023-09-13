/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

struct IDXGIFactory2;
struct IDXGIAdapter;
struct ID3D12Device;
struct ID3D12Device5;
struct ID3D12DescriptorHeap;
struct ID3D12CommandSignature;
struct IDXGIOutput;
struct D3D12_CPU_DESCRIPTOR_HANDLE;

typedef size_t DescriptorPointerCPU;
typedef uint64_t DescriptorPointerGPU;
typedef uint16_t HeapIndexType;
typedef uint16_t HeapOffsetType;

namespace nri
{

struct CommandQueueD3D12;

struct DescriptorHandle
{
    HeapIndexType heapIndex;
    HeapOffsetType heapOffset;
};

struct DescriptorHeapDesc
{
    ComPtr<ID3D12DescriptorHeap> descriptorHeap;
    DescriptorPointerCPU descriptorPointerCPU;
    DescriptorPointerGPU descriptorPointerGPU;
    uint32_t descriptorSize;
};

constexpr size_t DESCRIPTOR_HEAP_TYPE_NUM = D3D12_DESCRIPTOR_HEAP_TYPE_NUM_TYPES;
constexpr uint32_t DESCRIPTORS_BATCH_SIZE = 1024;

struct DeviceD3D12 final : public DeviceBase
{
    DeviceD3D12(const Log& log, StdAllocator<uint8_t>& stdAllocator);
    ~DeviceD3D12();

    inline operator ID3D12Device*() const
    { return m_Device.GetInterface(); }

    inline operator ID3D12Device5*() const
    { return m_Device5.GetInterface(); }

    inline bool IsMeshShaderSupported() const
    { return m_IsMeshShaderSupported; }

    inline const CoreInterface& GetCoreInterface() const
    { return m_CoreInterface; }

    inline void FreeDescriptorHandle(D3D12_DESCRIPTOR_HEAP_TYPE type, const DescriptorHandle& descriptorHandle)
    {
        ExclusiveScope lock(m_FreeDescriptorLocks[type]);
        auto& freeDescriptors = m_FreeDescriptors[type];
        freeDescriptors.push_back(descriptorHandle);
    }

    template<typename Implementation, typename Interface, typename ... Args>
    Result CreateImplementation(Interface*& entity, const Args&... args);

    bool GetOutput(Display* display, ComPtr<IDXGIOutput>& output) const;

    Result Create(IDXGIAdapter* dxgiAdapter, const DeviceCreationDesc& deviceCreationDesc);
    Result Create(const DeviceCreationD3D12Desc& deviceCreationDesc);

    Result CreateCpuOnlyVisibleDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type);
    Result GetDescriptorHandle(D3D12_DESCRIPTOR_HEAP_TYPE type, DescriptorHandle& descriptorHandle);
    DescriptorPointerCPU GetDescriptorPointerCPU(const DescriptorHandle& descriptorHandle);
    void GetMemoryInfo(MemoryLocation memoryLocation, const D3D12_RESOURCE_DESC& resourceDesc, MemoryDesc& memoryDesc) const;

    ID3D12CommandSignature* CreateCommandSignature(D3D12_INDIRECT_ARGUMENT_TYPE indirectArgumentType, uint32_t stride);
    ID3D12CommandSignature* GetDrawCommandSignature(uint32_t stride);
    ID3D12CommandSignature* GetDrawIndexedCommandSignature(uint32_t stride);
    ID3D12CommandSignature* GetDispatchCommandSignature() const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_Device, name); }

    inline const DeviceDesc& GetDesc() const
    { return m_DeviceDesc; }

    Result CreateSwapChain(const SwapChainDesc& swapChainDesc, SwapChain*& swapChain);
    void DestroySwapChain(SwapChain& swapChain);
    Result GetDisplays(Display** displays, uint32_t& displayNum);
    Result GetDisplaySize(Display& display, uint16_t& width, uint16_t& height);

    Result GetCommandQueue(CommandQueueType commandQueueType, CommandQueue*& commandQueue);
    Result CreateCommandQueue(CommandQueueType commandQueueType, CommandQueue*& commandQueue);
    Result CreateCommandQueue(void* d3d12commandQueue, CommandQueueD3D12*& commandQueue);
    Result CreateCommandAllocator(const CommandQueue& commandQueue, CommandAllocator*& commandAllocator);
    Result CreateDescriptorPool(const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool);
    Result CreateBuffer(const BufferDesc& bufferDesc, Buffer*& buffer);
    Result CreateTexture(const TextureDesc& textureDesc, Texture*& texture);
    Result CreateDescriptor(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView);
    Result CreateDescriptor(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView);
    Result CreateDescriptor(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView);
    Result CreateDescriptor(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView);
    Result CreateDescriptor(const AccelerationStructure& accelerationStructure, Descriptor*& accelerationStructureView);
    Result CreateDescriptor(const SamplerDesc& samplerDesc, Descriptor*& sampler);
    Result CreatePipelineLayout(const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout);
    Result CreatePipeline(const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline);
    Result CreatePipeline(const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline);
    Result CreatePipeline(const RayTracingPipelineDesc& rayTracingPipelineDesc, Pipeline*& pipeline);
    Result CreateFence(uint64_t initialValue, Fence*& fence);
    Result CreateFrameBuffer(const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer);
    Result CreateQueryPool(const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool);
    Result CreateCommandBuffer(const CommandBufferD3D12Desc& commandBufferDesc, CommandBuffer*& commandBuffer);
    Result CreateBuffer(const BufferD3D12Desc& bufferDesc, Buffer*& buffer);
    Result CreateTexture(const TextureD3D12Desc& textureDesc, Texture*& texture);
    Result CreateMemory(const MemoryD3D12Desc& memoryDesc, Memory*& memory);
    Result CreateAccelerationStructure(const AccelerationStructureD3D12Desc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure);
    void DestroyCommandAllocator(CommandAllocator& commandAllocator);
    void DestroyDescriptorPool(DescriptorPool& descriptorPool);
    void DestroyBuffer(Buffer& buffer);
    void DestroyTexture(Texture& texture);
    void DestroyDescriptor(Descriptor& descriptor);
    void DestroyPipelineLayout(PipelineLayout& pipelineLayout);
    void DestroyPipeline(Pipeline& pipeline);
    void DestroyFence(Fence& queueSemaphore);
    void DestroyFrameBuffer(FrameBuffer& frameBuffer);
    void DestroyQueryPool(QueryPool& queryPool);
    Result AllocateMemory(const MemoryType memoryType, uint64_t size, Memory*& memory);
    Result BindBufferMemory(const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum);
    Result BindTextureMemory(const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum);
    void FreeMemory(Memory& memory);
    FormatSupportBits GetFormatSupport(Format format) const;
    uint32_t CalculateAllocationNumber(const ResourceGroupDesc& resourceGroupDesc) const;
    Result AllocateAndBindMemory(const ResourceGroupDesc& resourceGroupDesc, Memory** allocations);

    Result CreateAccelerationStructure(const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure);
    Result BindAccelerationStructureMemory(const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum);
    void DestroyAccelerationStructure(AccelerationStructure& accelerationStructure);

    //================================================================================================================
    // DeviceBase
    //================================================================================================================

    void Destroy();
    Result FillFunctionTable(CoreInterface& table) const;
    Result FillFunctionTable(SwapChainInterface& table) const;
    Result FillFunctionTable(WrapperD3D12Interface& wrapperD3D12Interface) const;
    Result FillFunctionTable(RayTracingInterface& rayTracingInterface) const;
    Result FillFunctionTable(MeshShaderInterface& meshShaderInterface) const;
    Result FillFunctionTable(HelperInterface& helperInterface) const;

private:
    void UpdateDeviceDesc(bool enableValidation);
    MemoryType GetMemoryType(MemoryLocation memoryLocation, const D3D12_RESOURCE_DESC& resourceDesc) const;

private:
    ComPtr<ID3D12Device> m_Device;
    ComPtr<ID3D12Device5> m_Device5;
    std::array<CommandQueueD3D12*, COMMAND_QUEUE_TYPE_NUM> m_CommandQueues = {};
    Vector<DescriptorHeapDesc> m_DescriptorHeaps;
    Vector<Vector<DescriptorHandle>> m_FreeDescriptors;
    DeviceDesc m_DeviceDesc = {};
    UnorderedMap<uint32_t, ComPtr<ID3D12CommandSignature>> m_DrawCommandSignatures;
    UnorderedMap<uint32_t, ComPtr<ID3D12CommandSignature>> m_DrawIndexedCommandSignatures;
    ComPtr<ID3D12CommandSignature> m_DispatchCommandSignature;
    CoreInterface m_CoreInterface = {};
    ComPtr<IDXGIAdapter> m_Adapter;
    std::array<Lock, DESCRIPTOR_HEAP_TYPE_NUM> m_FreeDescriptorLocks;
    Lock m_DescriptorHeapLock;
    Lock m_QueueLock;
    bool m_IsRaytracingSupported = false;
    bool m_IsMeshShaderSupported = false;
    bool m_SkipLiveObjectsReporting = false;
};

}
