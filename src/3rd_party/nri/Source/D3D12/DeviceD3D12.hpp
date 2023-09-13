/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

Declare_PartiallyFillFunctionTable_Functions(D3D12)

#pragma region [  Core  ]

static const DeviceDesc& NRI_CALL GetDeviceDesc(const Device& device)
{
    return ((const DeviceD3D12&)device).GetDesc();
}

static Result NRI_CALL GetCommandQueue(Device& device, CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    return ((DeviceD3D12&)device).GetCommandQueue(commandQueueType, commandQueue);
}

static Result NRI_CALL CreateCommandAllocator(const CommandQueue& commandQueue, uint32_t physicalDeviceMask, CommandAllocator*& commandAllocator)
{
    MaybeUnused(physicalDeviceMask); // TODO: use it

    DeviceD3D12& device = ((CommandQueueD3D12&)commandQueue).GetDevice();
    return device.CreateCommandAllocator(commandQueue, commandAllocator);
}

static Result NRI_CALL CreateDescriptorPool(Device& device, const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return ((DeviceD3D12&)device).CreateDescriptorPool(descriptorPoolDesc, descriptorPool);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferDesc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceD3D12&)device).CreateBuffer(bufferDesc, buffer);
}

static Result NRI_CALL CreateTexture(Device& device, const TextureDesc& textureDesc, Texture*& texture)
{
    return ((DeviceD3D12&)device).CreateTexture(textureDesc, texture);
}

static Result NRI_CALL CreateBufferView(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    DeviceD3D12& device = ((const BufferD3D12*)bufferViewDesc.buffer)->GetDevice();
    return device.CreateDescriptor(bufferViewDesc, bufferView);
}

static Result NRI_CALL CreateTexture1DView(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceD3D12& device = ((const TextureD3D12*)textureViewDesc.texture)->GetDevice();
    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture2DView(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceD3D12& device = ((const TextureD3D12*)textureViewDesc.texture)->GetDevice();
    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture3DView(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceD3D12& device = ((const TextureD3D12*)textureViewDesc.texture)->GetDevice();
    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateSampler(Device& device, const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return ((DeviceD3D12&)device).CreateDescriptor(samplerDesc, sampler);
}

static Result NRI_CALL CreatePipelineLayout(Device& device, const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    return ((DeviceD3D12&)device).CreatePipelineLayout(pipelineLayoutDesc, pipelineLayout);
}

static Result NRI_CALL CreateGraphicsPipeline(Device& device, const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceD3D12&)device).CreatePipeline(graphicsPipelineDesc, pipeline);
}

static Result NRI_CALL CreateComputePipeline(Device& device, const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceD3D12&)device).CreatePipeline(computePipelineDesc, pipeline);
}

static Result NRI_CALL CreateFence(Device& device, uint64_t initialValue, Fence*& fence)
{
    return ((DeviceD3D12&)device).CreateFence(initialValue, fence);
}

static Result NRI_CALL CreateFrameBuffer(Device& device, const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    return ((DeviceD3D12&)device).CreateFrameBuffer(frameBufferDesc, frameBuffer);
}

static Result NRI_CALL CreateQueryPool(Device& device, const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return ((DeviceD3D12&)device).CreateQueryPool(queryPoolDesc, queryPool);
}

static void NRI_CALL DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    DeviceD3D12& device = ((CommandAllocatorD3D12&)commandAllocator).GetDevice();
    device.DestroyCommandAllocator(commandAllocator);
}

static void NRI_CALL DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    DeviceD3D12& device = ((DescriptorPoolD3D12&)descriptorPool).GetDevice();
    device.DestroyDescriptorPool(descriptorPool);
}

static void NRI_CALL DestroyBuffer(Buffer& buffer)
{
    DeviceD3D12& device = ((BufferD3D12&)buffer).GetDevice();
    device.DestroyBuffer(buffer);
}

static void NRI_CALL DestroyTexture(Texture& texture)
{
    DeviceD3D12& device = ((TextureD3D12&)texture).GetDevice();
    device.DestroyTexture(texture);
}

static void NRI_CALL DestroyDescriptor(Descriptor& descriptor)
{
    DeviceD3D12& device = ((DescriptorD3D12&)descriptor).GetDevice();
    device.DestroyDescriptor(descriptor);
}

static void NRI_CALL DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    DeviceD3D12& device = ((PipelineLayoutD3D12&)pipelineLayout).GetDevice();
    device.DestroyPipelineLayout(pipelineLayout);
}

static void NRI_CALL DestroyPipeline(Pipeline& pipeline)
{
    DeviceD3D12& device = ((PipelineD3D12&)pipeline).GetDevice();
    device.DestroyPipeline(pipeline);
}

static void NRI_CALL DestroyFence(Fence& fence)
{
    DeviceD3D12& device = ((FenceD3D12&)fence).GetDevice();
    device.DestroyFence(fence);
}

static void NRI_CALL DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    DeviceD3D12& device = ((FrameBufferD3D12&)frameBuffer).GetDevice();
    device.DestroyFrameBuffer(frameBuffer);
}

static void NRI_CALL DestroyQueryPool(QueryPool& queryPool)
{
    DeviceD3D12& device = ((QueryPoolD3D12&)queryPool).GetDevice();
    device.DestroyQueryPool(queryPool);
}

static Result NRI_CALL AllocateMemory(Device& device, uint32_t physicalDeviceMask, MemoryType memoryType, uint64_t size, Memory*& memory)
{
    MaybeUnused(physicalDeviceMask); // TODO: use it

    return ((DeviceD3D12&)device).AllocateMemory(memoryType, size, memory);
}

static Result NRI_CALL BindBufferMemory(Device& device, const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceD3D12&)device).BindBufferMemory(memoryBindingDescs, memoryBindingDescNum);
}

static Result NRI_CALL BindTextureMemory(Device& device, const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceD3D12&)device).BindTextureMemory(memoryBindingDescs, memoryBindingDescNum);
}

static void NRI_CALL FreeMemory(Memory& memory)
{
    DeviceD3D12& device = ((MemoryD3D12&)memory).GetDevice();
    device.FreeMemory(memory);
}

static FormatSupportBits NRI_CALL GetFormatSupport(const Device& device, Format format)
{
    return ((const DeviceD3D12&)device).GetFormatSupport(format);
}

static void NRI_CALL SetDeviceDebugName(Device& device, const char* name)
{
    ((DeviceD3D12&)device).SetDebugName(name);
}

static void NRI_CALL SetPipelineDebugName(Pipeline& pipeline, const char* name)
{
    ((PipelineD3D12&)pipeline).SetDebugName(name);
}

static void NRI_CALL SetPipelineLayoutDebugName(PipelineLayout& pipelineLayout, const char* name)
{
    ((PipelineLayoutD3D12&)pipelineLayout).SetDebugName(name);
}

static void NRI_CALL SetFrameBufferDebugName(FrameBuffer& frameBuffer, const char* name)
{
    ((FrameBufferD3D12&)frameBuffer).SetDebugName(name);
}

static void NRI_CALL SetMemoryDebugName(Memory& memory, const char* name)
{
    ((MemoryD3D12&)memory).SetDebugName(name);
}

static void* NRI_CALL GetDeviceNativeObject(const Device& device)
{
    return (DeviceD3D12&)device;
}

Result DeviceD3D12::FillFunctionTable(CoreInterface& coreInterface) const
{
    coreInterface = {};
    coreInterface.GetDeviceDesc = ::GetDeviceDesc;
    coreInterface.GetFormatSupport = ::GetFormatSupport;
    coreInterface.GetCommandQueue = ::GetCommandQueue;
    coreInterface.CreateCommandAllocator = ::CreateCommandAllocator;
    coreInterface.CreateDescriptorPool = ::CreateDescriptorPool;
    coreInterface.CreateBuffer = ::CreateBuffer;
    coreInterface.CreateTexture = ::CreateTexture;
    coreInterface.CreateBufferView = ::CreateBufferView;
    coreInterface.CreateTexture1DView = ::CreateTexture1DView;
    coreInterface.CreateTexture2DView = ::CreateTexture2DView;
    coreInterface.CreateTexture3DView = ::CreateTexture3DView;
    coreInterface.CreateSampler = ::CreateSampler;
    coreInterface.CreatePipelineLayout = ::CreatePipelineLayout;
    coreInterface.CreateGraphicsPipeline = ::CreateGraphicsPipeline;
    coreInterface.CreateComputePipeline = ::CreateComputePipeline;
    coreInterface.CreateFrameBuffer = ::CreateFrameBuffer;
    coreInterface.CreateQueryPool = ::CreateQueryPool;
    coreInterface.CreateFence = ::CreateFence;
    coreInterface.DestroyCommandAllocator = ::DestroyCommandAllocator;
    coreInterface.DestroyDescriptorPool = ::DestroyDescriptorPool;
    coreInterface.DestroyBuffer = ::DestroyBuffer;
    coreInterface.DestroyTexture = ::DestroyTexture;
    coreInterface.DestroyDescriptor = ::DestroyDescriptor;
    coreInterface.DestroyPipelineLayout = ::DestroyPipelineLayout;
    coreInterface.DestroyPipeline = ::DestroyPipeline;
    coreInterface.DestroyFence = ::DestroyFence;
    coreInterface.DestroyFrameBuffer = ::DestroyFrameBuffer;
    coreInterface.DestroyQueryPool = ::DestroyQueryPool;
    coreInterface.AllocateMemory = ::AllocateMemory;
    coreInterface.BindBufferMemory = ::BindBufferMemory;
    coreInterface.BindTextureMemory = ::BindTextureMemory;
    coreInterface.FreeMemory = ::FreeMemory;
    coreInterface.SetDeviceDebugName = ::SetDeviceDebugName;
    coreInterface.SetPipelineDebugName = ::SetPipelineDebugName;
    coreInterface.SetPipelineLayoutDebugName = ::SetPipelineLayoutDebugName;
    coreInterface.SetFrameBufferDebugName = ::SetFrameBufferDebugName;
    coreInterface.SetMemoryDebugName = ::SetMemoryDebugName;
    coreInterface.GetDeviceNativeObject = ::GetDeviceNativeObject;

    Core_Buffer_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_CommandAllocator_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_CommandBuffer_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_CommandQueue_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_Descriptor_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_DescriptorPool_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_DescriptorSet_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_Fence_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_QueryPool_PartiallyFillFunctionTableD3D12(coreInterface);
    Core_Texture_PartiallyFillFunctionTableD3D12(coreInterface);

    return ValidateFunctionTable(GetLog(), coreInterface);
}

#pragma endregion

#pragma region [  SwapChain  ]

static Result NRI_CALL CreateSwapChain(Device& device, const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return ((DeviceD3D12&)device).CreateSwapChain(swapChainDesc, swapChain);
}

static void NRI_CALL DestroySwapChain(SwapChain& swapChain)
{
    DeviceD3D12& device = ((SwapChainD3D12&)swapChain).GetDevice();
    device.DestroySwapChain(swapChain);
}

static Result NRI_CALL GetDisplays(Device& device, Display** displays, uint32_t& displayNum)
{
    return ((DeviceD3D12&)device).GetDisplays(displays, displayNum);
}

static Result NRI_CALL GetDisplaySize(Device& device, Display& display, uint16_t& width, uint16_t& height)
{
    return ((DeviceD3D12&)device).GetDisplaySize(display, width, height);
}

Result DeviceD3D12::FillFunctionTable(SwapChainInterface& swapChainInterface) const
{
    swapChainInterface = {};
    swapChainInterface.CreateSwapChain = ::CreateSwapChain;
    swapChainInterface.DestroySwapChain = ::DestroySwapChain;
    swapChainInterface.GetDisplays = ::GetDisplays;
    swapChainInterface.GetDisplaySize = ::GetDisplaySize;

    SwapChain_PartiallyFillFunctionTableD3D12(swapChainInterface);

    return ValidateFunctionTable(GetLog(), swapChainInterface);
}

#pragma endregion

#pragma region [  WrapperD3D12  ]

static Result NRI_CALL CreateCommandBuffer(Device& device, const CommandBufferD3D12Desc& commandBufferDesc, CommandBuffer*& commandBuffer)
{
    return ((DeviceD3D12&)device).CreateCommandBuffer(commandBufferDesc, commandBuffer);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferD3D12Desc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceD3D12&)device).CreateBuffer(bufferDesc, buffer);
}

static Result NRI_CALL CreateTexture(Device& device, const TextureD3D12Desc& textureDesc, Texture*& texture)
{
    return ((DeviceD3D12&)device).CreateTexture(textureDesc, texture);
}

static Result NRI_CALL CreateMemory(Device& device, const MemoryD3D12Desc& memoryDesc, Memory*& memory)
{
    return ((DeviceD3D12&)device).CreateMemory(memoryDesc, memory);
}

static Result NRI_CALL CreateAccelerationStructure(Device& device, const AccelerationStructureD3D12Desc& accelerationStructureDesc, AccelerationStructure*& memory)
{
    return ((DeviceD3D12&)device).CreateAccelerationStructure(accelerationStructureDesc, memory);
}

Result DeviceD3D12::FillFunctionTable(WrapperD3D12Interface& wrapperD3D12Interface) const
{
    wrapperD3D12Interface = {};
    wrapperD3D12Interface.CreateCommandBufferD3D12 = ::CreateCommandBuffer;
    wrapperD3D12Interface.CreateBufferD3D12 = ::CreateBuffer;
    wrapperD3D12Interface.CreateTextureD3D12 = ::CreateTexture;
    wrapperD3D12Interface.CreateMemoryD3D12 = ::CreateMemory;
    wrapperD3D12Interface.CreateAccelerationStructureD3D12 = ::CreateAccelerationStructure;

    return ValidateFunctionTable(GetLog(), wrapperD3D12Interface);
}

#pragma endregion

#pragma region [  RayTracing  ]

static Result NRI_CALL CreateRayTracingPipeline(Device& device, const RayTracingPipelineDesc& rayTracingPipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceD3D12&)device).CreatePipeline(rayTracingPipelineDesc, pipeline);
}

static Result NRI_CALL CreateAccelerationStructure(Device& device, const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return ((DeviceD3D12&)device).CreateAccelerationStructure(accelerationStructureDesc, accelerationStructure);
}

static void NRI_CALL DestroyAccelerationStructure(AccelerationStructure& accelerationStructure)
{
    DeviceD3D12& device = ((AccelerationStructureD3D12&)accelerationStructure).GetDevice();
    device.DestroyAccelerationStructure(accelerationStructure);
}

static Result NRI_CALL BindAccelerationStructureMemory(Device& device, const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceD3D12&)device).BindAccelerationStructureMemory(memoryBindingDescs, memoryBindingDescNum);
}

void FillFunctionTablePipelineD3D12(RayTracingInterface& rayTracingInterface);

Result DeviceD3D12::FillFunctionTable(RayTracingInterface& rayTracingInterface) const
{
    rayTracingInterface = {};

    if (!m_Device5.GetInterface() || !m_IsRaytracingSupported)
        return Result::UNSUPPORTED;

    FillFunctionTablePipelineD3D12(rayTracingInterface);
    RayTracing_AccelerationStructure_PartiallyFillFunctionTableD3D12(rayTracingInterface);
    RayTracing_CommandBuffer_PartiallyFillFunctionTableD3D12(rayTracingInterface);

    rayTracingInterface.CreateRayTracingPipeline = ::CreateRayTracingPipeline;
    rayTracingInterface.CreateAccelerationStructure = ::CreateAccelerationStructure;
    rayTracingInterface.BindAccelerationStructureMemory = ::BindAccelerationStructureMemory;
    rayTracingInterface.DestroyAccelerationStructure = ::DestroyAccelerationStructure;

    return ValidateFunctionTable(GetLog(), rayTracingInterface);
}

#pragma endregion

#pragma region [  MeshShader  ]

Result DeviceD3D12::FillFunctionTable(MeshShaderInterface& meshShaderInterface) const
{
    if (!m_IsMeshShaderSupported)
        return Result::UNSUPPORTED;

    meshShaderInterface = {};

    MeshShader_CommandBuffer_PartiallyFillFunctionTableD3D12(meshShaderInterface);

    return ValidateFunctionTable(GetLog(), meshShaderInterface);
}

#pragma endregion

#pragma region [  Helper  ]

static uint32_t NRI_CALL CountAllocationNum(Device& device, const ResourceGroupDesc& resourceGroupDesc)
{
    return ((DeviceD3D12&)device).CalculateAllocationNumber(resourceGroupDesc);
}

static Result NRI_CALL AllocateAndBindMemory(Device& device, const ResourceGroupDesc& resourceGroupDesc, Memory** allocations)
{
    return ((DeviceD3D12&)device).AllocateAndBindMemory(resourceGroupDesc, allocations);
}

Result DeviceD3D12::FillFunctionTable(HelperInterface& helperInterface) const
{
    helperInterface = {};
    helperInterface.CalculateAllocationNumber = ::CountAllocationNum;
    helperInterface.AllocateAndBindMemory = ::AllocateAndBindMemory;

    Helper_CommandQueue_PartiallyFillFunctionTableD3D12(helperInterface);

    return ValidateFunctionTable(GetLog(), helperInterface);
}

#pragma endregion
