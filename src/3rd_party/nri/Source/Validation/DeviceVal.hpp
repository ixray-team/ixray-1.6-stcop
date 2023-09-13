/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

Declare_PartiallyFillFunctionTable_Functions(Val)

#pragma region [  Core  ]

static const DeviceDesc& NRI_CALL GetDeviceDesc(const Device& device)
{
    return ((const DeviceVal&)device).GetDesc();
}

static Result NRI_CALL GetCommandQueue(Device& device, CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    return ((DeviceVal&)device).GetCommandQueue(commandQueueType, commandQueue);
}

static Result NRI_CALL CreateCommandAllocator(const CommandQueue& commandQueue, uint32_t physicalDeviceMask, CommandAllocator*& commandAllocator)
{
    return GetDeviceVal(commandQueue).CreateCommandAllocator(commandQueue, physicalDeviceMask, commandAllocator);
}

static Result NRI_CALL CreateDescriptorPool(Device& device, const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return ((DeviceVal&)device).CreateDescriptorPool(descriptorPoolDesc, descriptorPool);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferDesc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceVal&)device).CreateBuffer(bufferDesc, buffer);
}

static Result NRI_CALL CreateTexture(Device& device, const TextureDesc& textureDesc, Texture*& texture)
{
    return ((DeviceVal&)device).CreateTexture(textureDesc, texture);
}

static Result NRI_CALL CreateBufferView(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    DeviceVal& device = GetDeviceVal(*bufferViewDesc.buffer);

    RETURN_ON_FAILURE(device.GetLog(), bufferViewDesc.buffer != nullptr, Result::INVALID_ARGUMENT,
        "Can't create a buffer view: the buffer is nullptr.");

    return device.CreateDescriptor(bufferViewDesc, bufferView);
}

static Result NRI_CALL CreateTexture1DView(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceVal& device = GetDeviceVal(*textureViewDesc.texture);

    RETURN_ON_FAILURE(device.GetLog(), textureViewDesc.texture != nullptr, Result::INVALID_ARGUMENT,
        "Can't create a texture view: the texture is nullptr.");

    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture2DView(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceVal& device = GetDeviceVal(*textureViewDesc.texture);

    RETURN_ON_FAILURE(device.GetLog(), textureViewDesc.texture != nullptr, Result::INVALID_ARGUMENT,
        "Can't create a texture view: the texture is nullptr.");

    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture3DView(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceVal& device = GetDeviceVal(*textureViewDesc.texture);

    RETURN_ON_FAILURE(device.GetLog(), textureViewDesc.texture != nullptr, Result::INVALID_ARGUMENT,
        "Can't create a texture view: the texture is nullptr.");

    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateSampler(Device& device, const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return ((DeviceVal&)device).CreateDescriptor(samplerDesc, sampler);
}

static Result NRI_CALL CreatePipelineLayout(Device& device, const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    return ((DeviceVal&)device).CreatePipelineLayout(pipelineLayoutDesc, pipelineLayout);
}

static Result NRI_CALL CreateGraphicsPipeline(Device& device, const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceVal&)device).CreatePipeline(graphicsPipelineDesc, pipeline);
}

static Result NRI_CALL CreateComputePipeline(Device& device, const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceVal&)device).CreatePipeline(computePipelineDesc, pipeline);
}

static Result NRI_CALL CreateFrameBuffer(Device& device, const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    return ((DeviceVal&)device).CreateFrameBuffer(frameBufferDesc, frameBuffer);
}

static Result NRI_CALL CreateQueryPool(Device& device, const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return ((DeviceVal&)device).CreateQueryPool(queryPoolDesc, queryPool);
}

static Result NRI_CALL CreateFence(Device& device, uint64_t initialValue, Fence*& fence)
{
    return ((DeviceVal&)device).CreateFence(initialValue, fence);
}

static void NRI_CALL DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    GetDeviceVal(commandAllocator).DestroyCommandAllocator(commandAllocator);
}

static void NRI_CALL DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    GetDeviceVal(descriptorPool).DestroyDescriptorPool(descriptorPool);
}

static void NRI_CALL DestroyBuffer(Buffer& buffer)
{
    GetDeviceVal(buffer).DestroyBuffer(buffer);
}

static void NRI_CALL DestroyTexture(Texture& texture)
{
    GetDeviceVal(texture).DestroyTexture(texture);
}

static void NRI_CALL DestroyDescriptor(Descriptor& descriptor)
{
    GetDeviceVal(descriptor).DestroyDescriptor(descriptor);
}

static void NRI_CALL DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    GetDeviceVal(pipelineLayout).DestroyPipelineLayout(pipelineLayout);
}

static void NRI_CALL DestroyPipeline(Pipeline& pipeline)
{
    GetDeviceVal(pipeline).DestroyPipeline(pipeline);
}

static void NRI_CALL DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    GetDeviceVal(frameBuffer).DestroyFrameBuffer(frameBuffer);
}

static void NRI_CALL DestroyQueryPool(QueryPool& queryPool)
{
    GetDeviceVal(queryPool).DestroyQueryPool(queryPool);
}

static void NRI_CALL DestroyFence(Fence& fence)
{
    GetDeviceVal(fence).DestroyFence(fence);
}

static Result NRI_CALL AllocateMemory(Device& device, uint32_t physicalDeviceMask, MemoryType memoryType, uint64_t size, Memory*& memory)
{
    return ((DeviceVal&)device).AllocateMemory(physicalDeviceMask, memoryType, size, memory);
}

static Result NRI_CALL BindBufferMemory(Device& device, const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceVal&)device).BindBufferMemory(memoryBindingDescs, memoryBindingDescNum);
}

static Result NRI_CALL BindTextureMemory(Device& device, const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceVal&)device).BindTextureMemory(memoryBindingDescs, memoryBindingDescNum);
}

static void NRI_CALL FreeMemory(Memory& memory)
{
    GetDeviceVal(memory).FreeMemory(memory);
}

static FormatSupportBits NRI_CALL GetFormatSupport(const Device& device, Format format)
{
    return ((const DeviceVal&)device).GetFormatSupport(format);
}

static void NRI_CALL SetDeviceDebugName(Device& device, const char* name)
{
    ((DeviceVal&)device).SetDebugName(name);
}

static void NRI_CALL SetPipelineDebugName(Pipeline& pipeline, const char* name)
{
    ((PipelineVal&)pipeline).SetDebugName(name);
}

static void NRI_CALL SetPipelineLayoutDebugName(PipelineLayout& pipelineLayout, const char* name)
{
    ((PipelineLayoutVal&)pipelineLayout).SetDebugName(name);
}

static void NRI_CALL SetFrameBufferDebugName(FrameBuffer& frameBuffer, const char* name)
{
    ((FrameBufferVal&)frameBuffer).SetDebugName(name);
}

static void NRI_CALL SetMemoryDebugName(Memory& memory, const char* name)
{
    ((MemoryVal&)memory).SetDebugName(name);
}

static void* NRI_CALL GetDeviceNativeObject(const Device& device)
{
    return ((DeviceVal&)device).GetNativeObject();
}

Result DeviceVal::FillFunctionTable(CoreInterface& coreInterface) const
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
    coreInterface.DestroyFrameBuffer = ::DestroyFrameBuffer;
    coreInterface.DestroyQueryPool = ::DestroyQueryPool;
    coreInterface.DestroyFence = ::DestroyFence;
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

    Core_Buffer_PartiallyFillFunctionTableVal(coreInterface);
    Core_CommandAllocator_PartiallyFillFunctionTableVal(coreInterface);
    Core_CommandBuffer_PartiallyFillFunctionTableVal(coreInterface);
    Core_CommandQueue_PartiallyFillFunctionTableVal(coreInterface);
    Core_Descriptor_PartiallyFillFunctionTableVal(coreInterface);
    Core_DescriptorPool_PartiallyFillFunctionTableVal(coreInterface);
    Core_DescriptorSet_PartiallyFillFunctionTableVal(coreInterface);
    Core_Fence_PartiallyFillFunctionTableVal(coreInterface);
    Core_QueryPool_PartiallyFillFunctionTableVal(coreInterface);
    Core_Texture_PartiallyFillFunctionTableVal(coreInterface);

    return ValidateFunctionTable(GetLog(), coreInterface);
}

#pragma endregion

#pragma region [  SwapChain  ]

static Result NRI_CALL CreateSwapChain(Device& device, const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return ((DeviceVal&)device).CreateSwapChain(swapChainDesc, swapChain);
}

static void NRI_CALL DestroySwapChain(SwapChain& swapChain)
{
    GetDeviceVal(swapChain).DestroySwapChain(swapChain);
}

static Result NRI_CALL GetDisplays(Device& device, Display** displays, uint32_t& displayNum)
{
    return ((DeviceVal&)device).GetDisplays(displays, displayNum);
}

static Result NRI_CALL GetDisplaySize(Device& device, Display& display, uint16_t& width, uint16_t& height)
{
    return ((DeviceVal&)device).GetDisplaySize(display, width, height);
}

Result DeviceVal::FillFunctionTable(SwapChainInterface& swapChainInterface) const
{
    if (!m_IsSwapChainSupported)
        return Result::UNSUPPORTED;

    swapChainInterface = {};
    swapChainInterface.CreateSwapChain = ::CreateSwapChain;
    swapChainInterface.DestroySwapChain = ::DestroySwapChain;
    swapChainInterface.GetDisplays = ::GetDisplays;
    swapChainInterface.GetDisplaySize = ::GetDisplaySize;

    SwapChain_PartiallyFillFunctionTableVal(swapChainInterface);

    return ValidateFunctionTable(GetLog(), swapChainInterface);
}

#pragma endregion

#pragma region [  WrapperD3D11  ]

#if NRI_USE_D3D11

static Result NRI_CALL CreateCommandBufferD3D11(Device& device, const CommandBufferD3D11Desc& commandBufferDesc, CommandBuffer*& commandBuffer)
{
    return ((DeviceVal&)device).CreateCommandBufferD3D11(commandBufferDesc, commandBuffer);
}

static Result NRI_CALL CreateBufferD3D11(Device& device, const BufferD3D11Desc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceVal&)device).CreateBufferD3D11(bufferDesc, buffer);
}

static Result NRI_CALL CreateTextureD3D11(Device& device, const TextureD3D11Desc& textureDesc, Texture*& texture)
{
    return ((DeviceVal&)device).CreateTextureD3D11(textureDesc, texture);
}

#endif

Result DeviceVal::FillFunctionTable(WrapperD3D11Interface& wrapperD3D11Interface) const
{
#if NRI_USE_D3D11
    wrapperD3D11Interface = {};
    wrapperD3D11Interface.CreateCommandBufferD3D11 = ::CreateCommandBufferD3D11;
    wrapperD3D11Interface.CreateBufferD3D11 = ::CreateBufferD3D11;
    wrapperD3D11Interface.CreateTextureD3D11 = ::CreateTextureD3D11;

    return ValidateFunctionTable(GetLog(), wrapperD3D11Interface);
#else
    MaybeUnused(wrapperD3D11Interface);

    return Result::UNSUPPORTED;
#endif
}

#pragma endregion

#pragma region [  WrapperD3D12  ]

#if NRI_USE_D3D12

static Result NRI_CALL CreateCommandBufferD3D12(Device& device, const CommandBufferD3D12Desc& commandBufferDesc, CommandBuffer*& commandBuffer)
{
    return ((DeviceVal&)device).CreateCommandBufferD3D12(commandBufferDesc, commandBuffer);
}

static Result NRI_CALL CreateBufferD3D12(Device& device, const BufferD3D12Desc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceVal&)device).CreateBufferD3D12(bufferDesc, buffer);
}

static Result NRI_CALL CreateTextureD3D12(Device& device, const TextureD3D12Desc& textureDesc, Texture*& texture)
{
    return ((DeviceVal&)device).CreateTextureD3D12(textureDesc, texture);
}

static Result NRI_CALL CreateMemoryD3D12(Device& device, const MemoryD3D12Desc& memoryDesc, Memory*& memory)
{
    return ((DeviceVal&)device).CreateMemoryD3D12(memoryDesc, memory);
}

static Result NRI_CALL CreateAccelerationStructureD3D12(Device& device, const AccelerationStructureD3D12Desc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return ((DeviceVal&)device).CreateAccelerationStructureD3D12(accelerationStructureDesc, accelerationStructure);
}

#endif

Result DeviceVal::FillFunctionTable(WrapperD3D12Interface& wrapperD3D12Interface) const
{
#if NRI_USE_D3D12
    wrapperD3D12Interface = {};
    wrapperD3D12Interface.CreateCommandBufferD3D12 = ::CreateCommandBufferD3D12;
    wrapperD3D12Interface.CreateBufferD3D12 = ::CreateBufferD3D12;
    wrapperD3D12Interface.CreateTextureD3D12 = ::CreateTextureD3D12;
    wrapperD3D12Interface.CreateMemoryD3D12 = ::CreateMemoryD3D12;
    wrapperD3D12Interface.CreateAccelerationStructureD3D12 = ::CreateAccelerationStructureD3D12;

    return ValidateFunctionTable(GetLog(), wrapperD3D12Interface);

#else
    MaybeUnused(wrapperD3D12Interface);

    return Result::UNSUPPORTED;
#endif
}

#pragma endregion

#pragma region [  WrapperVK  ]

#if NRI_USE_VULKAN

static Result NRI_CALL CreateCommandQueueVK(Device& device, const CommandQueueVulkanDesc& commandQueueVulkanDesc, CommandQueue*& commandQueue)
{
    return ((DeviceVal&)device).CreateCommandQueueVK(commandQueueVulkanDesc, commandQueue);
}

static Result NRI_CALL CreateCommandAllocatorVK(Device& device, const CommandAllocatorVulkanDesc& commandAllocatorVulkanDesc, CommandAllocator*& commandAllocator)
{
    return ((DeviceVal&)device).CreateCommandAllocatorVK(commandAllocatorVulkanDesc, commandAllocator);
}

static Result NRI_CALL CreateCommandBufferVK(Device& device, const CommandBufferVulkanDesc& commandBufferVulkanDesc, CommandBuffer*& commandBuffer)
{
    return ((DeviceVal&)device).CreateCommandBufferVK(commandBufferVulkanDesc, commandBuffer);
}

static Result NRI_CALL CreateDescriptorPoolVK(Device& device, NRIVkDescriptorPool vkDescriptorPool, DescriptorPool*& descriptorPool)
{
    return ((DeviceVal&)device).CreateDescriptorPoolVK(vkDescriptorPool, descriptorPool);
}

static Result NRI_CALL CreateBufferVK(Device& device, const BufferVulkanDesc& bufferVulkanDesc, Buffer*& buffer)
{
    return ((DeviceVal&)device).CreateBufferVK(bufferVulkanDesc, buffer);
}

static Result NRI_CALL CreateTextureVK(Device& device, const TextureVulkanDesc& textureVulkanDesc, Texture*& texture)
{
    return ((DeviceVal&)device).CreateTextureVK(textureVulkanDesc, texture);
}

static Result NRI_CALL CreateMemoryVK(Device& device, const MemoryVulkanDesc& memoryVulkanDesc, Memory*& memory)
{
    return ((DeviceVal&)device).CreateMemoryVK(memoryVulkanDesc, memory);
}

static Result NRI_CALL CreateGraphicsPipelineVK(Device& device, NRIVkPipeline vkPipeline, Pipeline*& pipeline)
{
    return ((DeviceVal&)device).CreateGraphicsPipelineVK(vkPipeline, pipeline);
}

static Result NRI_CALL CreateComputePipelineVK(Device& device, NRIVkPipeline vkPipeline, Pipeline*& pipeline)
{
    return ((DeviceVal&)device).CreateComputePipelineVK(vkPipeline, pipeline);
}

static Result NRI_CALL CreateQueryPoolVK(Device& device, const QueryPoolVulkanDesc& queryPoolVulkanDesc, QueryPool*& queryPool)
{
    return ((DeviceVal&)device).CreateQueryPoolVK(queryPoolVulkanDesc, queryPool);
}

static Result NRI_CALL CreateAccelerationStructureVK(Device& device, const AccelerationStructureVulkanDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return ((DeviceVal&)device).CreateAccelerationStructureVK(accelerationStructureDesc, accelerationStructure);
}

static NRIVkPhysicalDevice NRI_CALL GetVkPhysicalDevice(const Device& device)
{
    return ((DeviceVal&)device).GetVkPhysicalDevice();
}

static NRIVkInstance NRI_CALL GetVkInstance(const Device& device)
{
    return ((DeviceVal&)device).GetVkInstance();
}

static NRIVkInstance NRI_CALL GetVkGetInstanceProcAddr(const Device& device)
{
    return ((DeviceVal&)device).GetVkGetInstanceProcAddr();
}

static NRIVkInstance NRI_CALL GetVkGetDeviceProcAddr(const Device& device)
{
    return ((DeviceVal&)device).GetVkGetDeviceProcAddr();
}

#endif

Result DeviceVal::FillFunctionTable(WrapperVKInterface& wrapperVKInterface) const
{
#if NRI_USE_VULKAN
    wrapperVKInterface = {};
    wrapperVKInterface.CreateCommandQueueVK = ::CreateCommandQueueVK;
    wrapperVKInterface.CreateCommandAllocatorVK = ::CreateCommandAllocatorVK;
    wrapperVKInterface.CreateCommandBufferVK = ::CreateCommandBufferVK;
    wrapperVKInterface.CreateDescriptorPoolVK = ::CreateDescriptorPoolVK;
    wrapperVKInterface.CreateBufferVK = ::CreateBufferVK;
    wrapperVKInterface.CreateTextureVK = ::CreateTextureVK;
    wrapperVKInterface.CreateMemoryVK = ::CreateMemoryVK;
    wrapperVKInterface.CreateGraphicsPipelineVK = ::CreateGraphicsPipelineVK;
    wrapperVKInterface.CreateComputePipelineVK = ::CreateComputePipelineVK;
    wrapperVKInterface.CreateQueryPoolVK = ::CreateQueryPoolVK;
    wrapperVKInterface.CreateAccelerationStructureVK = ::CreateAccelerationStructureVK;

    wrapperVKInterface.GetVkPhysicalDevice = ::GetVkPhysicalDevice;
    wrapperVKInterface.GetVkInstance = ::GetVkInstance;
    wrapperVKInterface.GetVkGetDeviceProcAddr = ::GetVkGetDeviceProcAddr;
    wrapperVKInterface.GetVkGetInstanceProcAddr = ::GetVkGetInstanceProcAddr;

    return ValidateFunctionTable(GetLog(), wrapperVKInterface);
#else
    MaybeUnused(wrapperVKInterface);

    return Result::UNSUPPORTED;
#endif
}

#pragma endregion

#pragma region [  RayTracing  ]

static Result NRI_CALL CreateRayTracingPipeline(Device& device, const RayTracingPipelineDesc& pipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceVal&)device).CreateRayTracingPipeline(pipelineDesc, pipeline);
}

static Result NRI_CALL CreateAccelerationStructure(Device& device, const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return ((DeviceVal&)device).CreateAccelerationStructure(accelerationStructureDesc, accelerationStructure);
}

static Result NRI_CALL BindAccelerationStructureMemory(Device& device, const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceVal&)device).BindAccelerationStructureMemory(memoryBindingDescs, memoryBindingDescNum);
}

static void NRI_CALL DestroyAccelerationStructure(AccelerationStructure& accelerationStructure)
{
    GetDeviceVal(accelerationStructure).DestroyAccelerationStructure(accelerationStructure);
}

void FillFunctionTablePipelineVal(RayTracingInterface& rayTracingInterface);

Result DeviceVal::FillFunctionTable(RayTracingInterface& rayTracingInterface) const
{
    if (!m_IsRayTracingSupported)
        return Result::UNSUPPORTED;

    rayTracingInterface = {};
    rayTracingInterface.CreateRayTracingPipeline = ::CreateRayTracingPipeline;
    rayTracingInterface.CreateAccelerationStructure = ::CreateAccelerationStructure;
    rayTracingInterface.BindAccelerationStructureMemory = ::BindAccelerationStructureMemory;
    rayTracingInterface.DestroyAccelerationStructure = ::DestroyAccelerationStructure;

    RayTracing_CommandBuffer_PartiallyFillFunctionTableVal(rayTracingInterface);
    RayTracing_AccelerationStructure_PartiallyFillFunctionTableVal(rayTracingInterface);
    FillFunctionTablePipelineVal(rayTracingInterface);

    return ValidateFunctionTable(GetLog(), rayTracingInterface);
}

#pragma endregion

#pragma region [  MeshShader  ]

Result DeviceVal::FillFunctionTable(MeshShaderInterface& meshShaderInterface) const
{
    if (!m_IsMeshShaderExtSupported)
        return Result::UNSUPPORTED;

    meshShaderInterface = {};

    MeshShader_CommandBuffer_PartiallyFillFunctionTableVal(meshShaderInterface);

    return ValidateFunctionTable(GetLog(), meshShaderInterface);
}

#pragma endregion

#pragma region [  Helper  ]

static uint32_t NRI_CALL CountAllocationNum(Device& device, const ResourceGroupDesc& resourceGroupDesc)
{
    return ((DeviceVal&)device).CalculateAllocationNumber(resourceGroupDesc);
}

static Result NRI_CALL AllocateAndBindMemory(Device& device, const ResourceGroupDesc& resourceGroupDesc, Memory** allocations)
{
    return ((DeviceVal&)device).AllocateAndBindMemory(resourceGroupDesc, allocations);
}

Result DeviceVal::FillFunctionTable(HelperInterface& helperInterface) const
{
    helperInterface = {};
    helperInterface.CalculateAllocationNumber = ::CountAllocationNum;
    helperInterface.AllocateAndBindMemory = ::AllocateAndBindMemory;

    Helper_CommandQueue_PartiallyFillFunctionTableVal(helperInterface);

    return ValidateFunctionTable(GetLog(), helperInterface);
}

#pragma endregion
