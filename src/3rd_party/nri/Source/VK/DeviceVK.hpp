/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

Declare_PartiallyFillFunctionTable_Functions(VK)

#pragma region [  Core  ]

static const DeviceDesc& NRI_CALL GetDeviceDesc(const Device& device)
{
    return ((DeviceVK&)device).GetDesc();
}

static Result NRI_CALL GetCommandQueue(Device& device, CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    return ((DeviceVK&)device).GetCommandQueue(commandQueueType, commandQueue);
}

static Result NRI_CALL CreateCommandAllocator(const CommandQueue& commandQueue, uint32_t physicalDeviceMask, CommandAllocator*& commandAllocator)
{
    DeviceVK& device = ((CommandQueueVK&)commandQueue).GetDevice();
    return device.CreateCommandAllocator(commandQueue, physicalDeviceMask, commandAllocator);
}

static Result NRI_CALL CreateDescriptorPool(Device& device, const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return ((DeviceVK&)device).CreateDescriptorPool(descriptorPoolDesc, descriptorPool);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferDesc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceVK&)device).CreateBuffer(bufferDesc, buffer);
}

static Result NRI_CALL CreateTexture(Device& device, const TextureDesc& textureDesc, Texture*& texture)
{
    return ((DeviceVK&)device).CreateTexture(textureDesc, texture);
}

static Result NRI_CALL CreateBufferView(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    DeviceVK& device = ((const BufferVK*)bufferViewDesc.buffer)->GetDevice();
    return device.CreateBufferView(bufferViewDesc, bufferView);
}

static Result NRI_CALL CreateTexture1DView(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceVK& device = ((const TextureVK*)textureViewDesc.texture)->GetDevice();
    return device.CreateTexture1DView(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture2DView(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceVK& device = ((const TextureVK*)textureViewDesc.texture)->GetDevice();
    return device.CreateTexture2DView(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture3DView(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceVK& device = ((const TextureVK*)textureViewDesc.texture)->GetDevice();
    return device.CreateTexture3DView(textureViewDesc, textureView);
}

static Result NRI_CALL CreateSampler(Device& device, const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return ((DeviceVK&)device).CreateSampler(samplerDesc, sampler);
}

static Result NRI_CALL CreatePipelineLayout(Device& device, const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    return ((DeviceVK&)device).CreatePipelineLayout(pipelineLayoutDesc, pipelineLayout);
}

static Result NRI_CALL CreateGraphicsPipeline(Device& device, const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceVK&)device).CreatePipeline(graphicsPipelineDesc, pipeline);
}

static Result NRI_CALL CreateComputePipeline(Device& device, const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceVK&)device).CreatePipeline(computePipelineDesc, pipeline);
}

static Result NRI_CALL CreateFrameBuffer(Device& device, const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    return ((DeviceVK&)device).CreateFrameBuffer(frameBufferDesc, frameBuffer);
}

static Result NRI_CALL CreateQueryPool(Device& device, const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return ((DeviceVK&)device).CreateQueryPool(queryPoolDesc, queryPool);
}

static Result NRI_CALL CreateFence(Device& device, uint64_t initialValue, Fence*& fence)
{
    return ((DeviceVK&)device).CreateFence(initialValue, fence);
}

static void NRI_CALL DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    ((CommandAllocatorVK&)commandAllocator).GetDevice().DestroyCommandAllocator(commandAllocator);
}

static void NRI_CALL DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    ((DescriptorPoolVK&)descriptorPool).GetDevice().DestroyDescriptorPool(descriptorPool);
}

static void NRI_CALL DestroyBuffer(Buffer& buffer)
{
    ((BufferVK&)buffer).GetDevice().DestroyBuffer(buffer);
}

static void NRI_CALL DestroyTexture(Texture& texture)
{
    ((TextureVK&)texture).GetDevice().DestroyTexture(texture);
}

static void NRI_CALL DestroyDescriptor(Descriptor& descriptor)
{
    ((DescriptorVK&)descriptor).GetDevice().DestroyDescriptor(descriptor);
}

static void NRI_CALL DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    ((PipelineLayoutVK&)pipelineLayout).GetDevice().DestroyPipelineLayout(pipelineLayout);
}

static void NRI_CALL DestroyPipeline(Pipeline& pipeline)
{
    ((PipelineVK&)pipeline).GetDevice().DestroyPipeline(pipeline);
}

static void NRI_CALL DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    ((FrameBufferVK&)frameBuffer).GetDevice().DestroyFrameBuffer(frameBuffer);
}

static void NRI_CALL DestroyQueryPool(QueryPool& queryPool)
{
    ((QueryPoolVK&)queryPool).GetDevice().DestroyQueryPool(queryPool);
}

static void NRI_CALL DestroyFence(Fence& fence)
{
    ((FenceVK&)fence).GetDevice().DestroyFence(fence);
}

static Result NRI_CALL AllocateMemory(Device& device, uint32_t physicalDeviceMask, MemoryType memoryType, uint64_t size, Memory*& memory)
{
    return ((DeviceVK&)device).AllocateMemory(physicalDeviceMask, memoryType, size, memory);
}

static Result NRI_CALL BindBufferMemory(Device& device, const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceVK&)device).BindBufferMemory(memoryBindingDescs, memoryBindingDescNum);
}

static Result NRI_CALL BindTextureMemory(Device& device, const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceVK&)device).BindTextureMemory(memoryBindingDescs, memoryBindingDescNum);
}

static void NRI_CALL FreeMemory(Memory& memory)
{
    ((MemoryVK&)memory).GetDevice().FreeMemory(memory);
}

static FormatSupportBits NRI_CALL GetFormatSupport(const Device& device, Format format)
{
    return ((const DeviceVK&)device).GetFormatSupport(format);
}

static void NRI_CALL SetDeviceDebugName(Device& device, const char* name)
{
    ((DeviceVK&)device).SetDebugName(name);
}

static void NRI_CALL SetPipelineDebugName(Pipeline& pipeline, const char* name)
{
    ((PipelineVK&)pipeline).SetDebugName(name);
}

static void NRI_CALL SetPipelineLayoutDebugName(PipelineLayout& pipelineLayout, const char* name)
{
    ((PipelineLayoutVK&)pipelineLayout).SetDebugName(name);
}

static void NRI_CALL SetFrameBufferDebugName(FrameBuffer& frameBuffer, const char* name)
{
    ((FrameBufferVK&)frameBuffer).SetDebugName(name);
}

static void NRI_CALL SetMemoryDebugName(Memory& memory, const char* name)
{
    ((MemoryVK&)memory).SetDebugName(name);
}

static void* NRI_CALL GetDeviceNativeObject(const Device& device)
{
    return (VkDevice)((DeviceVK&)device);
}

Result DeviceVK::FillFunctionTable(CoreInterface& coreInterface) const
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

    Core_Buffer_PartiallyFillFunctionTableVK(coreInterface);
    Core_CommandAllocator_PartiallyFillFunctionTableVK(coreInterface);
    Core_CommandBuffer_PartiallyFillFunctionTableVK(coreInterface);
    Core_CommandQueue_PartiallyFillFunctionTableVK(coreInterface);
    Core_Descriptor_PartiallyFillFunctionTableVK(coreInterface);
    Core_DescriptorPool_PartiallyFillFunctionTableVK(coreInterface);
    Core_DescriptorSet_PartiallyFillFunctionTableVK(coreInterface);
    Core_Fence_PartiallyFillFunctionTableVK(coreInterface);
    Core_QueryPool_PartiallyFillFunctionTableVK(coreInterface);
    Core_Texture_PartiallyFillFunctionTableVK(coreInterface);

    return ValidateFunctionTable(GetLog(), coreInterface);
}

#pragma endregion

#pragma region [  SwapChain  ]

static Result NRI_CALL CreateSwapChain(Device& device, const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return ((DeviceVK&)device).CreateSwapChain(swapChainDesc, swapChain);
}

static void NRI_CALL DestroySwapChain(SwapChain& swapChain)
{
    return ((SwapChainVK&)swapChain).GetDevice().DestroySwapChain(swapChain);
}

static Result NRI_CALL GetDisplays(Device& device, nri::Display** displays, uint32_t& displayNum)
{
    return ((DeviceVK&)device).GetDisplays(displays, displayNum);
}

static Result NRI_CALL GetDisplaySize(Device& device, nri::Display& display, uint16_t& width, uint16_t& height)
{
    return ((DeviceVK&)device).GetDisplaySize(display, width, height);
}

Result DeviceVK::FillFunctionTable(SwapChainInterface& swapChainInterface) const
{
    swapChainInterface = {};
    swapChainInterface.CreateSwapChain = ::CreateSwapChain;
    swapChainInterface.DestroySwapChain = ::DestroySwapChain;
    swapChainInterface.GetDisplays = ::GetDisplays;
    swapChainInterface.GetDisplaySize = ::GetDisplaySize;

    SwapChain_PartiallyFillFunctionTableVK(swapChainInterface);

    return ValidateFunctionTable(GetLog(), swapChainInterface);
}

#pragma endregion

#pragma region [  WrapperVK  ]

static Result NRI_CALL CreateCommandQueue(Device& device, const CommandQueueVulkanDesc& commandQueueVulkanDesc, CommandQueue*& commandQueue)
{
    return ((DeviceVK&)device).CreateCommandQueue(commandQueueVulkanDesc, commandQueue);
}

static Result NRI_CALL CreateCommandAllocator(Device& device, const CommandAllocatorVulkanDesc& commandAllocatorVulkanDesc, CommandAllocator*& commandAllocator)
{
    return ((DeviceVK&)device).CreateCommandAllocator(commandAllocatorVulkanDesc, commandAllocator);
}

static Result NRI_CALL CreateCommandBuffer(Device& device, const CommandBufferVulkanDesc& commandBufferVulkanDesc, CommandBuffer*& commandBuffer)
{
    return ((DeviceVK&)device).CreateCommandBuffer(commandBufferVulkanDesc, commandBuffer);
}

static Result NRI_CALL CreateDescriptorPool(Device& device, NRIVkDescriptorPool vkDescriptorPool, DescriptorPool*& descriptorPool)
{
    return ((DeviceVK&)device).CreateDescriptorPool(vkDescriptorPool, descriptorPool);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferVulkanDesc& bufferVulkanDesc, Buffer*& buffer)
{
    return ((DeviceVK&)device).CreateBuffer(bufferVulkanDesc, buffer);
}

static Result NRI_CALL CreateTexture(Device& device, const TextureVulkanDesc& textureVulkanDesc, Texture*& texture)
{
    return ((DeviceVK&)device).CreateTexture(textureVulkanDesc, texture);
}

static Result NRI_CALL CreateMemory(Device& device, const MemoryVulkanDesc& memoryVulkanDesc, Memory*& memory)
{
    return ((DeviceVK&)device).CreateMemory(memoryVulkanDesc, memory);
}

static Result NRI_CALL CreateGraphicsPipeline(Device& device, NRIVkPipeline vkPipeline, Pipeline*& pipeline)
{
    return ((DeviceVK&)device).CreateGraphicsPipeline(vkPipeline, pipeline);
}

static Result NRI_CALL CreateComputePipeline(Device& device, NRIVkPipeline vkPipeline, Pipeline*& pipeline)
{
    return ((DeviceVK&)device).CreateComputePipeline(vkPipeline, pipeline);
}

static Result NRI_CALL CreateQueryPool(Device& device, const QueryPoolVulkanDesc& queryPoolVulkanDesc, QueryPool*& queryPool)
{
    return ((DeviceVK&)device).CreateQueryPool(queryPoolVulkanDesc, queryPool);
}

static Result NRI_CALL CreateAccelerationStructure(Device& device, const AccelerationStructureVulkanDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return ((DeviceVK&)device).CreateAccelerationStructure(accelerationStructureDesc, accelerationStructure);
}

static NRIVkPhysicalDevice NRI_CALL GetVkPhysicalDevice(const Device& device)
{
    return (VkPhysicalDevice)((DeviceVK&)device);
}

static NRIVkInstance NRI_CALL GetVkInstance(const Device& device)
{
    return (VkInstance)((DeviceVK&)device);
}

static NRIVkProcAddress NRI_CALL GetVkGetInstanceProcAddr(const Device& device)
{
    return (NRIVkProcAddress)((DeviceVK&)device).GetDispatchTable().GetInstanceProcAddr;
}

static NRIVkProcAddress NRI_CALL GetVkGetDeviceProcAddr(const Device& device)
{
    return (NRIVkProcAddress)((DeviceVK&)device).GetDispatchTable().GetDeviceProcAddr;
}

Result DeviceVK::FillFunctionTable(WrapperVKInterface& wrapperVKInterface) const
{
    wrapperVKInterface = {};
    wrapperVKInterface.CreateCommandQueueVK = ::CreateCommandQueue;
    wrapperVKInterface.CreateCommandAllocatorVK = ::CreateCommandAllocator;
    wrapperVKInterface.CreateCommandBufferVK = ::CreateCommandBuffer;
    wrapperVKInterface.CreateDescriptorPoolVK = ::CreateDescriptorPool;
    wrapperVKInterface.CreateBufferVK = ::CreateBuffer;
    wrapperVKInterface.CreateTextureVK = ::CreateTexture;
    wrapperVKInterface.CreateMemoryVK = ::CreateMemory;
    wrapperVKInterface.CreateGraphicsPipelineVK = ::CreateGraphicsPipeline;
    wrapperVKInterface.CreateComputePipelineVK = ::CreateComputePipeline;
    wrapperVKInterface.CreateQueryPoolVK = ::CreateQueryPool;
    wrapperVKInterface.CreateAccelerationStructureVK = ::CreateAccelerationStructure;
    wrapperVKInterface.GetVkPhysicalDevice = ::GetVkPhysicalDevice;
    wrapperVKInterface.GetVkInstance = ::GetVkInstance;
    wrapperVKInterface.GetVkGetDeviceProcAddr = ::GetVkGetDeviceProcAddr;
    wrapperVKInterface.GetVkGetInstanceProcAddr = ::GetVkGetInstanceProcAddr;

    return ValidateFunctionTable(GetLog(), wrapperVKInterface);
}

#pragma endregion

#pragma region [  RayTracing  ]

static Result NRI_CALL CreateRayTracingPipeline(Device& device, const RayTracingPipelineDesc& pipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceVK&)device).CreatePipeline(pipelineDesc, pipeline);
}

static Result NRI_CALL CreateAccelerationStructure(Device& device, const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return ((DeviceVK&)device).CreateAccelerationStructure(accelerationStructureDesc, accelerationStructure);
}

static Result NRI_CALL BindAccelerationStructureMemory(Device& device, const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceVK&)device).BindAccelerationStructureMemory(memoryBindingDescs, memoryBindingDescNum);
}

static void NRI_CALL DestroyAccelerationStructure(AccelerationStructure& accelerationStructure)
{
    return ((AccelerationStructureVK&)accelerationStructure).GetDevice().DestroyAccelerationStructure(accelerationStructure);
}

void FillFunctionTablePipelineVK(RayTracingInterface& rayTracingInterface);

Result DeviceVK::FillFunctionTable(RayTracingInterface& rayTracingInterface) const
{
    if (!m_IsRayTracingExtSupported)
        return Result::UNSUPPORTED;

    rayTracingInterface = {};
    rayTracingInterface.CreateRayTracingPipeline = ::CreateRayTracingPipeline;
    rayTracingInterface.CreateAccelerationStructure = ::CreateAccelerationStructure;
    rayTracingInterface.BindAccelerationStructureMemory = ::BindAccelerationStructureMemory;
    rayTracingInterface.DestroyAccelerationStructure = ::DestroyAccelerationStructure;

    RayTracing_CommandBuffer_PartiallyFillFunctionTableVK(rayTracingInterface);
    RayTracing_AccelerationStructure_PartiallyFillFunctionTableVK(rayTracingInterface);
    FillFunctionTablePipelineVK(rayTracingInterface);

    return ValidateFunctionTable(GetLog(), rayTracingInterface);
}

#pragma endregion

#pragma region [  MeshShader  ]

Result DeviceVK::FillFunctionTable(MeshShaderInterface& meshShaderInterface) const
{
    if (!m_IsMeshShaderExtSupported)
        return Result::UNSUPPORTED;

    meshShaderInterface = {};

    MeshShader_CommandBuffer_PartiallyFillFunctionTableVK(meshShaderInterface);

    return ValidateFunctionTable(GetLog(), meshShaderInterface);
}

#pragma endregion

#pragma region [  Helper  ]

static uint32_t NRI_CALL CountAllocationNum(Device& device, const ResourceGroupDesc& resourceGroupDesc)
{
    return ((DeviceVK&)device).CalculateAllocationNumber(resourceGroupDesc);
}

static Result NRI_CALL AllocateAndBindMemory(Device& device, const ResourceGroupDesc& resourceGroupDesc, Memory** allocations)
{
    return ((DeviceVK&)device).AllocateAndBindMemory(resourceGroupDesc, allocations);
}

Result DeviceVK::FillFunctionTable(HelperInterface& helperInterface) const
{
    helperInterface = {};
    helperInterface.CalculateAllocationNumber = ::CountAllocationNum;
    helperInterface.AllocateAndBindMemory = ::AllocateAndBindMemory;

    Helper_CommandQueue_PartiallyFillFunctionTableVK(helperInterface);

    return ValidateFunctionTable(GetLog(), helperInterface);
}

#pragma endregion
