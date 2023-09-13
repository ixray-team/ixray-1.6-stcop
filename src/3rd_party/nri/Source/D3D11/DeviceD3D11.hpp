/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

Declare_PartiallyFillFunctionTable_Functions(D3D11)

#pragma region [  Core  ]

static const DeviceDesc& NRI_CALL GetDeviceDesc(const Device& device)
{
    return ((const DeviceD3D11&)device).GetDesc();
}

static Result NRI_CALL GetCommandQueue(Device& device, CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    return ((DeviceD3D11&)device).GetCommandQueue(commandQueueType, commandQueue);
}

static Result NRI_CALL CreateCommandAllocator(const CommandQueue& commandQueue, uint32_t physicalDeviceMask, CommandAllocator*& commandAllocator)
{
    MaybeUnused(physicalDeviceMask);

    DeviceD3D11& device = ((CommandQueueD3D11&)commandQueue).GetDevice();
    return device.CreateCommandAllocator(commandQueue, commandAllocator);
}

static Result NRI_CALL CreateDescriptorPool(Device& device, const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return ((DeviceD3D11&)device).CreateDescriptorPool(descriptorPoolDesc, descriptorPool);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferDesc& bufferDesc, Buffer*& buffer)
{
    return ((DeviceD3D11&)device).CreateBuffer(bufferDesc, buffer);
}

static Result NRI_CALL CreateTexture(Device& device, const TextureDesc& textureDesc, Texture*& texture)
{
    return ((DeviceD3D11&)device).CreateTexture(textureDesc, texture);
}

static Result NRI_CALL CreateBufferView(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    DeviceD3D11& device = ((const BufferD3D11*)bufferViewDesc.buffer)->GetDevice();
    return device.CreateDescriptor(bufferViewDesc, bufferView);
}

static Result NRI_CALL CreateTexture1DView(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceD3D11& device = ((const TextureD3D11*)textureViewDesc.texture)->GetDevice();
    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture2DView(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceD3D11& device = ((const TextureD3D11*)textureViewDesc.texture)->GetDevice();
    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateTexture3DView(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    DeviceD3D11& device = ((const TextureD3D11*)textureViewDesc.texture)->GetDevice();
    return device.CreateDescriptor(textureViewDesc, textureView);
}

static Result NRI_CALL CreateSampler(Device& device, const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return ((DeviceD3D11&)device).CreateDescriptor(samplerDesc, sampler);
}

static Result NRI_CALL CreatePipelineLayout(Device& device, const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    return ((DeviceD3D11&)device).CreatePipelineLayout(pipelineLayoutDesc, pipelineLayout);
}

static Result NRI_CALL CreateGraphicsPipeline(Device& device, const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceD3D11&)device).CreatePipeline(graphicsPipelineDesc, pipeline);
}

static Result NRI_CALL CreateComputePipeline(Device& device, const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    return ((DeviceD3D11&)device).CreatePipeline(computePipelineDesc, pipeline);
}

static Result NRI_CALL CreateFrameBuffer(Device& device, const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    return ((DeviceD3D11&)device).CreateFrameBuffer(frameBufferDesc, frameBuffer);
}

static Result NRI_CALL CreateQueryPool(Device& device, const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return ((DeviceD3D11&)device).CreateQueryPool(queryPoolDesc, queryPool);
}

static Result NRI_CALL CreateFence(Device& device, uint64_t initialValue, Fence*& queueSemaphore)
{
    return ((DeviceD3D11&)device).CreateFence(initialValue, queueSemaphore);
}

static void NRI_CALL DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    DeviceD3D11& device = ((CommandAllocatorD3D11&)commandAllocator).GetDevice();
    device.DestroyCommandAllocator(commandAllocator);
}

static void NRI_CALL DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    DeviceD3D11& device = ((DescriptorPoolD3D11&)descriptorPool).GetDevice();
    device.DestroyDescriptorPool(descriptorPool);
}

static void NRI_CALL DestroyBuffer(Buffer& buffer)
{
    DeviceD3D11& device = ((BufferD3D11&)buffer).GetDevice();
    device.DestroyBuffer(buffer);
}

static void NRI_CALL DestroyTexture(Texture& texture)
{
    DeviceD3D11& device = ((TextureD3D11&)texture).GetDevice();
    device.DestroyTexture(texture);
}

static void NRI_CALL DestroyDescriptor(Descriptor& descriptor)
{
    DeviceD3D11& device = ((DescriptorD3D11&)descriptor).GetDevice();
    device.DestroyDescriptor(descriptor);
}

static void NRI_CALL DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    DeviceD3D11& device = ((PipelineLayoutD3D11&)pipelineLayout).GetDevice();
    device.DestroyPipelineLayout(pipelineLayout);
}

static void NRI_CALL DestroyPipeline(Pipeline& pipeline)
{
    DeviceD3D11& device = ((PipelineD3D11&)pipeline).GetDevice();
    device.DestroyPipeline(pipeline);
}

static void NRI_CALL DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    DeviceD3D11& device = ((FrameBufferD3D11&)frameBuffer).GetDevice();
    device.DestroyFrameBuffer(frameBuffer);
}

static void NRI_CALL DestroyQueryPool(QueryPool& queryPool)
{
    DeviceD3D11& device = ((QueryPoolD3D11&)queryPool).GetDevice();
    device.DestroyQueryPool(queryPool);
}

static void NRI_CALL DestroyFence(Fence& fence)
{
    DeviceD3D11& device = ((FenceD3D11&)fence).GetDevice();
    device.DestroyFence(fence);
}

static Result NRI_CALL AllocateMemory(Device& device, uint32_t physicalDeviceMask, MemoryType memoryType, uint64_t size, Memory*& memory)
{
    MaybeUnused(physicalDeviceMask); // TODO: use it

    return ((DeviceD3D11&)device).AllocateMemory(memoryType, size, memory);
}

static Result NRI_CALL BindBufferMemory(Device& device, const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceD3D11&)device).BindBufferMemory(memoryBindingDescs, memoryBindingDescNum);
}

static Result NRI_CALL BindTextureMemory(Device& device, const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    return ((DeviceD3D11&)device).BindTextureMemory(memoryBindingDescs, memoryBindingDescNum);
}

static void NRI_CALL FreeMemory(Memory& memory)
{
    DeviceD3D11& device = ((MemoryD3D11&)memory).GetDevice();
    device.FreeMemory(memory);
}

static FormatSupportBits NRI_CALL GetFormatSupport(const Device& device, Format format)
{
    return ((const DeviceD3D11&)device).GetFormatSupport(format);
}

static void NRI_CALL SetDeviceDebugName(Device& device, const char* name)
{
    ((DeviceD3D11&)device).SetDebugName(name);
}

static void NRI_CALL SetPipelineDebugName(Pipeline& pipeline, const char* name)
{
    ((PipelineD3D11&)pipeline).SetDebugName(name);
}

static void NRI_CALL SetPipelineLayoutDebugName(PipelineLayout& pipelineLayout, const char* name)
{
    ((PipelineLayoutD3D11&)pipelineLayout).SetDebugName(name);
}

static void NRI_CALL SetFrameBufferDebugName(FrameBuffer& frameBuffer, const char* name)
{
    ((FrameBufferD3D11&)frameBuffer).SetDebugName(name);
}

static void NRI_CALL SetMemoryDebugName(Memory& memory, const char* name)
{
    ((MemoryD3D11&)memory).SetDebugName(name);
}

static void* NRI_CALL GetDeviceNativeObject(const Device& device)
{
    return ((DeviceD3D11&)device).GetDevice();
}

void Core_CommandBufferEmu_PartiallyFillFunctionTable(CoreInterface& coreInterface);

Result DeviceD3D11::FillFunctionTable(CoreInterface& coreInterface) const
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

    Core_Buffer_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_CommandAllocator_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_CommandQueue_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_Descriptor_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_DescriptorPool_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_DescriptorSet_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_Fence_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_QueryPool_PartiallyFillFunctionTableD3D11(coreInterface);
    Core_Texture_PartiallyFillFunctionTableD3D11(coreInterface);

    if (m_Device.isDeferredContextEmulated)
        Core_CommandBufferEmu_PartiallyFillFunctionTable(coreInterface);
    else
        Core_CommandBuffer_PartiallyFillFunctionTableD3D11(coreInterface);

    return ValidateFunctionTable(GetLog(), coreInterface);
}

#pragma endregion

#pragma region [  SwapChain  ]

static Result NRI_CALL CreateSwapChain(Device& device, const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return ((DeviceD3D11&)device).CreateSwapChain(swapChainDesc, swapChain);
}

static void NRI_CALL DestroySwapChain(SwapChain& swapChain)
{
    DeviceD3D11& device = ((SwapChainD3D11&)swapChain).GetDevice();
    return device.DestroySwapChain(swapChain);
}

static Result NRI_CALL GetDisplays(Device& device, Display** displays, uint32_t& displayNum)
{
    return ((DeviceD3D11&)device).GetDisplays(displays, displayNum);
}

static Result NRI_CALL GetDisplaySize(Device& device, Display& display, uint16_t& width, uint16_t& height)
{
    return ((DeviceD3D11&)device).GetDisplaySize(display, width, height);
}

Result DeviceD3D11::FillFunctionTable(SwapChainInterface& swapChainInterface) const
{
    swapChainInterface = {};
    swapChainInterface.CreateSwapChain = ::CreateSwapChain;
    swapChainInterface.DestroySwapChain = ::DestroySwapChain;
    swapChainInterface.GetDisplays = ::GetDisplays;
    swapChainInterface.GetDisplaySize = ::GetDisplaySize;

    SwapChain_PartiallyFillFunctionTableD3D11(swapChainInterface);

    return ValidateFunctionTable(GetLog(), swapChainInterface);
}

#pragma endregion

#pragma region [  WrapperD3D11  ]

Result CreateDeviceD3D11(const DeviceCreationD3D11Desc& deviceCreationD3D11Desc, DeviceBase*& device)
{
    DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.callbackInterface = deviceCreationD3D11Desc.callbackInterface;
    deviceCreationDesc.memoryAllocatorInterface = deviceCreationD3D11Desc.memoryAllocatorInterface;
    deviceCreationDesc.graphicsAPI = GraphicsAPI::D3D11;

    Log log(GraphicsAPI::D3D11, deviceCreationDesc.callbackInterface);
    StdAllocator<uint8_t> allocator(deviceCreationDesc.memoryAllocatorInterface);

    ComPtr<ID3D11Device> d3d11Device = (ID3D11Device*)deviceCreationD3D11Desc.d3d11Device;

    ComPtr<IDXGIDevice> dxgiDevice;
    HRESULT hr = d3d11Device->QueryInterface(IID_PPV_ARGS(&dxgiDevice));
    RETURN_ON_BAD_HRESULT(log, hr, "Can't create device. Failed to query IDXGIDevice from ID3D11Device. (result: %d)", (int32_t)hr);

    ComPtr<IDXGIAdapter> adapter;
    hr = dxgiDevice->GetAdapter(&adapter);
    RETURN_ON_BAD_HRESULT(log, hr, "Can't create device. IDXGIDevice::GetAdapter() failed. (result: %d)", (int32_t)hr);

    DeviceD3D11* implementation = Allocate<DeviceD3D11>(allocator, log, allocator);
    const nri::Result result = implementation->Create(deviceCreationDesc, adapter, d3d11Device, (AGSContext*)deviceCreationD3D11Desc.agsContextAssociatedWithDevice);

    if (result == nri::Result::SUCCESS)
    {
        device = (DeviceBase*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(allocator, implementation);

    return result;
}

static Result NRI_CALL CreateCommandBuffer(Device& device, const CommandBufferD3D11Desc& commandBufferDesc, CommandBuffer*& commandBuffer)
{
    DeviceD3D11& deviceD3D11 = (DeviceD3D11&)device;

    return ::CreateCommandBuffer(deviceD3D11, (ID3D11DeviceContext*)commandBufferDesc.d3d11DeviceContext, commandBuffer);
}

static Result NRI_CALL CreateBuffer(Device& device, const BufferD3D11Desc& bufferDesc, Buffer*& buffer)
{
    DeviceD3D11& deviceD3D11 = (DeviceD3D11&)device;

    BufferD3D11* implementation = Allocate<BufferD3D11>(deviceD3D11.GetStdAllocator(), deviceD3D11);
    const nri::Result res = implementation->Create(bufferDesc);

    if (res == nri::Result::SUCCESS)
    {
        buffer = (Buffer*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(deviceD3D11.GetStdAllocator(), implementation);

    return res;
}

static Result NRI_CALL CreateTexture(Device& device, const TextureD3D11Desc& textureDesc, Texture*& texture)
{
    DeviceD3D11& deviceD3D11 = (DeviceD3D11&)device;

    TextureD3D11* implementation = Allocate<TextureD3D11>(deviceD3D11.GetStdAllocator(), deviceD3D11);
    const nri::Result res = implementation->Create(textureDesc);

    if (res == nri::Result::SUCCESS)
    {
        texture = (Texture*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(deviceD3D11.GetStdAllocator(), implementation);

    return res;
}

Result DeviceD3D11::FillFunctionTable(WrapperD3D11Interface& wrapperD3D11Interface) const
{
    wrapperD3D11Interface = {};
    wrapperD3D11Interface.CreateCommandBufferD3D11 = ::CreateCommandBuffer;
    wrapperD3D11Interface.CreateTextureD3D11 = ::CreateTexture;
    wrapperD3D11Interface.CreateBufferD3D11 = ::CreateBuffer;

    return ValidateFunctionTable(GetLog(), wrapperD3D11Interface);
}

#pragma endregion

#pragma region [  Helper  ]

static uint32_t NRI_CALL CountAllocationNum(Device& device, const ResourceGroupDesc& resourceGroupDesc)
{
    return ((DeviceD3D11&)device).CalculateAllocationNumber(resourceGroupDesc);
}

static Result NRI_CALL AllocateAndBindMemory(Device& device, const ResourceGroupDesc& resourceGroupDesc, Memory** allocations)
{
    return ((DeviceD3D11&)device).AllocateAndBindMemory(resourceGroupDesc, allocations);
}

Result DeviceD3D11::FillFunctionTable(HelperInterface& helperInterface) const
{
    helperInterface = {};
    helperInterface.CalculateAllocationNumber = ::CountAllocationNum;
    helperInterface.AllocateAndBindMemory = ::AllocateAndBindMemory;

    Helper_CommandQueue_PartiallyFillFunctionTableD3D11(helperInterface);

    return ValidateFunctionTable(GetLog(), helperInterface);
}

#pragma endregion
