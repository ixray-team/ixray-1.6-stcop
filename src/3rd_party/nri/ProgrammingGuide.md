# NRI Programming Guide

NRI is an abstract render interface which provides a single interface to a set of implementations which translate NRI calls into graphics API calls.

Supported APIs:
- D3D11
- D3D12
- Vulkan

Supported platforms:
- Windows
- Linux

Supported CPU architectures:
- x86
- ARM

NRI can be used as a *shared* or *static* library.

## C/C++ interface differences

| C++                   | C                     |
|-----------------------|-----------------------|
| `nri::`               | `nri_`                |
| Reference `&`         | Pointer `*`           |
| `nri::EnumClass::Name`| `NRI_ENUM_CLASS_NAME` |

## Objects

Analogues from the graphics APIs:

| NRI               | D3D12             | Vulkan                     |
|-------------------|-------------------|----------------------------|
| Device            | Device            | Device                     |
| Command Buffer    | Command List      | Command Buffer             |
| Command Queue     | Command Queue     | Queue                      |
| Fence             | Fence             | Timeline semaphore         |
| Command Allocator | Command Allocator | Command Pool               |
| Buffer            | Resource          | Buffer                     |
| Texture           | Resource          | Image                      |
| Memory            | Heap              | Device Memory              |
| Descriptor        | View / Sampler    | View / Sampler             |
| Descriptor Set    | --                | Descriptor Set             |
| Descriptor Pool   | Descriptor heap   | Descriptor Pool            |
| Frame Buffer      | --                | Frame Buffer & Render Pass |
| Pipeline Layout   | Root signature    | Pipeline Layout            |
| Pipeline          | Pipeline          | Pipeline                   |

`Device` is an abstract logical device which allows the application to work with one or more physical devices.

`Command Buffer` is a list of commands for a physical device.

`Command Queue` is a queue of `Command Buffers` which are ready for execution on a physical device. A physical device may support multiple queue types, queues of different types are processed in parallel by the physical device.

`Fence` is a synchronization primitive which allows the application:
- to synchronize execution of `Command Buffers` between `Command Queues`
- to wait for a physical device to complete execution of command buffers

`Command Allocator` is a memory allocator for command recording. When the application records commands into `Command Buffer`, memory for the commands is allocated from `Command Allocator` associated with the `Command Buffer`.

`Buffer` is a resource which represents a continuous range of items or memory.

`Texture` is a resource which represents a multidimensional array of pixels.

`Memory` is a physical device memory, `Buffers` and `Textures` must be bound to `Memory` before they can be accessed.

`Descriptor` is a texture view, buffer view or sampler.

`Descriptor Set` is a set of descriptors which can be accessed by a shader.

`Descriptor Pool` is a pool of descriptors which allocates memory for `Descriptor Sets`.

`Frame Buffer` is a state object which contains information on color attachments, depth attachments and their usage.

`Pipeline Layout` is a state object which contains information on shader resources and pipeline stages.

`Pipeline` is a state object which contains information on pipeline state.

# Initialization

To start using NRI, the application needs to create a device. The NRI device can be created using an existing graphics API device or without it:

1. to create an NRI device from existing D3D11, D3D12 or Vulkan device
    * use a wrapping extension (`NRIWrapper*.h`)
2. to create an NRI driver without wrapping
    * use a device creation extension (`NRIDeviceCreation.h`)

## Creating device using vulkan wrapping extension

`NRIWrapperVK.h` contains the following function:

```cpp
Result CreateDevice(const DeviceCreationVulkanDesc& deviceDesc, Device*& device)
```

The application must specify the following members of `DeviceCreationVulkanDesc`:
* `deviceDesc.vkInstance`
  * VkInstance handle
* `deviceDesc.vkDevice`
  * VkDevice handle
* `deviceDesc.vkPhysicalDevices`
  * an array of VkPhysicalDevice handles
* `deviceDesc.deviceGroupSize`
  * the number of physical devices in the device group (the number of VkPhysicalDevice handles)
* `deviceDesc.queueFamilyIndices`
  * an array of queue family indices which were enabled on creation of the Vulkan logical device
* `deviceDesc.queueFamilyIndexNum`
  * The number of queue family indices

Other members of `DeviceCreationVulkanDesc` can be filled with zeros.

NRI library contains implementation of the `CreateDevice()` function.

## Creating device without wrapping

`NRIDeviceCreation.h` contains the following functions:

```cpp
Result GetPhysicalDevices(PhysicalDeviceGroup* physicalDeviceGroups, uint32_t& physicalDeviceGroupNum)
```

The function allows the application to enumerate available physical devices.

Valid usage:
* To get the number of physical devices, the application can call the function with physicalDeviceGroupNum set to 0

Multithreading:
* No synchronization required

```cpp
Result CreateDevice(const DeviceCreationDesc& deviceCreationDesc, Device*& device)
```
* `deviceCreationDesc.physicalDeviceGroup`
  * implementation will choose the first physical device if the value is nullptr
* `deviceCreationDesc.graphicsAPI`
  * controls which implementation will be used (D3D11, D3D12, VK)

Valid usage:
* The application can create any number of NRI devices using different implementations, there is no limitation on this

Multithreading:
* No synchronization required

## Destroying device

An NRI device must be explicitly destroyed to free memory and graphics API objects. NRI does not destroy D3D/VK objects passed to NRI via Wrapping Extension.

```cpp
void DestroyDevice(Device& device)
```

Valid usage:
* All NRI objects which were created using the device must be already destroyed

Multithreading:
* Access to device must be externally synchronized

# Querying interfaces

To get NRI interface, the application can use the following function:

```cpp
Result GetInterface(const Device& device, const char* interfaceName, size_t interfaceSize, void* interfacePtr)
```
* `interfaceName`
  * The name of the interface C++ struct with "nri::" prefix

`NRI.h` defines a macro which can be used to get an interface name and interface size from a structure name:

```cpp
nri::GetInterface(*m_Device, NRI_INTERFACE(nri::CoreInterface), (nri::CoreInterface*)&NRI)
```

# Core Interface

```cpp
const DeviceDesc& GetDeviceDesc(const Device& device)
```

Returns the device description which contains capabilities of the device and some info on the NRI implementation.

Multithreading:
* No synchronization required

```cpp
Result GetCommandQueue(Device& device, CommandQueueType commandQueueType, CommandQueue*& commandQueue)
```

Valid usage:
* The application can call this function to check which command queue types are supported

Multithreading:
* No synchronization required

```cpp
Result CreateCommandAllocator(const CommandQueue& commandQueue, uint32_t physicalDeviceMask, CommandAllocator*& commandAllocator)
```

Valid usage:
* physicalDeviceMask can be set to 0 (WHOLE_DEVICE_GROUP) to create a command allocator which can be used with all physical devices in the device group

Multithreading:
* No synchronization required

```cpp
Result CreateDescriptorPool(Device& device, const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
```

Creates a descriptor pool which can be used to allocate descriptor sets.

If descriptorPoolDesc.physicalDeviceMask is set to 0 or has more than one bit set and the device group consists of more than one physical device, the maximum number of descriptors in the descriptor pool is multiplied by the number of physical devices.

Valid usage:
* The function may fail if the maximum number of descriptors exceeds the maximum size of D3D12 descriptor heap

Multithreading:
* No synchronization required

```cpp
Result CreateBuffer(Device& device, const BufferDesc& bufferDesc, Buffer*& buffer)
```

Creates a buffer which must be bound to memory before it can be used on GPU.

Multithreading:
* No synchronization required

```cpp
Result CreateTexture(Device& device, const TextureDesc& textureDesc, Texture*& texture)
```

Creates a texture which must be bound to memory before it can be used on GPU.

Multithreading:
* No synchronization required

```cpp
Result CreateBufferView(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
```

Valid usage:
* The buffer must be bound to memory

Multithreading:
* No synchronization required

```cpp
Result CreateTexture1DView(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
```

Valid usage:
* The texture must be bound to memory

Multithreading:
* No synchronization required

```cpp
Result CreateTexture2DView(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
```

Valid usage:
* The texture must be bound to memory.

Multithreading:
* No synchronization required

```cpp
Result CreateTexture3DView(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
```

Valid usage:
* The texture must be bound to memory

Multithreading:
* No synchronization required

```cpp
Result CreateSampler(Device& device, const SamplerDesc& samplerDesc, Descriptor*& sampler)
```

Multithreading:
* No synchronization required

```cpp
Result CreatePipelineLayout(Device& device, const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
```

Creates a pipeline layout which describes resource binding for descriptors sets and pipelines.

Valid usage:
* pipelineLayoutDesc.stageMask can be a combination of bits which correspond to only one type of pipeline (graphics, compute, or ray tracing). For example, a pipeline layout can not be created for graphics and compute stages together, the application must create separate pipeline layouts pipelineLayoutDesc.stageMask must be a minimal set of pipeline stages which need access to shader resources.
* If the pipeline layout is used with a pipeline which has a vertex stage that needs access to vertex attributes, pipelineLayoutDesc.stageMask must include the vertex pipeline stage

Multithreading:
* No synchronization required

```cpp
Result CreateGraphicsPipeline(Device& device, const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
```

Valid usage:
* graphicsPipelineDesc.pipelineLayout must be a valid pipeline layout
* graphicsPipelineDesc.inputAssembly must be a valid pointer to InputAssemblyDesc
* graphicsPipelineDesc.rasterization can be nullptr
* graphicsPipelineDesc.outputMerger can be nullptr
* graphicsPipelineDesc.shaderStageNum must be greater than 0

Multithreading:
* No synchronization required

```cpp
Result CreateComputePipeline(Device& device, const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
```

Valid usage:
* computePipelineDesc.pipelineLayout must be a valid pipeline layout
* computePipelineDesc.computeShader must be a valid shader description

Multithreading:
* No synchronization required

```cpp
Result CreateFrameBuffer(Device& device, const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
```

Multithreading:
* No synchronization required

```cpp
Result CreateQueryPool(Device& device, const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
```

Multithreading:
* No synchronization required


```cpp
Result CreateQueueFence(Device& device, uint64_t initialValue, Fence*& fence)
```

Multithreading:
* No synchronization required

```cpp
Result CreateCommandBuffer(CommandAllocator& commandAllocator, CommandBuffer*& commandBuffer)
```

Multithreading:
* No synchronization required

```cpp
void DestroyCommandAllocator(CommandAllocator& commandAllocator)
```

Valid usage:
* All command buffers that were allocated using the command allocator must be destroyed* The application must ensure that all command buffers which were allocated using the command allocator have completed execution

Multithreading:
* Access to commandAllocator must be externally synchronized

```cpp
void DestroyDescriptorPool(DescriptorPool& descriptorPool)
```

Destroys the descriptor pool and its descriptor sets.

Valid usage:
* The application must ensure that all command buffers which use the descriptor pool and the descriptor sets have completed execution

Multithreading:
* Access to descriptorPool must be externally synchronized

```cpp
void DestroyBuffer(Buffer& buffer)
```

Valid usage:
* The application must ensure that GPU finished execution of command buffers which use the buffer
* All descriptors (buffer views) which reference the buffer must be destroyed

Multithreading:
* Access to buffer must be externally synchronized

```cpp
void DestroyTexture(Texture& texture)
```

Valid usage:
* The application must ensure that all command buffers which use the texture have completed execution
* All descriptors (texture views) which reference the texture must be destroyed

Multithreading:
* Access to texture must be externally synchronized

```cpp
void DestroyDescriptor(Descriptor& descriptor)
```

Valid usage:
* The application must ensure that all command buffers which use descriptor sets which contain the descriptor have completed execution
* Descriptor sets must not reference the descriptor

Multithreading:
* Access to descriptor must be externally synchronized

```cpp
void DestroyPipelineLayout(PipelineLayout& pipelineLayout)
```

Valid usage:
* Pipelines which were created using the pipeline layout must be destroyed
* Descriptor sets which were allocated using the pipeline layout must be destroyed

Multithreading:
* Access to pipelineLayout must be externally synchronized

```cpp
void DestroyPipeline(Pipeline& pipeline)
```

Valid usage:
* The application must ensure that all command buffers which use the pipeline have completed execution

Multithreading:
* Access to pipeline must be externally synchronized

```cpp
void DestroyFrameBuffer(FrameBuffer& frameBuffer)
```

Valid usage:
* The application must ensure that all command buffers which use the framebuffer have completed execution

Multithreading:
* Access to frameBuffer must be externally synchronized

```cpp
void DestroyQueryPool(QueryPool& queryPool)
```

Valid usage:
* The application must ensure that all command buffers which use the query pool have completed execution

Multithreading:
* Access to queryPool must be externally synchronized

```cpp
void DestroyFence(Fence& fence)
```

Valid usage:
* The application must ensure that all GPU operations related to the fence have finished

Multithreading:
* Access to fence must be externally synchronized

```cpp
void DestroyCommandBuffer(CommandBuffer& commandBuffer)
```

Valid usage:
* The application must ensure that the command buffer has completed execution

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
Result AllocateMemory(Device& device, uint32_t physicalDeviceMask, MemoryType memoryType, uint64_t size, Memory*& memory)
```

Valid usage:
* memoryType must be a valid memory type received using GetBufferMemoryInfo or GetTextureMemoryInfo

Multithreading:
* No synchronization required

```cpp
Result BindBufferMemory(Device& device, const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
```

Multithreading:
* Access to buffers must be externally synchronized
* Access to memory instances must be externally synchronized

```cpp
Result BindTextureMemory(Device& device, const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
```

Multithreading:
* Access to textures must be externally synchronized
* Access to memory instances must be externally synchronized

```cpp
void FreeMemory(Memory& memory)
```

Valid usage:
* All buffers or textures which are bound to the memory must be destroyed.

Multithreading:
* Access to memory must be externally synchronized

```cpp
Result BeginCommandBuffer(CommandBuffer& commandBuffer, const DescriptorPool* descriptorPool, uint32_t physicalDeviceIndex)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
Result EndCommandBuffer(CommandBuffer& commandBuffer)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetPipeline(CommandBuffer& commandBuffer, const Pipeline& pipeline)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetPipelineLayout(CommandBuffer& commandBuffer, const PipelineLayout& pipelineLayout)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetDescriptorSet(CommandBuffer& commandBuffer, uint32_t setIndexInPipelineLayout, const DescriptorSet& descriptorSet, const uint32_t* dynamicConstantBufferOffsets)
```

Valid usage:
* A compatible pipeline layout must be set
* The number of offsets in dynamicConstantBufferOffsets must be equal to the total number of dynamic constant buffers in descriptorSet

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetConstants(CommandBuffer& commandBuffer, uint32_t pushConstantIndex, const void* data, uint32_t size)
```

Valid usage:
* A compatible pipeline layout must be set

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetDescriptorPool(CommandBuffer& commandBuffer, const DescriptorPool& descriptorPool)
```

**`Performance notes:`**
* Sets descriptor heap in D3D12, which in some cases might cause pipeline flush

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdPipelineBarrier(CommandBuffer& commandBuffer, const TransitionBarrierDesc* transitionBarriers, const AliasingBarrierDesc* aliasingBarriers, BarrierDependency dependency)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdBeginRenderPass(CommandBuffer& commandBuffer, const FrameBuffer& frameBuffer, FramebufferBindFlag renderPassBeginFlag)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdEndRenderPass(CommandBuffer& commandBuffer)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetViewports(CommandBuffer& commandBuffer, const Viewport* viewports, uint32_t viewportNum)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetScissors(CommandBuffer& commandBuffer, const Rect* rects, uint32_t rectNum)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetDepthBounds(CommandBuffer& commandBuffer, float boundsMin, float boundsMax)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetStencilReference(CommandBuffer& commandBuffer, uint8_t reference)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetSamplePositions(CommandBuffer& commandBuffer, const SamplePosition* positions, uint32_t positionNum)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdClearAttachments(CommandBuffer& commandBuffer, const ClearDesc* clearDescs, uint32_t clearDescNum, const Rect* rects, uint32_t rectNum)
```

Valid usage:
* Can be called only inside render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetIndexBuffer(CommandBuffer& commandBuffer, const Buffer& buffer, uint64_t offset, IndexType indexType)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdSetVertexBuffers(CommandBuffer& commandBuffer, uint32_t baseSlot, uint32_t bufferNum, const Buffer* const* buffers, const uint64_t* offsets)
```

Valid usage:
* CmdSetVertexBuffers uses vertex stream strides from the current pipeline

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDraw(CommandBuffer& commandBuffer, uint32_t vertexNum, uint32_t instanceNum, uint32_t baseVertex, uint32_t baseInstance)
```

Valid usage:
* Can be called only inside render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDrawIndexed(CommandBuffer& commandBuffer, uint32_t indexNum, uint32_t instanceNum, uint32_t baseIndex, uint32_t baseVertex, uint32_t baseInstance)
```

Valid usage:
* Can be called only inside render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDrawIndirect(CommandBuffer& commandBuffer, const Buffer& buffer, uint64_t offset, uint32_t drawNum, uint32_t stride)
```

Valid usage:
* Can be called only inside render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDrawIndexedIndirect(CommandBuffer& commandBuffer, const Buffer& buffer, uint64_t offset, uint32_t drawNum, uint32_t stride)
```

Valid usage:
* Can be called only inside render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDispatch(CommandBuffer& commandBuffer, uint32_t x, uint32_t y, uint32_t z)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDispatchIndirect(CommandBuffer& commandBuffer, const Buffer& buffer, uint64_t offset)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdBeginQuery(CommandBuffer& commandBuffer, const QueryPool& queryPool, uint32_t offset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdEndQuery(CommandBuffer& commandBuffer, const QueryPool& queryPool, uint32_t offset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdBeginAnnotation(CommandBuffer& commandBuffer, const char* name)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdEndAnnotation(CommandBuffer& commandBuffer)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdClearStorageBuffer(CommandBuffer& commandBuffer, const ClearStorageBufferDesc& clearDesc)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdClearStorageTexture(CommandBuffer& commandBuffer, const ClearStorageTextureDesc& clearDesc)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdCopyBuffer(CommandBuffer& commandBuffer, Buffer& dstBuffer, uint32_t dstPhysicalDeviceIndex, uint64_t dstOffset, const Buffer& srcBuffer, uint32_t srcPhysicalDeviceIndex, uint64_t srcOffset, uint64_t size)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdCopyTexture(CommandBuffer& commandBuffer, Texture& dstTexture, uint32_t dstPhysicalDeviceIndex, const TextureRegionDesc* dstRegionDesc, const Texture& srcTexture, uint32_t srcPhysicalDeviceIndex, const TextureRegionDesc* srcRegionDesc)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdUploadBufferToTexture(CommandBuffer& commandBuffer, Texture& dstTexture, const TextureRegionDesc& dstRegionDesc, const Buffer& srcBuffer, const TextureDataLayoutDesc& srcDataLayoutDesc)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdReadbackTextureToBuffer(CommandBuffer& commandBuffer, Buffer& dstBuffer, TextureDataLayoutDesc& dstDataLayoutDesc, const Texture& srcTexture, const TextureRegionDesc& srcRegionDesc)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdCopyQueries(CommandBuffer& commandBuffer, const QueryPool& queryPool, uint32_t offset, uint32_t num, Buffer& dstBuffer, uint64_t dstOffset)
```

Valid usage:
* Can be called only outside of a render pass

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void QueueSubmit(CommandQueue& commandQueue, const QueueSubmitDesc& queueSubmitDesc)
```

Multithreading:
* Access to commandQueue must be externally synchronized

```cpp
void QueueWait(CommandQueue& commandQueue, Fence& fence, uint64_t value)
```

Multithreading:
* Access to commandQueue must be externally synchronized

```cpp
void QueueSignal(CommandQueue& commandQueue, Fence& fence, uint64_t value)
```

Multithreading:
* Access to commandQueue must be externally synchronized

```cpp
uint64_t GetFenceValue(Fence& fence)
```

Multithreading:
* No synchronization required

```cpp
void Wait(Fence& fence, uint64_t value)
```

Multithreading:
* No synchronization required

```cpp
void UpdateDescriptorRanges(DescriptorSet& descriptorSet, uint32_t physicalDeviceMask, uint32_t baseRange, uint32_t rangeNum, const DescriptorRangeUpdateDesc* rangeUpdateDescs)
```

Multithreading:
* Access to descriptorSet must be externally synchronized

```cpp
void UpdateDynamicConstantBuffers(DescriptorSet& descriptorSet, uint32_t physicalDeviceMask, uint32_t baseBuffer, uint32_t bufferNum, const Descriptor* const* descriptors)
```

Multithreading:
* Access to descriptorSet must be externally synchronized

```cpp
void CopyDescriptorSet(DescriptorSet& descriptorSet, const DescriptorSetCopyDesc& descriptorSetCopyDesc)
```

Multithreading:
* Access to descriptorSet must be externally synchronized* Access to descriptorSetCopyDesc.srcDescriptorSet must be externally synchronized

```cpp
Result AllocateDescriptorSets(DescriptorPool& descriptorPool, const PipelineLayout& pipelineLayout, uint32_t setIndexInPipelineLayout, DescriptorSet** const descriptorSets, uint32_t instanceNum, uint32_t physicalDeviceMask, uint32_t variableDescriptorNum)
```
* `pipelineLayout`
  * The pipeline layout which describes shader resource binding
* `setIndexInPipelineLayout`
  * The index of the descriptor set in the pipeline layout
* `instanceNum`
  * The number of descriptor sets to allocate
* `variableDescriptorNum`
  * If the descriptor set in the pipeline layout uses variable descriptor number, the argument specifies the actual size of the descriptor range

Multithreading:
* Access to descriptorPool must be externally synchronized

```cpp
void ResetDescriptorPool(DescriptorPool& descriptorPool)
```

Destroys descriptor sets allocated from the descriptor pool

Valid usage:
* The application must ensure that all command buffers which use the descriptor pool and the descriptor sets completed execution

Multithreading:
* Access to descriptorPool must be externally synchronized

```cpp
void ResetCommandAllocator(CommandAllocator& commandAllocator)
```

Deallocates memory which was allocated for command recording, but does not destroy command buffers.

Valid usage:
* The application must ensure that all command buffers which were allocated using the command allocator completed execution

Multithreading:
* Access to commandAllocator must be externally synchronized

```cpp
uint32_t GetQuerySize(const QueryPool& queryPool)
```

Returns the size of the query in bytes

Multithreading:
* No synchronized required

```cpp
void GetBufferMemoryInfo(const Buffer& buffer, MemoryLocation memoryLocation, MemoryDesc& memoryDesc)
```

Returns memory requirements such as size, alignment and memory type.
* `memoryLocation`
  * Memory location for the buffer, it can be DEVICE, HOST_UPLOAD, or HOST_READBACK.

**`Performance notes:`**
* HOST_UPLOAD and HOST_READBACK both correspond to coherent and cached memory in Vulkan

Multithreading:
* No synchronized required

```cpp
void* MapBuffer(Buffer& buffer, uint64_t offset, uint64_t size)
```

Valid usage:
* The buffer must be bound to HOST_UPLOAD or HOST_READBACK memory
* The buffer must not already be mapped

Multithreading:
* Access to buffer must be externally synchronized

```cpp
void UnmapBuffer(Buffer& buffer)
```

Multithreading:
* Access to buffer must be externally synchronized

```cpp
void GetTextureMemoryInfo(const Texture& texture, MemoryLocation memoryLocation, MemoryDesc& memoryDesc)
```

Multithreading:
* Access to texture must be externally synchronized

```cpp
FormatSupportBits GetFormatSupport(const Device& device, Format format)
```

The function allows the application to check format support for different use cases.

Multithreading:
* No synchronized required

```cpp
void SetDeviceDebugName(Device& device, const char* name)
```

Multithreading:
* Access to device must be externally synchronized

```cpp
void SetCommandQueueDebugName(CommandQueue& commandQueue, const char* name)
```

Multithreading:
* Access to commandQueue must be externally synchronized

```cpp
void SetFenceDebugName(Fence& fence, const char* name)
```

Multithreading:
* Access to fence must be externally synchronized

```cpp
void SetCommandAllocatorDebugName(CommandAllocator& commandAllocator, const char* name)
```

Multithreading:
* Access to commandAllocator must be externally synchronized

```cpp
void SetDescriptorPoolDebugName(DescriptorPool& descriptorPool, const char* name)
```

Multithreading:
* Access to descriptorPool must be externally synchronized

```cpp
void SetBufferDebugName(Buffer& buffer, const char* name)
```

Multithreading:
* Access to buffer must be externally synchronized

```cpp
void SetTextureDebugName(Texture& texture, const char* name)
```

Multithreading:
* Access to texture must be externally synchronized

```cpp
void SetDescriptorDebugName(Descriptor& descriptor, const char* name)
```

Multithreading:
* Access to descriptor must be externally synchronized

```cpp
void SetPipelineLayoutDebugName(PipelineLayout& pipelineLayout, const char* name)
```

Multithreading:
* Access to pipelineLayout must be externally synchronized

```cpp
void SetPipelineDebugName(Pipeline& pipeline, const char* name)
```

Multithreading:
* Access to pipeline must be externally synchronized

```cpp
void SetFrameBufferDebugName(FrameBuffer& frameBuffer, const char* name)
```

Multithreading:
* Access to frameBuffer must be externally synchronized

```cpp
void SetQueryPoolDebugName(QueryPool& queryPool, const char* name)
```

Multithreading:
* Access to queryPool must be externally synchronized

```cpp
void SetDescriptorSetDebugName(DescriptorSet& descriptorSet, const char* name)
```

Multithreading:
* Access to descriptorSet must be externally synchronized

```cpp
void SetCommandBufferDebugName(CommandBuffer& commandBuffer, const char* name)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void SetMemoryDebugName(Memory& memory, const char* name)
```

Multithreading:
* Access to memory must be externally synchronized

# Swapchain Interface

```cpp
Result CreateSwapChain(Device& device, const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
```
* `swapChainDesc.commandQueue`
  * The command queue which will be presenting

Multithreading:
* No synchronization required

```cpp
void DestroySwapChain(SwapChain& swapChain)
```

Multithreading:
* Access to swapChain must be externally synchronized

```cpp
void SetSwapChainDebugName(SwapChain& swapChain, const char* name)
```

Multithreading:
* Access to swapChain must be externally synchronized

```cpp
Texture* const* GetSwapChainTextures(const SwapChain& swapChain, uint32_t& textureNum, Format& format)
```

Valid usage:
* To use the textures, the application must acquire access using AcquireNextSwapChainTexture()* textureNum may not match swapChainDesc.textureNum

Multithreading:
* No synchronization required

```cpp
uint32_t AcquireNextSwapChainTexture(SwapChain& swapChain)
```
Returns index of the acquired swapchain texture.

Multithreading:
* Access to swapChain must be externally synchronized

```cpp
Result SwapChainPresent(SwapChain& swapChain)
```

Multithreading:
* Access to swapChain must be externally synchronized

```cpp
Result SetSwapChainHdrMetadata(SwapChain& swapChain, const HdrMetadata& hdrMetadata)
```

Multithreading:
* Access to swapChain must be externally synchronized

# Ray Tracing Interface

```cpp
Result CreateRayTracingPipeline(Device& device, const RayTracingPipelineDesc& rayTracingPipelineDesc, Pipeline*& pipeline)
```

Multithreading:
* No synchronization required

```cpp
Result CreateAccelerationStructure(Device& device, const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
```

Multithreading:
* No synchronization required

```cpp
Result BindAccelerationStructureMemory(Device& device, const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
```

Multithreading:
* Access to the acceleration structure instances must be externally synchronized* Access to the memory instances must be externally synchronized

```cpp
Result CreateAccelerationStructureDescriptor(const AccelerationStructure& accelerationStructure, uint32_t physicalDeviceMask, Descriptor*& descriptor)
```

Multithreading:
* Access to accelerationStructure must be externally synchronized

```cpp
void SetAccelerationStructureDebugName(AccelerationStructure& accelerationStructure, const char* name)
```

Multithreading:
* Access to accelerationStructure must be externally synchronized

```cpp
void DestroyAccelerationStructure(AccelerationStructure& accelerationStructure)
```

Multithreading:
* Access to accelerationStructure must be externally synchronized

```cpp
void GetAccelerationStructureMemoryInfo(const AccelerationStructure& accelerationStructure, MemoryDesc& memoryDesc)
```

Multithreading:
* No synchronization required

```cpp
uint64_t GetAccelerationStructureUpdateScratchBufferSize(const AccelerationStructure& accelerationStructure)
```

Multithreading:
* No synchronization required

```cpp
uint64_t GetAccelerationStructureBuildScratchBufferSize(const AccelerationStructure& accelerationStructure)
```

Multithreading:
* No synchronization required

```cpp
uint64_t GetAccelerationStructureHandle(const AccelerationStructure& accelerationStructure, uint32_t physicalDeviceIndex)
```

Multithreading:
* No synchronization required

```cpp
Result WriteShaderGroupIdentifiers(const Pipeline& pipeline, uint32_t baseShaderGroupIndex, uint32_t shaderGroupNum, void* buffer)
```

Multithreading:
* No synchronization required

```cpp
void CmdBuildTopLevelAccelerationStructure(CommandBuffer& commandBuffer, uint32_t instanceNum, const Buffer& buffer, uint64_t bufferOffset, AccelerationStructureBuildBits flags, AccelerationStructure& dst, Buffer& scratch, uint64_t scratchOffset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdBuildBottomLevelAccelerationStructure(CommandBuffer& commandBuffer, uint32_t geometryObjectNum, const GeometryObject* geometryObjects, AccelerationStructureBuildBits flags, AccelerationStructure& dst, Buffer& scratch, uint64_t scratchOffset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdUpdateTopLevelAccelerationStructure(CommandBuffer& commandBuffer, uint32_t instanceNum, const Buffer& buffer, uint64_t bufferOffset, AccelerationStructureBuildBits flags, AccelerationStructure& dst, AccelerationStructure& src, Buffer& scratch, uint64_t scratchOffset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdUpdateBottomLevelAccelerationStructure(CommandBuffer& commandBuffer, uint32_t geometryObjectNum, const GeometryObject* geometryObjects, AccelerationStructureBuildBits flags, AccelerationStructure& dst, AccelerationStructure& src, Buffer& scratch, uint64_t scratchOffset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdCopyAccelerationStructure(CommandBuffer& commandBuffer, AccelerationStructure& dst, AccelerationStructure& src, CopyMode copyMode)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdWriteAccelerationStructureSize(CommandBuffer& commandBuffer, const AccelerationStructure* const* accelerationStructures, uint32_t accelerationStructureNum, QueryPool& queryPool, uint32_t queryPoolOffset)
```

Multithreading:
* Access to commandBuffer must be externally synchronized

```cpp
void CmdDispatchRays(CommandBuffer& commandBuffer, const DispatchRaysDesc& dispatchRaysDesc)
```

Multithreading:
* Access to commandBuffer must be externally synchronized
