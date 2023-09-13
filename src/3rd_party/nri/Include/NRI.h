/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include <stdint.h>
#include <stddef.h>

#define NRI_VERSION_MAJOR 1
#define NRI_VERSION_MINOR 95
#define NRI_VERSION_DATE "23 May 2023"
#define NRI_INTERFACE( name ) #name, sizeof(name)

#ifdef _WIN32
    #define NRI_CALL __fastcall
#else
    #define NRI_CALL
#endif

// C++ API
#ifndef NRI_API
    #if NRI_STATIC_LIBRARY
        #define NRI_API
    #else
        #define NRI_API extern "C"
    #endif
#endif

// C API
#ifndef NRIC_API
    #if NRI_STATIC_LIBRARY
        #define NRIC_API
    #elif( defined(__cplusplus) )
        #define NRIC_API extern "C"
    #else
        #define NRIC_API extern
    #endif
#endif

#include "NRIDescs.h"

NRIC_API uint8_t NRI_CALL nri_GetInterface(const void* device, const char* interfaceName, size_t interfaceSize, void* interfacePtr);

NRI_NAMESPACE_BEGIN

#if defined(NRI_CPP)
    NRI_API Result NRI_CALL GetInterface(const Device& device, const char* interfaceName, size_t interfaceSize, void* interfacePtr);
#endif

NRI_STRUCT(CoreInterface)
{
    // Get
    const NRI_REF_NAME(DeviceDesc) (NRI_CALL *GetDeviceDesc)(const NRI_REF_NAME(Device) device);
    NRI_NAME(FormatSupportBits) (NRI_CALL *GetFormatSupport)(const NRI_REF_NAME(Device) device, NRI_NAME(Format) format);
    uint32_t (NRI_CALL *GetQuerySize)(const NRI_REF_NAME(QueryPool) queryPool);
    void (NRI_CALL *GetBufferMemoryInfo)(const NRI_REF_NAME(Buffer) buffer, NRI_NAME(MemoryLocation) memoryLocation, NRI_REF_NAME(MemoryDesc) memoryDesc);
    void (NRI_CALL *GetTextureMemoryInfo)(const NRI_REF_NAME(Texture) texture, NRI_NAME(MemoryLocation) memoryLocation, NRI_REF_NAME(MemoryDesc) memoryDesc);
    NRI_NAME(Result) (NRI_CALL *GetCommandQueue)(NRI_REF_NAME(Device) device, NRI_NAME(CommandQueueType) commandQueueType, NRI_REF_NAME(CommandQueue*) commandQueue);

    // Create
    NRI_NAME(Result) (NRI_CALL *CreateCommandAllocator)(const NRI_REF_NAME(CommandQueue) commandQueue, uint32_t physicalDeviceMask, NRI_REF_NAME(CommandAllocator*) commandAllocator);
    NRI_NAME(Result) (NRI_CALL *CreateCommandBuffer)(NRI_REF_NAME(CommandAllocator) commandAllocator, NRI_REF_NAME(CommandBuffer*) commandBuffer);
    NRI_NAME(Result) (NRI_CALL *CreateDescriptorPool)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(DescriptorPoolDesc) descriptorPoolDesc, NRI_REF_NAME(DescriptorPool*) descriptorPool);
    NRI_NAME(Result) (NRI_CALL *CreateBuffer)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(BufferDesc) bufferDesc, NRI_REF_NAME(Buffer*) buffer);
    NRI_NAME(Result) (NRI_CALL *CreateTexture)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(TextureDesc) textureDesc, NRI_REF_NAME(Texture*) texture);
    NRI_NAME(Result) (NRI_CALL *CreateBufferView)(const NRI_REF_NAME(BufferViewDesc) bufferViewDesc, NRI_REF_NAME(Descriptor*) bufferView);
    NRI_NAME(Result) (NRI_CALL *CreateTexture1DView)(const NRI_REF_NAME(Texture1DViewDesc) textureViewDesc, NRI_REF_NAME(Descriptor*) textureView);
    NRI_NAME(Result) (NRI_CALL *CreateTexture2DView)(const NRI_REF_NAME(Texture2DViewDesc) textureViewDesc, NRI_REF_NAME(Descriptor*) textureView);
    NRI_NAME(Result) (NRI_CALL *CreateTexture3DView)(const NRI_REF_NAME(Texture3DViewDesc) textureViewDesc, NRI_REF_NAME(Descriptor*) textureView);
    NRI_NAME(Result) (NRI_CALL *CreateSampler)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(SamplerDesc) samplerDesc, NRI_REF_NAME(Descriptor*) sampler);
    NRI_NAME(Result) (NRI_CALL *CreatePipelineLayout)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(PipelineLayoutDesc) pipelineLayoutDesc, NRI_REF_NAME(PipelineLayout*) pipelineLayout);
    NRI_NAME(Result) (NRI_CALL *CreateGraphicsPipeline)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(GraphicsPipelineDesc) graphicsPipelineDesc, NRI_REF_NAME(Pipeline*) pipeline);
    NRI_NAME(Result) (NRI_CALL *CreateComputePipeline)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(ComputePipelineDesc) computePipelineDesc, NRI_REF_NAME(Pipeline*) pipeline);
    NRI_NAME(Result) (NRI_CALL *CreateFrameBuffer)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(FrameBufferDesc) frameBufferDesc, NRI_REF_NAME(FrameBuffer*) frameBuffer);
    NRI_NAME(Result) (NRI_CALL *CreateQueryPool)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(QueryPoolDesc) queryPoolDesc, NRI_REF_NAME(QueryPool*) queryPool);
    NRI_NAME(Result) (NRI_CALL *CreateFence)(NRI_REF_NAME(Device) device, uint64_t initialValue, NRI_REF_NAME(Fence*) fence);

    // Destroy
    void (NRI_CALL *DestroyCommandAllocator)(NRI_REF_NAME(CommandAllocator) commandAllocator);
    void (NRI_CALL *DestroyCommandBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer);
    void (NRI_CALL *DestroyDescriptorPool)(NRI_REF_NAME(DescriptorPool) descriptorPool);
    void (NRI_CALL *DestroyBuffer)(NRI_REF_NAME(Buffer) buffer);
    void (NRI_CALL *DestroyTexture)(NRI_REF_NAME(Texture) texture);
    void (NRI_CALL *DestroyDescriptor)(NRI_REF_NAME(Descriptor) descriptor);
    void (NRI_CALL *DestroyPipelineLayout)(NRI_REF_NAME(PipelineLayout) pipelineLayout);
    void (NRI_CALL *DestroyPipeline)(NRI_REF_NAME(Pipeline) pipeline);
    void (NRI_CALL *DestroyFrameBuffer)(NRI_REF_NAME(FrameBuffer) frameBuffer);
    void (NRI_CALL *DestroyQueryPool)(NRI_REF_NAME(QueryPool) queryPool);
    void (NRI_CALL *DestroyFence)(NRI_REF_NAME(Fence) fence);

    // Memory
    NRI_NAME(Result) (NRI_CALL *AllocateMemory)(NRI_REF_NAME(Device) device, uint32_t physicalDeviceMask, NRI_NAME(MemoryType) memoryType, uint64_t size, NRI_REF_NAME(Memory*) memory);
    NRI_NAME(Result) (NRI_CALL *BindBufferMemory)(NRI_REF_NAME(Device) device, const NRI_NAME(BufferMemoryBindingDesc)* memoryBindingDescs, uint32_t memoryBindingDescNum);
    NRI_NAME(Result) (NRI_CALL *BindTextureMemory)(NRI_REF_NAME(Device) device, const NRI_NAME(TextureMemoryBindingDesc)* memoryBindingDescs, uint32_t memoryBindingDescNum);
    void (NRI_CALL *FreeMemory)(NRI_REF_NAME(Memory) memory);

    // Command buffer commands
    NRI_NAME(Result) (NRI_CALL *BeginCommandBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_NAME(DescriptorPool)* descriptorPool, uint32_t physicalDeviceIndex);
    NRI_NAME(Result) (NRI_CALL *EndCommandBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer);

    void (NRI_CALL *CmdSetPipeline)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(Pipeline) pipeline);
    void (NRI_CALL *CmdSetPipelineLayout)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(PipelineLayout) pipelineLayout);
    void (NRI_CALL *CmdSetDescriptorSet)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t setIndexInPipelineLayout, const NRI_REF_NAME(DescriptorSet) descriptorSet, const uint32_t* dynamicConstantBufferOffsets);
    void (NRI_CALL *CmdSetConstants)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t pushConstantIndex, const void* data, uint32_t size);
    void (NRI_CALL *CmdSetDescriptorPool)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(DescriptorPool) descriptorPool);
    void (NRI_CALL *CmdPipelineBarrier)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_NAME(TransitionBarrierDesc)* transitionBarriers, const NRI_NAME(AliasingBarrierDesc)* aliasingBarriers, NRI_NAME(BarrierDependency) dependency);

    void (NRI_CALL *CmdBeginRenderPass)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(FrameBuffer) frameBuffer, NRI_NAME(RenderPassBeginFlag) renderPassBeginFlag);
    void (NRI_CALL *CmdEndRenderPass)(NRI_REF_NAME(CommandBuffer) commandBuffer);
    void (NRI_CALL *CmdSetViewports)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_NAME(Viewport)* viewports, uint32_t viewportNum);
    void (NRI_CALL *CmdSetScissors)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_NAME(Rect)* rects, uint32_t rectNum);
    void (NRI_CALL *CmdSetDepthBounds)(NRI_REF_NAME(CommandBuffer) commandBuffer, float boundsMin, float boundsMax);
    void (NRI_CALL *CmdSetStencilReference)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint8_t reference);
    void (NRI_CALL *CmdSetSamplePositions)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_NAME(SamplePosition)* positions, uint32_t positionNum);
    void (NRI_CALL *CmdClearAttachments)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_NAME(ClearDesc)* clearDescs, uint32_t clearDescNum, const NRI_NAME(Rect)* rects, uint32_t rectNum);
    void (NRI_CALL *CmdSetIndexBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(Buffer) buffer, uint64_t offset, NRI_NAME(IndexType) indexType);
    void (NRI_CALL *CmdSetVertexBuffers)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t baseSlot, uint32_t bufferNum, const NRI_NAME(Buffer)* const* buffers, const uint64_t* offsets);

    void (NRI_CALL *CmdDraw)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t vertexNum, uint32_t instanceNum, uint32_t baseVertex, uint32_t baseInstance);
    void (NRI_CALL *CmdDrawIndexed)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t indexNum, uint32_t instanceNum, uint32_t baseIndex, uint32_t baseVertex, uint32_t baseInstance);
    void (NRI_CALL *CmdDrawIndirect)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(Buffer) buffer, uint64_t offset, uint32_t drawNum, uint32_t stride);
    void (NRI_CALL *CmdDrawIndexedIndirect)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(Buffer) buffer, uint64_t offset, uint32_t drawNum, uint32_t stride);
    void (NRI_CALL *CmdDispatch)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t x, uint32_t y, uint32_t z);
    void (NRI_CALL *CmdDispatchIndirect)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(Buffer) buffer, uint64_t offset);
    void (NRI_CALL *CmdBeginQuery)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(QueryPool) queryPool, uint32_t offset);
    void (NRI_CALL *CmdEndQuery)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(QueryPool) queryPool, uint32_t offset);
    void (NRI_CALL *CmdBeginAnnotation)(NRI_REF_NAME(CommandBuffer) commandBuffer, const char* name);
    void (NRI_CALL *CmdEndAnnotation)(NRI_REF_NAME(CommandBuffer) commandBuffer);

    void (NRI_CALL *CmdClearStorageBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(ClearStorageBufferDesc) clearDesc);
    void (NRI_CALL *CmdClearStorageTexture)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(ClearStorageTextureDesc) clearDesc);
    void (NRI_CALL *CmdCopyBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer, NRI_REF_NAME(Buffer) dstBuffer, uint32_t dstPhysicalDeviceIndex, uint64_t dstOffset, const NRI_REF_NAME(Buffer) srcBuffer, uint32_t srcPhysicalDeviceIndex, uint64_t srcOffset, uint64_t size);
    void (NRI_CALL *CmdCopyTexture)(NRI_REF_NAME(CommandBuffer) commandBuffer, NRI_REF_NAME(Texture) dstTexture, uint32_t dstPhysicalDeviceIndex, const NRI_NAME(TextureRegionDesc)* dstRegionDesc, const NRI_REF_NAME(Texture) srcTexture, uint32_t srcPhysicalDeviceIndex, const NRI_NAME(TextureRegionDesc)* srcRegionDesc);
    void (NRI_CALL *CmdUploadBufferToTexture)(NRI_REF_NAME(CommandBuffer) commandBuffer, NRI_REF_NAME(Texture) dstTexture, const NRI_REF_NAME(TextureRegionDesc) dstRegionDesc, const NRI_REF_NAME(Buffer) srcBuffer, const NRI_REF_NAME(TextureDataLayoutDesc) srcDataLayoutDesc);
    void (NRI_CALL *CmdReadbackTextureToBuffer)(NRI_REF_NAME(CommandBuffer) commandBuffer, NRI_REF_NAME(Buffer) dstBuffer, NRI_REF_NAME(TextureDataLayoutDesc) dstDataLayoutDesc, const NRI_REF_NAME(Texture) srcTexture, const NRI_REF_NAME(TextureRegionDesc) srcRegionDesc);
    void (NRI_CALL *CmdCopyQueries)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(QueryPool) queryPool, uint32_t offset, uint32_t num, NRI_REF_NAME(Buffer) dstBuffer, uint64_t dstOffset);
    void (NRI_CALL *CmdResetQueries)(NRI_REF_NAME(CommandBuffer) commandBuffer, const NRI_REF_NAME(QueryPool) queryPool, uint32_t offset, uint32_t num);

    // Synchronization
    uint64_t (NRI_CALL *GetFenceValue)(NRI_REF_NAME(Fence) fence);
    void (NRI_CALL *QueueSignal)(NRI_REF_NAME(CommandQueue) commandQueue, NRI_REF_NAME(Fence) fence, uint64_t value);
    void (NRI_CALL *QueueWait)(NRI_REF_NAME(CommandQueue) commandQueue, NRI_REF_NAME(Fence) fence, uint64_t value);
    void (NRI_CALL *Wait)(NRI_REF_NAME(Fence) fence, uint64_t value);

    // Work submission
    void (NRI_CALL *QueueSubmit)(NRI_REF_NAME(CommandQueue) commandQueue, const NRI_REF_NAME(QueueSubmitDesc) queueSubmitDesc);

    // Descriptor set
    void (NRI_CALL *UpdateDescriptorRanges)(NRI_REF_NAME(DescriptorSet) descriptorSet, uint32_t physicalDeviceMask, uint32_t baseRange, uint32_t rangeNum, const NRI_NAME(DescriptorRangeUpdateDesc)* rangeUpdateDescs);
    void (NRI_CALL *UpdateDynamicConstantBuffers)(NRI_REF_NAME(DescriptorSet) descriptorSet, uint32_t physicalDeviceMask, uint32_t baseBuffer, uint32_t bufferNum, const NRI_NAME(Descriptor)* const* descriptors);
    void (NRI_CALL *CopyDescriptorSet)(NRI_REF_NAME(DescriptorSet) descriptorSet, const NRI_REF_NAME(DescriptorSetCopyDesc) descriptorSetCopyDesc);

    // Descriptor pool
    NRI_NAME(Result) (NRI_CALL *AllocateDescriptorSets)(NRI_REF_NAME(DescriptorPool) descriptorPool, const NRI_REF_NAME(PipelineLayout) pipelineLayout, uint32_t setIndexInPipelineLayout, NRI_NAME(DescriptorSet)** descriptorSets, uint32_t instanceNum, uint32_t physicalDeviceMask, uint32_t variableDescriptorNum);
    void (NRI_CALL *ResetDescriptorPool)(NRI_REF_NAME(DescriptorPool) descriptorPool);

    // Command allocator
    void (NRI_CALL *ResetCommandAllocator)(NRI_REF_NAME(CommandAllocator) commandAllocator);

    // Map / Unmap
    void* (NRI_CALL *MapBuffer)(NRI_REF_NAME(Buffer) buffer, uint64_t offset, uint64_t size);
    void (NRI_CALL *UnmapBuffer)(NRI_REF_NAME(Buffer) buffer);

    // Debug name
    void (NRI_CALL *SetDeviceDebugName)(NRI_REF_NAME(Device) device, const char* name);
    void (NRI_CALL *SetCommandQueueDebugName)(NRI_REF_NAME(CommandQueue) commandQueue, const char* name);
    void (NRI_CALL *SetFenceDebugName)(NRI_REF_NAME(Fence) fence, const char* name);
    void (NRI_CALL *SetCommandAllocatorDebugName)(NRI_REF_NAME(CommandAllocator) commandAllocator, const char* name);
    void (NRI_CALL *SetDescriptorPoolDebugName)(NRI_REF_NAME(DescriptorPool) descriptorPool, const char* name);
    void (NRI_CALL *SetBufferDebugName)(NRI_REF_NAME(Buffer) buffer, const char* name);
    void (NRI_CALL *SetTextureDebugName)(NRI_REF_NAME(Texture) texture, const char* name);
    void (NRI_CALL *SetDescriptorDebugName)(NRI_REF_NAME(Descriptor) descriptor, const char* name);
    void (NRI_CALL *SetPipelineLayoutDebugName)(NRI_REF_NAME(PipelineLayout) pipelineLayout, const char* name);
    void (NRI_CALL *SetPipelineDebugName)(NRI_REF_NAME(Pipeline) pipeline, const char* name);
    void (NRI_CALL *SetFrameBufferDebugName)(NRI_REF_NAME(FrameBuffer) frameBuffer, const char* name);
    void (NRI_CALL *SetQueryPoolDebugName)(NRI_REF_NAME(QueryPool) queryPool, const char* name);
    void (NRI_CALL *SetDescriptorSetDebugName)(NRI_REF_NAME(DescriptorSet) descriptorSet, const char* name);
    void (NRI_CALL *SetCommandBufferDebugName)(NRI_REF_NAME(CommandBuffer) commandBuffer, const char* name);
    void (NRI_CALL *SetMemoryDebugName)(NRI_REF_NAME(Memory) memory, const char* name);

    // Native objects
    void* (NRI_CALL *GetDeviceNativeObject)(const NRI_REF_NAME(Device) device); // ID3D11Device*, ID3D12Device* or VkDevice
    void* (NRI_CALL *GetCommandBufferNativeObject)(const NRI_REF_NAME(CommandBuffer) commandBuffer); // ID3D11DeviceContext*, ID3D12GraphicsCommandList* or VkCommandBuffer
    uint64_t (NRI_CALL *GetBufferNativeObject)(const NRI_REF_NAME(Buffer) buffer, uint32_t physicalDeviceIndex); // ID3D11Buffer*, ID3D12Resource* or VkBuffer
    uint64_t (NRI_CALL *GetTextureNativeObject)(const NRI_REF_NAME(Texture) texture, uint32_t physicalDeviceIndex); // ID3D11Resource*, ID3D12Resource* or VkImage
    uint64_t (NRI_CALL *GetDescriptorNativeObject)(const NRI_REF_NAME(Descriptor) descriptor, uint32_t physicalDeviceIndex); // ID3D11View*, D3D12_CPU_DESCRIPTOR_HANDLE or VkImageView/VkBufferView
};

NRI_NAMESPACE_END
