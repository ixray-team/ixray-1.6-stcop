/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

namespace nri
{
    struct DeviceBase
    {
        inline DeviceBase(const Log& log, const StdAllocator<uint8_t>& stdAllocator) :
            m_Log(log),
            m_StdAllocator(stdAllocator)
        {}

        inline const Log& GetLog() const
        { return m_Log; }

        inline StdAllocator<uint8_t>& GetStdAllocator()
        { return m_StdAllocator; }

        virtual ~DeviceBase() {}
        virtual void Destroy() = 0;
        virtual Result FillFunctionTable(CoreInterface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(SwapChainInterface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(WrapperD3D11Interface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(WrapperD3D12Interface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(WrapperVKInterface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(RayTracingInterface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(MeshShaderInterface& table) const { table = {}; return Result::UNSUPPORTED; }
        virtual Result FillFunctionTable(HelperInterface& table) const { table = {}; return Result::UNSUPPORTED; }

    protected:
        Log m_Log;
        StdAllocator<uint8_t> m_StdAllocator;
    };
}

#define Declare_PartiallyFillFunctionTable_Functions(API) \
    void Core_Buffer_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_CommandAllocator_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_CommandBuffer_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_CommandQueue_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_Descriptor_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_DescriptorPool_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_DescriptorSet_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_Fence_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_QueryPool_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void Core_Texture_PartiallyFillFunctionTable##API(CoreInterface& table); \
    void SwapChain_PartiallyFillFunctionTable##API(SwapChainInterface& table); \
    void RayTracing_CommandBuffer_PartiallyFillFunctionTable##API(RayTracingInterface& table); \
    void RayTracing_AccelerationStructure_PartiallyFillFunctionTable##API(RayTracingInterface& table); \
    void MeshShader_CommandBuffer_PartiallyFillFunctionTable##API(MeshShaderInterface& table); \
    void Helper_CommandQueue_PartiallyFillFunctionTable##API(HelperInterface& table);

#define Define_Core_Buffer_PartiallyFillFunctionTable(API) \
void Core_Buffer_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetBufferDebugName = ::SetBufferDebugName; \
    table.GetBufferNativeObject = ::GetBufferNativeObject; \
    table.GetBufferMemoryInfo = ::GetBufferMemoryInfo; \
    table.MapBuffer = ::MapBuffer; \
    table.UnmapBuffer = ::UnmapBuffer; \
}

#define Define_Core_CommandAllocator_PartiallyFillFunctionTable(API) \
void Core_CommandAllocator_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetCommandAllocatorDebugName = ::SetCommandAllocatorDebugName; \
    table.CreateCommandBuffer = ::CreateCommandBuffer; \
    table.ResetCommandAllocator = ::ResetCommandAllocator; \
}

#define Define_Core_CommandBuffer_PartiallyFillFunctionTable(API) \
void Core_CommandBuffer_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.DestroyCommandBuffer = ::DestroyCommandBuffer; \
    table.BeginCommandBuffer = ::BeginCommandBuffer; \
    table.EndCommandBuffer = ::EndCommandBuffer; \
    table.CmdSetPipelineLayout = ::CmdSetPipelineLayout; \
    table.CmdSetPipeline = ::CmdSetPipeline; \
    table.CmdPipelineBarrier = ::CmdPipelineBarrier; \
    table.CmdSetDescriptorPool = ::CmdSetDescriptorPool; \
    table.CmdSetDescriptorSet = ::CmdSetDescriptorSet; \
    table.CmdSetConstants = ::CmdSetConstants; \
    table.CmdBeginRenderPass = ::CmdBeginRenderPass; \
    table.CmdEndRenderPass = ::CmdEndRenderPass; \
    table.CmdSetViewports = ::CmdSetViewports; \
    table.CmdSetScissors = ::CmdSetScissors; \
    table.CmdSetDepthBounds = ::CmdSetDepthBounds; \
    table.CmdSetStencilReference = ::CmdSetStencilReference; \
    table.CmdSetSamplePositions = ::CmdSetSamplePositions; \
    table.CmdClearAttachments = ::CmdClearAttachments; \
    table.CmdSetIndexBuffer = ::CmdSetIndexBuffer; \
    table.CmdSetVertexBuffers = ::CmdSetVertexBuffers; \
    table.CmdDraw = ::CmdDraw; \
    table.CmdDrawIndexed = ::CmdDrawIndexed; \
    table.CmdDrawIndirect = ::CmdDrawIndirect; \
    table.CmdDrawIndexedIndirect = ::CmdDrawIndexedIndirect; \
    table.CmdDispatch = ::CmdDispatch; \
    table.CmdDispatchIndirect = ::CmdDispatchIndirect; \
    table.CmdBeginQuery = ::CmdBeginQuery; \
    table.CmdEndQuery = ::CmdEndQuery; \
    table.CmdBeginAnnotation = ::CmdBeginAnnotation; \
    table.CmdEndAnnotation = ::CmdEndAnnotation; \
    table.CmdClearStorageBuffer = ::CmdClearStorageBuffer; \
    table.CmdClearStorageTexture = ::CmdClearStorageTexture; \
    table.CmdCopyBuffer = ::CmdCopyBuffer; \
    table.CmdCopyTexture = ::CmdCopyTexture; \
    table.CmdUploadBufferToTexture = ::CmdUploadBufferToTexture; \
    table.CmdReadbackTextureToBuffer = ::CmdReadbackTextureToBuffer; \
    table.CmdCopyQueries = ::CmdCopyQueries; \
    table.CmdResetQueries = ::CmdResetQueries; \
    table.SetCommandBufferDebugName = ::SetCommandBufferDebugName; \
    table.GetCommandBufferNativeObject = ::GetCommandBufferNativeObject; \
}

#define Define_Core_CommandQueue_PartiallyFillFunctionTable(API) \
void Core_CommandQueue_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetCommandQueueDebugName = ::SetCommandQueueDebugName; \
    table.QueueSubmit = ::QueueSubmit; \
}

#define Define_Core_Descriptor_PartiallyFillFunctionTable(API) \
void Core_Descriptor_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetDescriptorDebugName = ::SetDescriptorDebugName; \
    table.GetDescriptorNativeObject = ::GetDescriptorNativeObject; \
}

#define Define_Core_DescriptorPool_PartiallyFillFunctionTable(API) \
void Core_DescriptorPool_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetDescriptorPoolDebugName = ::SetDescriptorPoolDebugName; \
    table.AllocateDescriptorSets = ::AllocateDescriptorSets; \
    table.ResetDescriptorPool = ::ResetDescriptorPool; \
}

#define Define_Core_DescriptorSet_PartiallyFillFunctionTable(API) \
void Core_DescriptorSet_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetDescriptorSetDebugName = ::SetDescriptorSetDebugName; \
    table.UpdateDescriptorRanges = ::UpdateDescriptorRanges; \
    table.UpdateDynamicConstantBuffers = ::UpdateDynamicConstantBuffers; \
    table.CopyDescriptorSet = ::CopyDescriptorSet; \
}

#define Define_Core_Fence_PartiallyFillFunctionTable(API) \
void Core_Fence_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.GetFenceValue = ::GetFenceValue; \
    table.QueueSignal = ::QueueSignal; \
    table.QueueWait = ::QueueWait; \
    table.Wait = ::Wait; \
    table.SetFenceDebugName = ::SetFenceDebugName; \
}

#define Define_Core_QueryPool_PartiallyFillFunctionTable(API) \
void Core_QueryPool_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetQueryPoolDebugName = ::SetQueryPoolDebugName; \
    table.GetQuerySize = ::GetQuerySize; \
}

#define Define_Core_Texture_PartiallyFillFunctionTable(API) \
void Core_Texture_PartiallyFillFunctionTable##API(CoreInterface& table) \
{ \
    table.SetTextureDebugName = ::SetTextureDebugName; \
    table.GetTextureNativeObject = ::GetTextureNativeObject; \
    table.GetTextureMemoryInfo = ::GetTextureMemoryInfo; \
}

#define Define_SwapChain_PartiallyFillFunctionTable(API) \
void SwapChain_PartiallyFillFunctionTable##API(SwapChainInterface& table) \
{ \
    table.SetSwapChainDebugName = ::SetSwapChainDebugName; \
    table.GetSwapChainTextures = ::GetSwapChainTextures; \
    table.AcquireNextSwapChainTexture = ::AcquireNextSwapChainTexture; \
    table.SwapChainPresent = ::SwapChainPresent; \
    table.SetSwapChainHdrMetadata = ::SetSwapChainHdrMetadata; \
}

#define Define_RayTracing_CommandBuffer_PartiallyFillFunctionTable(API) \
void RayTracing_CommandBuffer_PartiallyFillFunctionTable##API(RayTracingInterface& table) \
{ \
    table.CmdBuildTopLevelAccelerationStructure = ::CmdBuildTopLevelAccelerationStructure; \
    table.CmdBuildBottomLevelAccelerationStructure = ::CmdBuildBottomLevelAccelerationStructure; \
    table.CmdUpdateTopLevelAccelerationStructure = ::CmdUpdateTopLevelAccelerationStructure; \
    table.CmdUpdateBottomLevelAccelerationStructure = ::CmdUpdateBottomLevelAccelerationStructure; \
    table.CmdCopyAccelerationStructure = ::CmdCopyAccelerationStructure; \
    table.CmdWriteAccelerationStructureSize = ::CmdWriteAccelerationStructureSize; \
    table.CmdDispatchRays = ::CmdDispatchRays; \
}

#define Define_RayTracing_AccelerationStructure_PartiallyFillFunctionTable(API) \
void RayTracing_AccelerationStructure_PartiallyFillFunctionTable##API(RayTracingInterface& table) \
{ \
    table.CreateAccelerationStructureDescriptor = ::CreateAccelerationStructureDescriptor; \
    table.GetAccelerationStructureMemoryInfo = ::GetAccelerationStructureMemoryInfo; \
    table.GetAccelerationStructureUpdateScratchBufferSize = ::GetAccelerationStructureUpdateScratchBufferSize; \
    table.GetAccelerationStructureBuildScratchBufferSize = ::GetAccelerationStructureBuildScratchBufferSize; \
    table.GetAccelerationStructureHandle = ::GetAccelerationStructureHandle; \
    table.SetAccelerationStructureDebugName = ::SetAccelerationStructureDebugName; \
    table.GetAccelerationStructureNativeObject = ::GetAccelerationStructureNativeObject; \
}

#define Define_MeshShader_CommandBuffer_PartiallyFillFunctionTable(API) \
void MeshShader_CommandBuffer_PartiallyFillFunctionTable##API(MeshShaderInterface& table) \
{ \
    table.CmdDispatchMeshTasks = ::CmdDispatchMeshTasks; \
}

#define Define_Helper_CommandQueue_PartiallyFillFunctionTable(API) \
void Helper_CommandQueue_PartiallyFillFunctionTable##API(HelperInterface& table) \
{ \
    table.ChangeResourceStates = ::ChangeResourceStates; \
    table.UploadData = ::UploadData; \
    table.WaitForIdle = ::WaitForIdle; \
}
