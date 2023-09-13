/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#define VULKAN_FUNCTION(name) PFN_vk##name name

struct DispatchTable
{
    VULKAN_FUNCTION(GetInstanceProcAddr);
    VULKAN_FUNCTION(CreateInstance);
    VULKAN_FUNCTION(EnumerateInstanceExtensionProperties);
    VULKAN_FUNCTION(EnumerateInstanceLayerProperties);

    VULKAN_FUNCTION(GetDeviceProcAddr);
    VULKAN_FUNCTION(DestroyInstance);
    VULKAN_FUNCTION(DestroyDevice);
    VULKAN_FUNCTION(GetPhysicalDeviceMemoryProperties);
    VULKAN_FUNCTION(GetDeviceGroupPeerMemoryFeatures);
    VULKAN_FUNCTION(GetPhysicalDeviceFormatProperties);
    VULKAN_FUNCTION(CreateDevice);
    VULKAN_FUNCTION(GetDeviceQueue);
    VULKAN_FUNCTION(EnumeratePhysicalDeviceGroups);
    VULKAN_FUNCTION(GetPhysicalDeviceProperties);
    VULKAN_FUNCTION(GetPhysicalDeviceProperties2);
    VULKAN_FUNCTION(GetPhysicalDeviceFeatures);
    VULKAN_FUNCTION(GetPhysicalDeviceFeatures2);
    VULKAN_FUNCTION(GetPhysicalDeviceQueueFamilyProperties);
    VULKAN_FUNCTION(EnumerateDeviceExtensionProperties);
    VULKAN_FUNCTION(GetPhysicalDeviceSurfaceFormatsKHR);
    VULKAN_FUNCTION(GetPhysicalDeviceSurfaceSupportKHR);
    VULKAN_FUNCTION(GetPhysicalDeviceSurfaceCapabilitiesKHR);
    VULKAN_FUNCTION(GetPhysicalDeviceSurfacePresentModesKHR);
    VULKAN_FUNCTION(DestroySurfaceKHR);

#if VK_USE_PLATFORM_WIN32_KHR
    VULKAN_FUNCTION(CreateWin32SurfaceKHR);
#endif
#if VK_USE_PLATFORM_METAL_EXT
    VULKAN_FUNCTION(CreateMetalSurfaceEXT);
#endif
#if VK_USE_PLATFORM_XLIB_KHR
    VULKAN_FUNCTION(CreateXlibSurfaceKHR);
#endif
#if VK_USE_PLATFORM_WAYLAND_KHR
    VULKAN_FUNCTION(CreateWaylandSurfaceKHR);
#endif

    VULKAN_FUNCTION(CreateBuffer);
    VULKAN_FUNCTION(CreateImage);
    VULKAN_FUNCTION(CreateBufferView);
    VULKAN_FUNCTION(CreateImageView);
    VULKAN_FUNCTION(CreateSampler);
    VULKAN_FUNCTION(CreateRenderPass);
    VULKAN_FUNCTION(CreateFramebuffer);
    VULKAN_FUNCTION(CreateQueryPool);
    VULKAN_FUNCTION(CreateCommandPool);
    VULKAN_FUNCTION(CreateSemaphore);
    VULKAN_FUNCTION(CreateDescriptorPool);
    VULKAN_FUNCTION(CreatePipelineLayout);
    VULKAN_FUNCTION(CreateDescriptorSetLayout);
    VULKAN_FUNCTION(CreateShaderModule);
    VULKAN_FUNCTION(CreateGraphicsPipelines);
    VULKAN_FUNCTION(CreateComputePipelines);
    VULKAN_FUNCTION(CreateSwapchainKHR);

    VULKAN_FUNCTION(DestroyBuffer);
    VULKAN_FUNCTION(DestroyImage);
    VULKAN_FUNCTION(DestroyBufferView);
    VULKAN_FUNCTION(DestroyImageView);
    VULKAN_FUNCTION(DestroySampler);
    VULKAN_FUNCTION(DestroyRenderPass);
    VULKAN_FUNCTION(DestroyFramebuffer);
    VULKAN_FUNCTION(DestroyQueryPool);
    VULKAN_FUNCTION(DestroyCommandPool);
    VULKAN_FUNCTION(DestroySemaphore);
    VULKAN_FUNCTION(DestroyDescriptorPool);
    VULKAN_FUNCTION(DestroyPipelineLayout);
    VULKAN_FUNCTION(DestroyDescriptorSetLayout);
    VULKAN_FUNCTION(DestroyShaderModule);
    VULKAN_FUNCTION(DestroyPipeline);
    VULKAN_FUNCTION(DestroySwapchainKHR);

    VULKAN_FUNCTION(AllocateMemory);
    VULKAN_FUNCTION(MapMemory);
    VULKAN_FUNCTION(UnmapMemory);
    VULKAN_FUNCTION(FreeMemory);
    VULKAN_FUNCTION(BindBufferMemory2);
    VULKAN_FUNCTION(BindImageMemory2);

    VULKAN_FUNCTION(GetBufferMemoryRequirements2);
    VULKAN_FUNCTION(GetImageMemoryRequirements2);

    VULKAN_FUNCTION(QueueWaitIdle);
    VULKAN_FUNCTION(AcquireNextImageKHR);
    VULKAN_FUNCTION(QueueSubmit);
    VULKAN_FUNCTION(QueuePresentKHR);

    VULKAN_FUNCTION(GetSemaphoreCounterValue);
    VULKAN_FUNCTION(WaitSemaphores);

    VULKAN_FUNCTION(ResetCommandPool);
    VULKAN_FUNCTION(ResetDescriptorPool);
    VULKAN_FUNCTION(AllocateCommandBuffers);
    VULKAN_FUNCTION(AllocateDescriptorSets);
    VULKAN_FUNCTION(FreeCommandBuffers);
    VULKAN_FUNCTION(FreeDescriptorSets);
    VULKAN_FUNCTION(UpdateDescriptorSets);

    VULKAN_FUNCTION(BeginCommandBuffer);
    VULKAN_FUNCTION(CmdSetDepthBounds);
    VULKAN_FUNCTION(CmdSetViewport);
    VULKAN_FUNCTION(CmdSetScissor);
    VULKAN_FUNCTION(CmdSetStencilReference);
    VULKAN_FUNCTION(CmdClearAttachments);
    VULKAN_FUNCTION(CmdClearColorImage);
    VULKAN_FUNCTION(CmdBeginRenderPass);
    VULKAN_FUNCTION(CmdBindVertexBuffers);
    VULKAN_FUNCTION(CmdBindIndexBuffer);
    VULKAN_FUNCTION(CmdBindPipeline);
    VULKAN_FUNCTION(CmdBindDescriptorSets);
    VULKAN_FUNCTION(CmdPushConstants);
    VULKAN_FUNCTION(CmdDispatch);
    VULKAN_FUNCTION(CmdDispatchIndirect);
    VULKAN_FUNCTION(CmdDraw);
    VULKAN_FUNCTION(CmdDrawIndexed);
    VULKAN_FUNCTION(CmdDrawIndirect);
    VULKAN_FUNCTION(CmdDrawIndexedIndirect);
    VULKAN_FUNCTION(CmdCopyBuffer);
    VULKAN_FUNCTION(CmdCopyImage);
    VULKAN_FUNCTION(CmdCopyBufferToImage);
    VULKAN_FUNCTION(CmdCopyImageToBuffer);
    VULKAN_FUNCTION(CmdPipelineBarrier);
    VULKAN_FUNCTION(CmdBeginQuery);
    VULKAN_FUNCTION(CmdEndQuery);
    VULKAN_FUNCTION(CmdWriteTimestamp);
    VULKAN_FUNCTION(CmdCopyQueryPoolResults);
    VULKAN_FUNCTION(CmdResetQueryPool);
    VULKAN_FUNCTION(CmdEndRenderPass);
    VULKAN_FUNCTION(CmdFillBuffer);
    VULKAN_FUNCTION(EndCommandBuffer);

    VULKAN_FUNCTION(GetSwapchainImagesKHR);

    VULKAN_FUNCTION(SetDebugUtilsObjectNameEXT);
    VULKAN_FUNCTION(CmdBeginDebugUtilsLabelEXT);
    VULKAN_FUNCTION(CmdEndDebugUtilsLabelEXT);

    VULKAN_FUNCTION(SetHdrMetadataEXT);

    VULKAN_FUNCTION(CreateAccelerationStructureKHR);
    VULKAN_FUNCTION(CreateRayTracingPipelinesKHR);
    VULKAN_FUNCTION(DestroyAccelerationStructureKHR);
    VULKAN_FUNCTION(GetAccelerationStructureDeviceAddressKHR);
    VULKAN_FUNCTION(GetAccelerationStructureBuildSizesKHR);
    VULKAN_FUNCTION(GetRayTracingShaderGroupHandlesKHR);
    VULKAN_FUNCTION(CmdBuildAccelerationStructuresKHR);
    VULKAN_FUNCTION(CmdCopyAccelerationStructureKHR);
    VULKAN_FUNCTION(CmdWriteAccelerationStructuresPropertiesKHR);
    VULKAN_FUNCTION(CmdTraceRaysKHR);
    VULKAN_FUNCTION(GetBufferDeviceAddress);

    VULKAN_FUNCTION(CmdDrawMeshTasksEXT);
};

#undef VULKAN_FUNCTION
