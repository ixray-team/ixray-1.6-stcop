/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include "NRIDeviceCreation.h"

#if defined(NRI_CPP)
    struct VkImageSubresourceRange;
#else
    typedef struct VkImageSubresourceRange VkImageSubresourceRange;
#endif

NRI_NAMESPACE_BEGIN

NRI_FORWARD_STRUCT(AccelerationStructure);

typedef uint64_t NRIVkCommandPool;
typedef uint64_t NRIVkImage;
typedef uint64_t NRIVkBuffer;
typedef uint64_t NRIVkDeviceMemory;
typedef uint64_t NRIVkQueryPool;
typedef uint64_t NRIVkPipeline;
typedef uint64_t NRIVkDescriptorPool;
typedef uint64_t NRIVkImageView;
typedef uint64_t NRIVkBufferView;
typedef uint64_t NRIVkAccelerationStructureKHR;

typedef void* NRIVkInstance;
typedef void* NRIVkPhysicalDevice;
typedef void* NRIVkDevice;
typedef void* NRIVkQueue;
typedef void* NRIVkCommandBuffer;
typedef void* NRIVkProcAddress;

NRI_STRUCT(DeviceCreationVulkanDesc)
{
    NRIVkInstance vkInstance;
    NRIVkDevice vkDevice;
    const NRIVkPhysicalDevice* vkPhysicalDevices;
    uint32_t deviceGroupSize;
    const uint32_t* queueFamilyIndices;
    uint32_t queueFamilyIndexNum;
    NRI_NAME(CallbackInterface) callbackInterface;
    NRI_NAME(MemoryAllocatorInterface) memoryAllocatorInterface;
    NRI_NAME(SPIRVBindingOffsets) spirvBindingOffsets;
    const char* vulkanLoaderPath;
    bool enableNRIValidation;
    bool enableAPIValidation;
};

NRI_STRUCT(CommandQueueVulkanDesc)
{
    NRIVkQueue vkQueue;
    uint32_t familyIndex;
    NRI_NAME(CommandQueueType) commandQueueType;
};

NRI_STRUCT(CommandAllocatorVulkanDesc)
{
    NRIVkCommandPool vkCommandPool;
    NRI_NAME(CommandQueueType) commandQueueType;
};

NRI_STRUCT(CommandBufferVulkanDesc)
{
    NRIVkCommandBuffer vkCommandBuffer;
    NRI_NAME(CommandQueueType) commandQueueType;
};

NRI_STRUCT(BufferVulkanDesc)
{
    NRIVkBuffer vkBuffer;
    NRI_NAME(Memory)* memory;
    uint64_t bufferSize;
    uint64_t memoryOffset;
    uint64_t deviceAddress;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(TextureVulkanDesc)
{
    NRIVkImage vkImage;
    uint32_t vkFormat;
    uint32_t vkImageAspectFlags;
    uint32_t vkImageType;
    uint16_t size[3];
    uint16_t mipNum;
    uint16_t arraySize;
    uint8_t sampleNum;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(MemoryVulkanDesc)
{
    NRIVkDeviceMemory vkDeviceMemory;
    uint64_t size;
    uint32_t memoryTypeIndex;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(QueryPoolVulkanDesc)
{
    NRIVkQueryPool vkQueryPool;
    uint32_t vkQueryType;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(AccelerationStructureVulkanDesc)
{
    NRIVkAccelerationStructureKHR vkAccelerationStructure;
    uint64_t buildScratchSize;
    uint64_t updateScratchSize;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(WrapperVKInterface)
{
    NRI_NAME(Result) (NRI_CALL *CreateCommandQueueVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(CommandQueueVulkanDesc) commandQueueVulkanDesc, NRI_REF_NAME(CommandQueue*) commandQueue);
    NRI_NAME(Result) (NRI_CALL *CreateCommandAllocatorVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(CommandAllocatorVulkanDesc) commandAllocatorVulkanDesc, NRI_REF_NAME(CommandAllocator*) commandAllocator);
    NRI_NAME(Result) (NRI_CALL *CreateCommandBufferVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(CommandBufferVulkanDesc) commandBufferVulkanDesc, NRI_REF_NAME(CommandBuffer*) commandBuffer);
    NRI_NAME(Result) (NRI_CALL *CreateDescriptorPoolVK)(NRI_REF_NAME(Device) device, NRIVkDescriptorPool vkDescriptorPool, NRI_REF_NAME(DescriptorPool*) descriptorPool);
    NRI_NAME(Result) (NRI_CALL *CreateBufferVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(BufferVulkanDesc) bufferVulkanDesc, NRI_REF_NAME(Buffer*) buffer);
    NRI_NAME(Result) (NRI_CALL *CreateTextureVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(TextureVulkanDesc) textureVulkanDesc, NRI_REF_NAME(Texture*) texture);
    NRI_NAME(Result) (NRI_CALL *CreateMemoryVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(MemoryVulkanDesc) memoryVulkanDesc, NRI_REF_NAME(Memory*) memory);
    NRI_NAME(Result) (NRI_CALL *CreateGraphicsPipelineVK)(NRI_REF_NAME(Device) device, NRIVkPipeline vkPipeline, NRI_REF_NAME(Pipeline*) pipeline);
    NRI_NAME(Result) (NRI_CALL *CreateComputePipelineVK)(NRI_REF_NAME(Device) device, NRIVkPipeline vkPipeline, NRI_REF_NAME(Pipeline*) pipeline);
    NRI_NAME(Result) (NRI_CALL *CreateQueryPoolVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(QueryPoolVulkanDesc) queryPoolVulkanDesc, NRI_REF_NAME(QueryPool*) queryPool);
    NRI_NAME(Result) (NRI_CALL *CreateAccelerationStructureVK)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(AccelerationStructureVulkanDesc) accelerationStructureDesc, NRI_REF_NAME(AccelerationStructure*) accelerationStructure);

    NRIVkPhysicalDevice (NRI_CALL *GetVkPhysicalDevice)(const NRI_REF_NAME(Device) device);
    NRIVkInstance (NRI_CALL *GetVkInstance)(const NRI_REF_NAME(Device) device);
    NRIVkProcAddress(NRI_CALL *GetVkGetInstanceProcAddr)(const NRI_REF_NAME(Device) device);
    NRIVkProcAddress(NRI_CALL *GetVkGetDeviceProcAddr)(const NRI_REF_NAME(Device) device);
};

#if defined(NRI_CPP)
    NRI_API Result NRI_CALL CreateDeviceFromVkDevice(const DeviceCreationVulkanDesc& deviceDesc, Device*& device);
    NRI_API Format NRI_CALL ConvertVKFormatToNRI(uint32_t vkFormat);
    NRI_API uint32_t NRI_CALL ConvertNRIFormatToVK(Format format);
#endif

NRI_NAMESPACE_END

NRIC_API uint8_t NRI_CALL nri_CreateDeviceFromVkDevice(const void* deviceDesc, void** device);
NRIC_API uint8_t NRI_CALL nri_ConvertVKFormatToNRI(uint32_t vkFormat);
NRIC_API uint32_t NRI_CALL nri_ConvertNRIFormatToVK(uint8_t format);
