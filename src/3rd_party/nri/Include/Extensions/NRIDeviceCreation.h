/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

NRI_NAMESPACE_BEGIN

NRI_ENUM
(
    Message, MESSAGE, uint8_t,

    TYPE_INFO,
    TYPE_WARNING,
    TYPE_ERROR,

    MAX_NUM,
);

NRI_STRUCT(MemoryAllocatorInterface)
{
    void* (*Allocate)(void* userArg, size_t size, size_t alignment);
    void* (*Reallocate)(void* userArg, void* memory, size_t size, size_t alignment);
    void (*Free)(void* userArg, void* memory);
    void* userArg;
};

NRI_STRUCT(CallbackInterface)
{
    void (*MessageCallback)(void* userArg, const char* message, NRI_NAME(Message) messageType);
    void (*AbortExecution)(void* userArg);
    void* userArg;
};

NRI_STRUCT(PhysicalDeviceGroup)
{
    char description[128];
    uint64_t luid;
    uint64_t dedicatedVideoMemory;
    uint32_t deviceID;
    NRI_NAME(Vendor) vendor;
};

NRI_STRUCT(VulkanExtensions)
{
    const char* const* instanceExtensions;
    uint32_t instanceExtensionNum;
    const char* const* deviceExtensions;
    uint32_t deviceExtensionNum;
};

NRI_STRUCT(DeviceCreationDesc)
{
    const NRI_NAME(PhysicalDeviceGroup)* physicalDeviceGroup;
    NRI_NAME(CallbackInterface) callbackInterface;
    NRI_NAME(MemoryAllocatorInterface) memoryAllocatorInterface;
    NRI_NAME(GraphicsAPI) graphicsAPI;
    NRI_NAME(SPIRVBindingOffsets) spirvBindingOffsets;
    NRI_NAME(VulkanExtensions) vulkanExtensions;
    bool enableNRIValidation;
    bool enableAPIValidation;
    bool enableMGPU;
    bool D3D11CommandBufferEmulation;
    bool skipLiveObjectsReporting;
};

#if defined(NRI_CPP)
    NRI_API Result NRI_CALL GetPhysicalDevices(PhysicalDeviceGroup* physicalDeviceGroups, uint32_t& physicalDeviceGroupNum);
    NRI_API Result NRI_CALL CreateDevice(const DeviceCreationDesc& deviceCreationDesc, Device*& device);
    NRI_API void NRI_CALL DestroyDevice(Device& device);
#endif

NRI_NAMESPACE_END

NRIC_API uint8_t NRI_CALL nri_GetPhysicalDevices(void* physicalDeviceGroups, uint32_t* physicalDeviceGroupNum);
NRIC_API uint8_t NRI_CALL nri_CreateDevice(const void* deviceCreationDesc, void** device);
NRIC_API void NRI_CALL nri_DestroyDevice(void* device);
