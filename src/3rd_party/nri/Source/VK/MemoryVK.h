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

struct DeviceVK;
struct BufferVK;
struct TextureVK;

struct MemoryVK
{
    inline MemoryVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline VkDeviceMemory GetHandle(uint32_t physicalDeviceIndex) const
    { return m_Handles[physicalDeviceIndex]; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline MemoryType GetType() const
    { return m_Type; }

    inline uint8_t* GetMappedMemory(uint32_t physicalDeviceIndex) const
    { return m_MappedMemory[physicalDeviceIndex]; }

    ~MemoryVK();

    Result Create(uint32_t physicalDeviceMask, const MemoryType memoryType, uint64_t size);
    Result Create(const MemoryVulkanDesc& memoryDesc);
    Result CreateDedicated(BufferVK& buffer, uint32_t physicalDeviceMask);
    Result CreateDedicated(TextureVK& texture, uint32_t physicalDeviceMask);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);

private:
    DeviceVK& m_Device;
    std::array<VkDeviceMemory, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_Handles = {};
    std::array<uint8_t*, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_MappedMemory = {};
    MemoryType m_Type = std::numeric_limits<MemoryType>::max();
    bool m_OwnsNativeObjects = false;
};

}