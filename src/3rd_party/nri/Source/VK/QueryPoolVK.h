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

struct QueryPoolVK
{
    inline QueryPoolVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline VkQueryPool GetHandle(uint32_t physicalDeviceIndex) const
    { return m_Handles[physicalDeviceIndex]; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline uint32_t GetStride() const
    { return m_Stride; }

    inline VkQueryType GetType() const
    { return m_Type; }

    ~QueryPoolVK();

    Result Create(const QueryPoolDesc& queryPoolDesc);
    Result Create(const QueryPoolVulkanDesc& queryPoolDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    uint32_t GetQuerySize() const;

private:
    DeviceVK& m_Device;
    std::array<VkQueryPool, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_Handles = {};
    uint32_t m_Stride = 0;
    VkQueryType m_Type = (VkQueryType)0;
    bool m_OwnsNativeObjects = false;
};

}