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

struct CommandAllocatorVK
{
    inline CommandAllocatorVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline operator VkCommandPool() const
    { return m_Handle; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    ~CommandAllocatorVK();

    Result Create(const CommandQueue& commandQueue, uint32_t physicalDeviceMask);
    Result Create(const CommandAllocatorVulkanDesc& commandAllocatorDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    Result CreateCommandBuffer(CommandBuffer*& commandBuffer);
    void Reset();

private:
    DeviceVK& m_Device;
    VkCommandPool m_Handle = VK_NULL_HANDLE;
    CommandQueueType m_Type = (CommandQueueType)0;
    bool m_OwnsNativeObjects = false;
};

}
