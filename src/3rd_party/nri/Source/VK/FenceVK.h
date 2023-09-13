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
struct CommandQueueVK;

struct FenceVK
{
    inline FenceVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    ~FenceVK();

    Result Create(uint64_t initialValue);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    uint64_t GetFenceValue() const;
    void QueueSignal(CommandQueueVK& commandQueue, uint64_t value);
    void QueueWait(CommandQueueVK& commandQueue, uint64_t value);
    void Wait(uint64_t value);

private:
    DeviceVK& m_Device;
    VkSemaphore m_Fence = VK_NULL_HANDLE;
};

}
