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

struct CommandQueueVal;

struct FenceVal : public DeviceObjectVal<Fence>
{
    inline FenceVal(DeviceVal& device, Fence& queueSemaphore) : DeviceObjectVal(device, queueSemaphore)
    {}

    inline ~FenceVal()
    {}

    //================================================================================================================
    // NRI
    //================================================================================================================

    uint64_t GetFenceValue() const;
    void QueueSignal(CommandQueueVal& commandQueue, uint64_t value);
    void QueueWait(CommandQueueVal& commandQueue, uint64_t value);
    void Wait(uint64_t value);
    void SetDebugName(const char* name);
};

}
