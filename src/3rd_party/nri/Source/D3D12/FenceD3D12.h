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

struct DeviceD3D12;
struct CommandQueueD3D12;

struct FenceD3D12
{
    inline FenceD3D12(DeviceD3D12& device) :
        m_Device(device)
    {}

    inline ~FenceD3D12()
    {}

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    Result Create(uint64_t initialValue);

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_Fence, name); }

    uint64_t GetFenceValue() const;
    void QueueSignal(CommandQueueD3D12& commandQueue, uint64_t value);
    void QueueWait(CommandQueueD3D12& commandQueue, uint64_t value);
    void Wait(uint64_t value);

private:
    DeviceD3D12& m_Device;
    ComPtr<ID3D12Fence> m_Fence;
};

}
