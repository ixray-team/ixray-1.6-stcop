/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D12.h"
#include "FenceD3D12.h"
#include "CommandQueueD3D12.h"

using namespace nri;

Result FenceD3D12::Create(uint64_t initialValue)
{
    HRESULT hr = ((ID3D12Device*)m_Device)->CreateFence(initialValue, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_Fence));
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "ID3D12Device::CreateFence() - FAILED!");

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

inline uint64_t FenceD3D12::GetFenceValue() const
{
    return m_Fence->GetCompletedValue();
}

inline void FenceD3D12::QueueSignal(CommandQueueD3D12& commandQueue, uint64_t value)
{
    HRESULT hr = ((ID3D12CommandQueue*)commandQueue)->Signal(m_Fence, value);
    CHECK(m_Device.GetLog(), hr == S_OK, "ID3D12CommandQueue::Signal() - FAILED!");
}

inline void FenceD3D12::QueueWait(CommandQueueD3D12& commandQueue, uint64_t value)
{
    HRESULT hr = ((ID3D12CommandQueue*)commandQueue)->Wait(m_Fence, value);
    CHECK(m_Device.GetLog(), hr == S_OK, "ID3D12CommandQueue::Wait() - FAILED!");
}

inline void FenceD3D12::Wait(uint64_t value)
{
    // TODO: "busy wait" vs "wait for event"?
    while (m_Fence->GetCompletedValue() < value)
        ;
}

#include "FenceD3D12.hpp"
