/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D11.h"
#include "FenceD3D11.h"

using namespace nri;

Result FenceD3D11::Create(uint64_t initialValue)
{
    HRESULT hr = E_INVALIDARG;
    const char* message = "";

    if (m_Device.GetDevice().version >= 5)
    {
        hr = m_Device.GetDevice()->CreateFence(initialValue, D3D11_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_Fence));
        message = "ID3D11Device5::CreateFence() - FAILED!";
    }
    else
    {
        D3D11_QUERY_DESC queryDesc = {};
        queryDesc.Query = D3D11_QUERY_EVENT;

        hr = m_Device.GetDevice()->CreateQuery(&queryDesc, &m_Query);
        message = "ID3D11Device::CreateQuery() - FAILED!";
    }

    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, message);

    m_Value = initialValue;

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

inline uint64_t FenceD3D11::GetFenceValue() const
{
    if (m_Fence)
        return m_Fence->GetCompletedValue();

    return m_Value;
}

inline void FenceD3D11::QueueSignal(CommandQueueD3D11& commandQueue, uint64_t value)
{
    MaybeUnused(commandQueue);

    if (m_Fence)
    {
        HRESULT hr = m_Device.GetImmediateContext()->Signal(m_Fence, value);
        CHECK(m_Device.GetLog(), hr == S_OK, "D3D11DeviceContext4::Signal() - FAILED!");
    }
    else
    {
        m_Device.GetImmediateContext()->End(m_Query);
        m_Value = value;
    }
}

inline void FenceD3D11::QueueWait(CommandQueueD3D11& commandQueue, uint64_t value)
{
    MaybeUnused(commandQueue);

    if (m_Fence)
    {
        HRESULT hr = m_Device.GetImmediateContext()->Wait(m_Fence, value);
        CHECK(m_Device.GetLog(), hr == S_OK, "D3D11DeviceContext4::Wait() - FAILED!");
    }
}

inline void FenceD3D11::Wait(uint64_t value)
{
    if (m_Fence)
    {
        // TODO: "busy wait" vs "wait for event"?
        while (m_Fence->GetCompletedValue() < value)
            ;
    }
    else
    {
        HRESULT hr = S_FALSE;
        while (hr == S_FALSE)
            hr = m_Device.GetImmediateContext()->GetData(m_Query, nullptr, 0, 0);

        CHECK(m_Device.GetLog(), hr == S_OK, "D3D11DeviceContext::GetData() - FAILED!");
    }
}

#include "FenceD3D11.hpp"
