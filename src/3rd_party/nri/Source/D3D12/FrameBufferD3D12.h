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

struct FrameBufferD3D12
{
    inline FrameBufferD3D12(DeviceD3D12& device)
        : m_Device(device)
        , m_RenderTargets(device.GetStdAllocator())
        , m_ClearDescs(device.GetStdAllocator())
    {}

    inline ~FrameBufferD3D12()
    {}

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    Result Create(const FrameBufferDesc& frameBufferDesc);
    void Bind(ID3D12GraphicsCommandList* graphicsCommandList, RenderPassBeginFlag renderPassBeginFlag);
    void Clear(ID3D12GraphicsCommandList* graphicsCommandList, const ClearDesc* clearDescs, uint32_t clearDescNum, const Rect* rects, uint32_t rectNum);

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { MaybeUnused(name); }

private:
    DeviceD3D12& m_Device;
    Vector<D3D12_CPU_DESCRIPTOR_HANDLE> m_RenderTargets;
    D3D12_CPU_DESCRIPTOR_HANDLE m_DepthStencilTarget = {};
    Vector<ClearDesc> m_ClearDescs;
};

}
