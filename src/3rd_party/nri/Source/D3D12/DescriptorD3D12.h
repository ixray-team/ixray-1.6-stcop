/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

struct ID3D12Resource;

namespace nri
{

struct DeviceD3D12;

struct DescriptorD3D12
{
    inline DescriptorD3D12(DeviceD3D12& device)
        : m_Device(device)
    {}

    inline ~DescriptorD3D12()
    { m_Device.FreeDescriptorHandle(m_HeapType, m_Handle); }

    inline operator ID3D12Resource*() const
    { return m_Resource; }

    inline DescriptorPointerCPU GetPointerCPU() const
    { return m_DescriptorPointerCPU; }

    inline D3D12_GPU_VIRTUAL_ADDRESS GetBufferLocation() const
    { return m_BufferLocation; }

    inline bool IsFloatingPointUAV() const
    { return m_IsFloatingPointFormatUAV; }

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    Result Create(const BufferViewDesc& bufferViewDesc);
    Result Create(const Texture1DViewDesc& textureViewDesc);
    Result Create(const Texture2DViewDesc& textureViewDesc);
    Result Create(const Texture3DViewDesc& textureViewDesc);
    Result Create(const AccelerationStructure& accelerationStructure);
    Result Create(const SamplerDesc& samplerDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { MaybeUnused(name); }

private:
    Result CreateConstantBufferView(const D3D12_CONSTANT_BUFFER_VIEW_DESC& desc);
    Result CreateShaderResourceView(ID3D12Resource* resource, const D3D12_SHADER_RESOURCE_VIEW_DESC& desc);
    Result CreateUnorderedAccessView(ID3D12Resource* resource, const D3D12_UNORDERED_ACCESS_VIEW_DESC& desc, Format format);
    Result CreateRenderTargetView(ID3D12Resource* resource, const D3D12_RENDER_TARGET_VIEW_DESC& desc);
    Result CreateDepthStencilView(ID3D12Resource* resource, const D3D12_DEPTH_STENCIL_VIEW_DESC& desc);

private:
    DeviceD3D12& m_Device;
    ID3D12Resource* m_Resource = nullptr;
    D3D12_GPU_VIRTUAL_ADDRESS m_BufferLocation = 0;
    DescriptorHandle m_Handle = {};
    DescriptorPointerCPU m_DescriptorPointerCPU = {};
    D3D12_DESCRIPTOR_HEAP_TYPE m_HeapType = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    bool m_IsFloatingPointFormatUAV = false;
};

}
