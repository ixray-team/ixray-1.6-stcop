/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include "SharedD3D12.h"

struct ID3D12Resource;

namespace nri
{

struct DeviceD3D12;
struct MemoryD3D12;

struct TextureD3D12
{
    inline TextureD3D12(DeviceD3D12& device)
        : m_Device(device)
    {}

    inline ~TextureD3D12()
    {}

    inline operator ID3D12Resource*() const
    { return m_Texture.GetInterface(); }

    inline const D3D12_RESOURCE_DESC& GetTextureDesc() const
    { return m_TextureDesc; }

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    inline uint32_t GetSubresourceIndex(uint32_t arrayOffset, uint32_t mipOffset) const
    { return arrayOffset * m_TextureDesc.MipLevels + mipOffset; }

    Result Create(const TextureDesc& textureDesc);
    Result Create(const TextureD3D12Desc& textureDesc);
    void Initialize(ID3D12Resource* resource);
    Result BindMemory(const MemoryD3D12* memory, uint64_t offset);
    uint16_t GetSize(uint32_t dim, uint32_t mipOffset = 0) const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_Texture, name); }

    void GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const;

private:
    DeviceD3D12& m_Device;
    D3D12_RESOURCE_DESC m_TextureDesc = {};
    ComPtr<ID3D12Resource> m_Texture;
    Format m_Format = Format::UNKNOWN;
};

}
