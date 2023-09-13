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

struct DeviceD3D11;
struct MemoryD3D11;

struct TextureD3D11
{
    inline TextureD3D11(DeviceD3D11& device) :
        m_Device(device)
    {}

    inline ~TextureD3D11()
    {}

    inline DeviceD3D11& GetDevice() const
    { return m_Device; }

    inline operator ID3D11Resource*() const
    { return m_Texture; }

    inline operator ID3D11Texture1D*() const
    { return (ID3D11Texture1D*)m_Texture.GetInterface(); }

    inline operator ID3D11Texture2D*() const
    { return (ID3D11Texture2D*)m_Texture.GetInterface(); }

    inline operator ID3D11Texture3D*() const
    { return (ID3D11Texture3D*)m_Texture.GetInterface(); }

    inline const TextureDesc& GetDesc() const
    { return m_Desc; }

    inline uint32_t GetSubresourceIndex(const TextureRegionDesc& regionDesc) const
    { return regionDesc.mipOffset + regionDesc.arrayOffset * m_Desc.mipNum; }

    inline uint16_t GetSize(uint32_t dimension, uint32_t mipOffset = 0) const
    {
        assert(dimension < 3);

        uint16_t size = m_Desc.size[dimension];
        size = (uint16_t)std::max(size >> mipOffset, 1);
        size = Align(size, dimension < 2 ? (uint16_t)GetTexelBlockWidth(m_Desc.format) : 1);

        return size;
    }

    TextureD3D11(DeviceD3D11& device, const TextureDesc& textureDesc);

    Result Create(const MemoryD3D11* memory);
    Result Create(const TextureD3D11Desc& textureDesc);

    uint32_t GetMipmappedSize(uint32_t w = 0, uint32_t h = 0, uint32_t d = 0, uint32_t mipOffset = 0, uint32_t mipNum = 0) const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_Texture, name); }

    void GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const;

private:
    DeviceD3D11& m_Device;
    ComPtr<ID3D11Resource> m_Texture;
    TextureDesc m_Desc = {};
};

}
