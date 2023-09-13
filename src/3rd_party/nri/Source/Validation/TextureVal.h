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

struct MemoryVal;

struct TextureVal : public DeviceObjectVal<Texture>
{
    TextureVal(DeviceVal& device, Texture& texture, const TextureDesc& textureDesc);
    TextureVal(DeviceVal& device, Texture& texture, const TextureD3D11Desc& textureD3D11Desc);
    TextureVal(DeviceVal& device, Texture& texture, const TextureD3D12Desc& textureD3D12Desc);
    TextureVal(DeviceVal& device, Texture& texture, const TextureVulkanDesc& textureVulkanDesc);
    ~TextureVal();

    inline const TextureDesc& GetDesc() const
    { return m_TextureDesc; }

    inline uint64_t GetNativeObject(uint32_t physicalDeviceIndex) const
    { return m_CoreAPI.GetTextureNativeObject(m_ImplObject, physicalDeviceIndex); }

    inline bool IsBoundToMemory() const
    { return m_IsBoundToMemory; }

    inline void SetBoundToMemory()
    { m_IsBoundToMemory = true; }

    inline void SetBoundToMemory(MemoryVal& memory)
    {
        m_Memory = &memory;
        m_IsBoundToMemory = true;
    }

    //================================================================================================================
    // NRI
    //================================================================================================================
    void SetDebugName(const char* name);
    void GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const;

private:
    MemoryVal* m_Memory = nullptr;
    bool m_IsBoundToMemory = false;
    TextureDesc m_TextureDesc = {};
};

}
