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

struct TextureVK
{
    inline TextureVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline VkImage GetHandle(uint32_t physicalDeviceIndex) const
    { return m_Handles[physicalDeviceIndex]; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline VkImageAspectFlags GetImageAspectFlags() const
    { return m_ImageAspectFlags; }

    inline const VkExtent3D& GetExtent() const
    { return m_Extent; }

    inline uint16_t GetMipNum() const
    { return m_MipNum; }

    inline uint16_t GetArraySize() const
    { return m_ArraySize; }

    inline Format GetFormat() const
    { return m_Format; }

    inline TextureType GetType() const
    { return m_TextureType; }

    inline VkSampleCountFlagBits GetSampleCount() const
    { return m_SampleCount; }

    inline void ClearHandle()
    {
        for (uint32_t i = 0; i < GetCountOf(m_Handles); i++)
            m_Handles[i] = VK_NULL_HANDLE;
    }

    inline uint16_t GetSize(uint32_t dimension, uint32_t mipOffset) const
    {
        assert(dimension < 3);

        uint16_t size = (uint16_t)((&m_Extent.width)[dimension]);
        size = (uint16_t)std::max(size >> mipOffset, 1);

        // TODO: VK doesn't require manual alignment, but probably we should use it here and during texture creation
        //size = Align( size, dimension < 2 ? (uint16_t)GetTexelBlockWidth(m_Format) : 1 );

        return size;
    }

    ~TextureVK();

    void Create(VkImage handle, VkImageAspectFlags aspectFlags, VkImageType imageType, const VkExtent3D& extent, Format format);
    Result Create(const TextureDesc& textureDesc);
    Result Create(const TextureVulkanDesc& textureDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    void GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const;

private:
    DeviceVK& m_Device;
    std::array<VkImage, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_Handles = {};
    VkImageAspectFlags m_ImageAspectFlags = (VkImageAspectFlags)0;
    VkExtent3D m_Extent = {};
    Format m_Format = nri::Format::UNKNOWN;
    TextureType m_TextureType = (TextureType)0;
    VkSampleCountFlagBits m_SampleCount = (VkSampleCountFlagBits)0;
    uint16_t m_MipNum = 0;
    uint16_t m_ArraySize = 0;
    bool m_OwnsNativeObjects = false;
};

}