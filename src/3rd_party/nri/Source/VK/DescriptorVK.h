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
struct TextureVK;

enum class DescriptorTypeVK
{
    NONE = 0,
    BUFFER_VIEW,
    IMAGE_VIEW,
    SAMPLER,
    ACCELERATION_STRUCTURE
};

struct DescriptorBufferDesc
{
    std::array<VkBuffer, PHYSICAL_DEVICE_GROUP_MAX_SIZE> handles;
    uint64_t offset;
    uint64_t size;
};

struct DescriptorTextureDesc
{
    std::array<VkImage, PHYSICAL_DEVICE_GROUP_MAX_SIZE> handles;
    const TextureVK* texture;
    VkImageLayout imageLayout;
    uint32_t imageMipOffset;
    uint32_t imageMipNum;
    uint32_t imageArrayOffset;
    uint32_t imageArraySize;
    VkImageAspectFlags imageAspectFlags;
};

struct DescriptorVK
{
    inline DescriptorVK(DeviceVK& device) :
        m_Device(device)
    { m_BufferViews.fill(VK_NULL_HANDLE); }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline VkBufferView GetBufferView(uint32_t physicalDeviceIndex) const
    { return m_BufferViews[physicalDeviceIndex]; }

    inline VkImageView GetImageView(uint32_t physicalDeviceIndex) const
    { return m_ImageViews[physicalDeviceIndex]; }

    inline const VkSampler& GetSampler() const
    { return m_Sampler; }

    inline VkAccelerationStructureKHR GetAccelerationStructure(uint32_t physicalDeviceIndex) const
    { return m_AccelerationStructures[physicalDeviceIndex]; }

    inline VkBuffer GetBuffer(uint32_t physicalDeviceIndex) const
    { return m_BufferDesc.handles[physicalDeviceIndex]; }

    inline VkImage GetImage(uint32_t physicalDeviceIndex) const
    { return m_TextureDesc.handles[physicalDeviceIndex]; }

    inline const TextureVK& GetTexture() const
    { return *m_TextureDesc.texture; }

    inline DescriptorTypeVK GetType() const
    { return m_Type; }

    inline VkFormat GetFormat() const
    { return m_Format; }

    inline VkImageLayout GetImageLayout() const
    { return m_TextureDesc.imageLayout; }

    inline const DescriptorTextureDesc& GetTextureDesc() const
    { return m_TextureDesc; }

    inline const DescriptorBufferDesc& GetBufferDesc() const
    { return m_BufferDesc; }

    inline void GetBufferInfo(uint32_t physicalDeviceIndex, VkDescriptorBufferInfo& info) const
    {
        info.buffer = m_BufferDesc.handles[physicalDeviceIndex];
        info.offset = m_BufferDesc.offset;
        info.range = m_BufferDesc.size;
    }

    inline void GetImageSubresourceRange(VkImageSubresourceRange& range) const
    {
        range.aspectMask = m_TextureDesc.imageAspectFlags;
        range.baseMipLevel = m_TextureDesc.imageMipOffset;
        range.levelCount = m_TextureDesc.imageMipNum;
        range.baseArrayLayer = m_TextureDesc.imageArrayOffset;
        range.layerCount = m_TextureDesc.imageArraySize;
    }

    ~DescriptorVK();

    Result Create(const BufferViewDesc& bufferViewDesc);
    Result Create(const Texture1DViewDesc& textureViewDesc);
    Result Create(const Texture2DViewDesc& textureViewDesc);
    Result Create(const Texture3DViewDesc& textureViewDesc);
    Result Create(const SamplerDesc& samplerDesc);
    Result Create(const VkAccelerationStructureKHR* accelerationStructures, uint32_t physicalDeviceMask);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);

private:
    template<typename T>
    Result CreateTextureView(const T& textureViewDesc);

private:
    DeviceVK& m_Device;
    union
    {
        std::array<VkBufferView, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_BufferViews;
        std::array<VkImageView, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_ImageViews;
        std::array<VkAccelerationStructureKHR, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_AccelerationStructures;
        VkSampler m_Sampler;
    };
    union
    {
        DescriptorBufferDesc m_BufferDesc;
        DescriptorTextureDesc m_TextureDesc = {};
    };
    DescriptorTypeVK m_Type = DescriptorTypeVK::NONE;
    VkFormat m_Format = VK_FORMAT_UNDEFINED;
};

}