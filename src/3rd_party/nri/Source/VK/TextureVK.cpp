/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedVK.h"
#include "TextureVK.h"
#include "CommandQueueVK.h"

using namespace nri;

TextureVK::~TextureVK()
{
    const auto& vk = m_Device.GetDispatchTable();

    if (!m_OwnsNativeObjects)
        return;

    for (uint32_t i = 0; i < m_Device.GetPhysicalDeviceGroupSize(); i++)
    {
        if (m_Handles[i] != VK_NULL_HANDLE)
            vk.DestroyImage(m_Device, m_Handles[i], m_Device.GetAllocationCallbacks());
    }
}

void TextureVK::Create(VkImage handle, VkImageAspectFlags aspectFlags, VkImageType imageType, const VkExtent3D& extent, Format format)
{
    m_OwnsNativeObjects = false;
    m_TextureType = GetTextureType(imageType);
    m_Handles[0] = handle;
    m_ImageAspectFlags = aspectFlags;
    m_Extent = extent;
    m_MipNum = 1;
    m_ArraySize = 1;
    m_Format = format;
    m_SampleCount = VK_SAMPLE_COUNT_1_BIT;
}

Result TextureVK::Create(const TextureDesc& textureDesc)
{
    m_OwnsNativeObjects = true;
    m_TextureType = textureDesc.type;
    m_Extent = { textureDesc.size[0], textureDesc.size[1], textureDesc.size[2] };
    m_MipNum = textureDesc.mipNum;
    m_ArraySize = textureDesc.arraySize;
    m_Format = textureDesc.format;
    m_SampleCount = ::GetSampleCount(textureDesc.sampleNum);

    const VkImageType imageType = ::GetImageType(textureDesc.type);

    const VkSharingMode sharingMode =
        m_Device.IsConcurrentSharingModeEnabledForImages() ? VK_SHARING_MODE_CONCURRENT : VK_SHARING_MODE_EXCLUSIVE;

    const Vector<uint32_t>& queueIndices = m_Device.GetConcurrentSharingModeQueueIndices();

    VkImageCreateInfo info = {};
    info.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    info.imageType = imageType;
    info.format = ::GetVkFormat(m_Format);
    info.extent.width = textureDesc.size[0];
    info.extent.height = textureDesc.size[1];
    info.extent.depth = textureDesc.size[2];
    info.mipLevels = textureDesc.mipNum;
    info.arrayLayers = textureDesc.arraySize;
    info.samples = m_SampleCount;
    info.tiling = VK_IMAGE_TILING_OPTIMAL;
    info.usage = GetImageUsageFlags(textureDesc.usageMask);
    info.sharingMode = sharingMode;
    info.queueFamilyIndexCount = (uint32_t)queueIndices.size();
    info.pQueueFamilyIndices = queueIndices.data();
    info.flags = GetImageCreateFlags(m_Format);

    m_ImageAspectFlags = ::GetImageAspectFlags(textureDesc.format);

    const auto& vk = m_Device.GetDispatchTable();

    uint32_t physicalDeviceMask = (textureDesc.physicalDeviceMask == WHOLE_DEVICE_GROUP) ? 0xff : textureDesc.physicalDeviceMask;

    for (uint32_t i = 0; i < m_Device.GetPhysicalDeviceGroupSize(); i++)
    {
        if ((1 << i) & physicalDeviceMask)
        {
            const VkResult result = vk.CreateImage(m_Device, &info, m_Device.GetAllocationCallbacks(), &m_Handles[i]);

            RETURN_ON_FAILURE(m_Device.GetLog(), result == VK_SUCCESS, GetReturnCode(result),
                "Can't create a texture: vkCreateImage returned %d.", (int32_t)result);
        }
    }

    return Result::SUCCESS;
}

Result TextureVK::Create(const TextureVulkanDesc& textureDesc)
{
    m_OwnsNativeObjects = false;
    m_Extent = { textureDesc.size[0], textureDesc.size[1], textureDesc.size[2] };
    m_MipNum = textureDesc.mipNum;
    m_ArraySize = textureDesc.arraySize;
    m_Format = VKFormatToNRIFormat((VkFormat)textureDesc.vkFormat);
    m_ImageAspectFlags = (VkImageAspectFlags)textureDesc.vkImageAspectFlags;
    m_TextureType = GetTextureType((VkImageType)textureDesc.vkImageType);
    m_SampleCount = (VkSampleCountFlagBits)textureDesc.sampleNum;

    const VkImage handle = (VkImage)textureDesc.vkImage;
    const uint32_t physicalDeviceMask = GetPhysicalDeviceGroupMask(textureDesc.physicalDeviceMask);

    for (uint32_t i = 0; i < m_Device.GetPhysicalDeviceGroupSize(); i++)
    {
        if ((1 << i) & physicalDeviceMask)
            m_Handles[i] = handle;
    }

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

inline void TextureVK::SetDebugName(const char* name)
{
    std::array<uint64_t, PHYSICAL_DEVICE_GROUP_MAX_SIZE> handles;
    for (size_t i = 0; i < handles.size(); i++)
        handles[i] = (uint64_t)m_Handles[i];

    m_Device.SetDebugNameToDeviceGroupObject(VK_OBJECT_TYPE_IMAGE, handles.data(), name);
}

void TextureVK::GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const
{
    VkImage handle = VK_NULL_HANDLE;
    for (uint32_t i = 0; i < m_Device.GetPhysicalDeviceGroupSize() && handle == VK_NULL_HANDLE; i++)
        handle = m_Handles[i];

    const auto& vk = m_Device.GetDispatchTable();

    VkMemoryDedicatedRequirements dedicatedRequirements = {
        VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS,
        nullptr
    };

    VkMemoryRequirements2 requirements = {
        VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
        &dedicatedRequirements
    };

    VkImageMemoryRequirementsInfo2 info = {
        VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
        nullptr,
        handle
    };

    vk.GetImageMemoryRequirements2(m_Device, &info, &requirements);

    memoryDesc.mustBeDedicated = dedicatedRequirements.prefersDedicatedAllocation ||
        dedicatedRequirements.requiresDedicatedAllocation;

    memoryDesc.alignment = (uint32_t)requirements.memoryRequirements.alignment;
    memoryDesc.size = requirements.memoryRequirements.size;

    MemoryTypeUnpack unpack = {};
    const bool found = m_Device.GetMemoryType(memoryLocation, requirements.memoryRequirements.memoryTypeBits, unpack.info);
    CHECK(m_Device.GetLog(), found, "Can't find suitable memory type: %d", requirements.memoryRequirements.memoryTypeBits);

    unpack.info.isDedicated = dedicatedRequirements.requiresDedicatedAllocation;

    memoryDesc.type = unpack.type;
}

#include "TextureVK.hpp"
