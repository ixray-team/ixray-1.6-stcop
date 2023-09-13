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

enum class ResourceType
{
    NONE,
    BUFFER,
    TEXTURE,
    SAMPLER,
    ACCELERATION_STRUCTURE
};

enum class ResourceViewType
{
    NONE,
    COLOR_ATTACHMENT,
    DEPTH_STENCIL_ATTACHMENT,
    SHADER_RESOURCE,
    SHADER_RESOURCE_STORAGE,
    CONSTANT_BUFFER_VIEW
};

struct DescriptorVal : public DeviceObjectVal<Descriptor>
{
    DescriptorVal(DeviceVal& device, Descriptor& descriptor, ResourceType resourceType);
    DescriptorVal(DeviceVal& device, Descriptor& descriptor, const BufferViewDesc& bufferViewDesc);
    DescriptorVal(DeviceVal& device, Descriptor& descriptor, const Texture1DViewDesc& textureViewDesc);
    DescriptorVal(DeviceVal& device, Descriptor& descriptor, const Texture2DViewDesc& textureViewDesc);
    DescriptorVal(DeviceVal& device, Descriptor& descriptor, const Texture3DViewDesc& textureViewDesc);
    DescriptorVal(DeviceVal& device, Descriptor& descriptor);

    inline uint64_t GetNativeObject(uint32_t physicalDeviceIndex) const
    { return m_CoreAPI.GetDescriptorNativeObject(m_ImplObject, physicalDeviceIndex); }

    inline bool IsBufferView() const
    { return m_ResourceType == ResourceType::BUFFER; }

    inline bool IsTextureView() const
    { return m_ResourceType == ResourceType::TEXTURE; }

    inline bool IsSampler() const
    { return m_ResourceType == ResourceType::SAMPLER; }

    inline bool IsAccelerationStructure() const
    { return m_ResourceType == ResourceType::ACCELERATION_STRUCTURE; }

    inline bool IsConstantBufferView() const
    { return m_ResourceType == ResourceType::BUFFER && m_ResourceViewType == ResourceViewType::CONSTANT_BUFFER_VIEW; }

    inline bool IsColorAttachment() const
    { return IsTextureView() && m_ResourceViewType == ResourceViewType::COLOR_ATTACHMENT; }

    inline bool IsDepthStencilAttachment() const
    { return IsTextureView() && m_ResourceViewType == ResourceViewType::DEPTH_STENCIL_ATTACHMENT; }

    inline bool IsShaderResource() const
    { return m_ResourceType != ResourceType::NONE && !IsSampler() && m_ResourceViewType == ResourceViewType::SHADER_RESOURCE; }

    inline bool IsShaderResourceStorage() const
    { return m_ResourceType != ResourceType::NONE && !IsSampler() && m_ResourceViewType == ResourceViewType::SHADER_RESOURCE_STORAGE; }

    //================================================================================================================
    // NRI
    //================================================================================================================
    void SetDebugName(const char* name);

private:
    ResourceType m_ResourceType = ResourceType::NONE;
    ResourceViewType m_ResourceViewType = ResourceViewType::NONE;
};

}
