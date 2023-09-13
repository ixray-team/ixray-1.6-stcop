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
struct DescriptorSetDesc;

struct DescriptorSetVK
{
    inline DescriptorSetVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline VkDescriptorSet GetHandle(uint32_t physicalDeviceIndex) const
    { return m_Handles[physicalDeviceIndex]; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline uint32_t GetDynamicConstantBufferNum() const
    { return m_DynamicConstantBufferNum; }

    void Create(const VkDescriptorSet* handles, uint32_t physicalDeviceMask, const DescriptorSetDesc& setDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    void UpdateDescriptorRanges(uint32_t physicalDeviceMask, uint32_t rangeOffset, uint32_t rangeNum, const DescriptorRangeUpdateDesc* rangeUpdateDescs);
    void UpdateDynamicConstantBuffers(uint32_t physicalDeviceMask, uint32_t bufferOffset, uint32_t descriptorNum, const Descriptor* const* descriptors);
    void Copy(const DescriptorSetCopyDesc& descriptorSetCopyDesc);

private:
    DeviceVK& m_Device;
    std::array<VkDescriptorSet, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_Handles = {};
    const DescriptorSetDesc* m_SetDesc = nullptr;
    uint32_t m_DynamicConstantBufferNum = 0;
};

}