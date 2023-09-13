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
struct DescriptorSetVK;

struct DescriptorPoolVK
{
    inline DescriptorPoolVK(DeviceVK& device) :
        m_Device(device)
        , m_AllocatedSets(device.GetStdAllocator())
    { m_AllocatedSets.reserve(64); }

    inline operator VkDescriptorPool() const
    { return m_Handle; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    ~DescriptorPoolVK();

    Result Create(const DescriptorPoolDesc& descriptorPoolDesc);
    Result Create(NRIVkDescriptorPool vkDescriptorPool);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    void Reset();

    Result AllocateDescriptorSets(const PipelineLayout& pipelineLayout, uint32_t setIndexInPipelineLayout, DescriptorSet** descriptorSets,
        uint32_t numberOfCopies, uint32_t physicalDeviceMask, uint32_t variableDescriptorNum);

private:
    DeviceVK& m_Device;
    Vector<DescriptorSetVK*> m_AllocatedSets;
    VkDescriptorPool m_Handle = VK_NULL_HANDLE;
    uint32_t m_UsedSets = 0;
    bool m_OwnsNativeObjects = false;
};

}