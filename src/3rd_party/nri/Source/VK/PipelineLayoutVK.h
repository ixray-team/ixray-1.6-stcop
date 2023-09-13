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
struct DescriptorVK;

struct PushConstantRangeBindingDesc
{
    VkShaderStageFlags flags;
    uint32_t offset;
};

struct RuntimeBindingInfo
{
    RuntimeBindingInfo(StdAllocator<uint8_t>& allocator);

    Vector<bool> hasVariableDescriptorNum;
    Vector<DescriptorRangeDesc> descriptorSetRangeDescs;
    Vector<DynamicConstantBufferDesc> dynamicConstantBufferDescs;
    Vector<DescriptorSetDesc> descriptorSetDescs;
    Vector<PushConstantDesc> pushConstantDescs;
    Vector<PushConstantRangeBindingDesc> pushConstantBindings;
};

struct PipelineLayoutVK
{
    inline PipelineLayoutVK(DeviceVK& device) :
        m_Device(device)
        , m_RuntimeBindingInfo(device.GetStdAllocator())
        , m_DescriptorSetLayouts(device.GetStdAllocator())
        , m_DescriptorSetSpaces(device.GetStdAllocator())
    {}

    inline operator VkPipelineLayout() const
    { return m_Handle; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline const RuntimeBindingInfo& GetRuntimeBindingInfo() const
    { return m_RuntimeBindingInfo; }

    inline VkDescriptorSetLayout GetDescriptorSetLayout(uint32_t index) const
    { return m_DescriptorSetLayouts[index]; }

    inline VkPipelineBindPoint GetPipelineBindPoint() const
    { return m_PipelineBindPoint; }

    inline uint32_t GetDescriptorSetSpace(uint32_t setIndexInPipelineLayout) const
    { return m_DescriptorSetSpaces[setIndexInPipelineLayout]; }

    ~PipelineLayoutVK();

    Result Create(const PipelineLayoutDesc& pipelineLayoutDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);

private:
    void FillBindingOffsets(bool ignoreGlobalSPIRVOffsets, uint32_t* bindingOffsets);
    VkDescriptorSetLayout CreateSetLayout(const DescriptorSetDesc& descriptorSetDesc, const uint32_t* bindingOffsets);

    void FillDescriptorBindings(const DescriptorSetDesc& descriptorSetDesc, const uint32_t* bindingOffsets,
        VkDescriptorSetLayoutBinding*& bindings, VkDescriptorBindingFlagsEXT*& bindingFlags) const;
    void FillDynamicConstantBufferBindings(const DescriptorSetDesc& descriptorSetDesc, const uint32_t* bindingOffsets,
        VkDescriptorSetLayoutBinding*& bindings, VkDescriptorBindingFlagsEXT*& bindingFlags) const;

    void FillPushConstantRanges(const PipelineLayoutDesc& pipelineLayoutDesc, VkPushConstantRange* pushConstantRanges) const;
    void FillRuntimeBindingInfo(const PipelineLayoutDesc& pipelineLayoutDesc, const uint32_t* bindingOffsets);

private:
    DeviceVK& m_Device;
    VkPipelineLayout m_Handle = VK_NULL_HANDLE;
    VkPipelineBindPoint m_PipelineBindPoint = VK_PIPELINE_BIND_POINT_MAX_ENUM;
    RuntimeBindingInfo m_RuntimeBindingInfo;
    Vector<VkDescriptorSetLayout> m_DescriptorSetLayouts;
    Vector<uint32_t> m_DescriptorSetSpaces;
};

}