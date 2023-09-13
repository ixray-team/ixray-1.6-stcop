/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include "DescriptorSetD3D12.h"

namespace nri
{

struct DeviceD3D12;

constexpr uint16_t ROOT_PARAMETER_UNUSED = uint16_t(-1);

struct DescriptorSetRootMapping
{
    inline DescriptorSetRootMapping(StdAllocator<uint8_t>& allocator)
        : rootOffsets(allocator)
    {}

    Vector<uint16_t> rootOffsets;
};

struct DynamicConstantBufferMapping
{
    uint16_t constantNum;
    uint16_t rootOffset;
};

struct PipelineLayoutD3D12
{
    PipelineLayoutD3D12(DeviceD3D12& device);

    inline operator ID3D12RootSignature*() const
    { return m_RootSignature.GetInterface(); }

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    inline bool IsGraphicsPipelineLayout() const
    { return m_IsGraphicsPipelineLayout; }

    inline const DescriptorSetMapping& GetDescriptorSetMapping(uint32_t setIndexInPipelineLayout) const
    { return m_DescriptorSetMappings[setIndexInPipelineLayout]; }

    inline const DescriptorSetRootMapping& GetDescriptorSetRootMapping(uint32_t setIndexInPipelineLayout) const
    { return m_DescriptorSetRootMappings[setIndexInPipelineLayout]; }

    inline const DynamicConstantBufferMapping& GetDynamicConstantBufferMapping(uint32_t setIndexInPipelineLayout) const
    { return m_DynamicConstantBufferMappings[setIndexInPipelineLayout]; }

    inline uint32_t GetPushConstantsRootOffset(uint32_t rangeIndex) const
    { return m_PushConstantsBaseIndex + rangeIndex; }

    Result Create(const PipelineLayoutDesc& pipelineLayoutDesc);

    void SetDescriptorSet(ID3D12GraphicsCommandList& graphicsCommandList, bool isGraphics,
        uint32_t setIndexInPipelineLayout, const DescriptorSet& descriptorSet, const uint32_t* dynamicConstantBufferOffsets) const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_RootSignature, name); }

private:
    template<bool isGraphics>
    void SetDescriptorSetImpl(ID3D12GraphicsCommandList& graphicsCommandList, uint32_t setIndexInPipelineLayout,
        const DescriptorSet& descriptorSet, const uint32_t* dynamicConstantBufferOffsets) const;

private:
    ComPtr<ID3D12RootSignature> m_RootSignature;
    bool m_IsGraphicsPipelineLayout = false;
    uint32_t m_PushConstantsBaseIndex = 0;
    Vector<DescriptorSetMapping> m_DescriptorSetMappings;
    Vector<DescriptorSetRootMapping> m_DescriptorSetRootMappings;
    Vector<DynamicConstantBufferMapping> m_DynamicConstantBufferMappings;
    DeviceD3D12& m_Device;
};

}