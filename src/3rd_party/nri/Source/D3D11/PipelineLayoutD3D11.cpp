/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D11.h"
#include "PipelineLayoutD3D11.h"
#include "DescriptorD3D11.h"
#include "DescriptorSetD3D11.h"

using namespace nri;

#define SET_CONSTANT_BUFFERS1(xy, stage) \
    if ( IsShaderVisible(bindingRange.shaderVisibility, stage) ) \
        deferredContext->xy##SetConstantBuffers1(bindingRange.baseSlot, bindingRange.descriptorNum, (ID3D11Buffer**)descriptors, constantFirst, constantNum)

#define SET_CONSTANT_BUFFERS(xy, stage) \
    if ( IsShaderVisible(bindingRange.shaderVisibility, stage) ) \
        deferredContext->xy##SetConstantBuffers(bindingRange.baseSlot, bindingRange.descriptorNum, (ID3D11Buffer**)descriptors)

#define SET_SHADER_RESOURCES(xy, stage) \
    if ( IsShaderVisible(bindingRange.shaderVisibility, stage) ) \
        deferredContext->xy##SetShaderResources(bindingRange.baseSlot, bindingRange.descriptorNum, (ID3D11ShaderResourceView**)descriptors)

#define SET_SAMPLERS(xy, stage) \
    if ( IsShaderVisible(bindingRange.shaderVisibility, stage) ) \
        deferredContext->xy##SetSamplers(bindingRange.baseSlot, bindingRange.descriptorNum, (ID3D11SamplerState**)descriptors)

#define SET_CONSTANT_BUFFER(xy, stage) \
    if ( IsShaderVisible(cb.shaderVisibility, stage) ) \
        deferredContext->xy##SetConstantBuffers(cb.slot, 1, (ID3D11Buffer**)&cb.buffer)

#define SET_SAMPLER(xy, stage) \
    if ( IsShaderVisible(ss.shaderVisibility, stage) ) \
        deferredContext->xy##SetSamplers(ss.slot, 1, (ID3D11SamplerState**)&ss.sampler)

// see StageSlots
constexpr std::array<DescriptorTypeDX11, (uint32_t)DescriptorType::MAX_NUM> g_RemapDescriptorTypeToIndex =
{
    DescriptorTypeDX11::SAMPLER,  // SAMPLER
    DescriptorTypeDX11::CONSTANT, // CONSTANT_BUFFER
    DescriptorTypeDX11::RESOURCE, // TEXTURE
    DescriptorTypeDX11::STORAGE,  // STORAGE_TEXTURE
    DescriptorTypeDX11::RESOURCE, // BUFFER
    DescriptorTypeDX11::STORAGE,  // STORAGE_BUFFER
    DescriptorTypeDX11::RESOURCE, // STRUCTURED_BUFFER
    DescriptorTypeDX11::STORAGE,  // STORAGE_STRUCTURED_BUFFER
    DescriptorTypeDX11::RESOURCE, // ACCELERATION_STRUCTURE
};

constexpr DescriptorTypeDX11 GetDescriptorTypeIndex(DescriptorType type)
{
    return g_RemapDescriptorTypeToIndex[(uint32_t)type];
}

constexpr bool IsShaderVisible(uint32_t shaderVisibility, ShaderStage stage)
{
    return shaderVisibility & (1 << (uint32_t)stage);
}

constexpr uint32_t GetShaderVisibility(ShaderStage visibility, PipelineLayoutShaderStageBits stageMask)
{
    // UAVs are visible from any stage on DX11.1, but can be bound to OM or compute
    //if (descriptorTypeIndex == DescriptorTypeDX11::STORAGE && !m_ComputeShader)
    //    visibility = ShaderStage::FRAGMENT;

    return (visibility == ShaderStage::ALL) ? (uint32_t)stageMask : (1 << (uint32_t)visibility) & (uint32_t)stageMask;
}

Result PipelineLayoutD3D11::Create(const PipelineLayoutDesc& pipelineLayoutDesc)
{
    m_IsGraphicsPipelineLayout = pipelineLayoutDesc.stageMask & PipelineLayoutShaderStageBits::ALL_GRAPHICS;

    BindingSet bindingSet = {};

    for (uint32_t i = 0; i < pipelineLayoutDesc.descriptorSetNum; i++)
    {
        const DescriptorSetDesc& set = pipelineLayoutDesc.descriptorSets[i];

        bindingSet.descriptorNum = 0;

        // Normal ranges
        for (uint32_t j = 0; j < set.rangeNum; j++)
        {
            const DescriptorRangeDesc& range = set.ranges[j];

            BindingRange bindingRange = {};
            bindingRange.baseSlot = range.baseRegisterIndex;
            bindingRange.descriptorOffset = bindingSet.descriptorNum;
            bindingRange.descriptorNum = range.descriptorNum;
            bindingRange.descriptorType = GetDescriptorTypeIndex(range.descriptorType);
            bindingRange.shaderVisibility = GetShaderVisibility(range.visibility, pipelineLayoutDesc.stageMask);
            m_BindingRanges.push_back(bindingRange);

            bindingSet.descriptorNum += bindingRange.descriptorNum;
        }

        // Dynamic constant buffers
        if (set.dynamicConstantBufferNum && m_Device.GetDevice().version == 0)
            REPORT_ERROR(m_Device.GetLog(), "Dynamic constant buffers with non-zero offsets require DX11.1+");

        for (uint32_t j = 0; j < set.dynamicConstantBufferNum; j++)
        {
            const DynamicConstantBufferDesc& cb = set.dynamicConstantBuffers[j];

            BindingRange bindingRange = {};
            bindingRange.baseSlot = cb.registerIndex;
            bindingRange.descriptorOffset = bindingSet.descriptorNum;
            bindingRange.descriptorNum = 1;
            bindingRange.descriptorType = DescriptorTypeDX11::DYNAMIC_CONSTANT;
            bindingRange.shaderVisibility = GetShaderVisibility(cb.visibility, pipelineLayoutDesc.stageMask);
            m_BindingRanges.push_back(bindingRange);

            bindingSet.descriptorNum += bindingRange.descriptorNum;
        }

        bindingSet.rangeEnd = bindingSet.rangeStart + set.rangeNum;
        bindingSet.rangeEnd += set.dynamicConstantBufferNum;

        m_BindingSets.push_back(bindingSet);

        bindingSet.rangeStart = bindingSet.rangeEnd;
    }

    // Push constants
    D3D11_BUFFER_DESC desc = {};
    desc.Usage = D3D11_USAGE_DEFAULT;
    desc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;

    for (uint32_t i = 0; i < pipelineLayoutDesc.pushConstantNum; i++)
    {
        const PushConstantDesc& pushConstant = pipelineLayoutDesc.pushConstants[i];

        ConstantBuffer cb = {};
        cb.shaderVisibility = GetShaderVisibility(pushConstant.visibility, pipelineLayoutDesc.stageMask);
        cb.slot = pushConstant.registerIndex;

        desc.ByteWidth = Align(pushConstant.size, 16);
        HRESULT hr = m_Device.GetDevice()->CreateBuffer(&desc, nullptr, &cb.buffer);
        if (FAILED(hr))
            REPORT_ERROR(m_Device.GetLog(), "Can't create a constant buffer for push constants! ID3D11Device::CreateBuffer() - FAILED!");

        m_ConstantBuffers.push_back(cb);
    }

    return Result::SUCCESS;
}

void PipelineLayoutD3D11::Bind(const VersionedContext& deferredContext)
{
    for (size_t i = 0; i < m_ConstantBuffers.size(); i++)
    {
        const ConstantBuffer& cb = m_ConstantBuffers[i];

        SET_CONSTANT_BUFFER(VS, ShaderStage::VERTEX);
        SET_CONSTANT_BUFFER(HS, ShaderStage::TESS_CONTROL);
        SET_CONSTANT_BUFFER(DS, ShaderStage::TESS_EVALUATION);
        SET_CONSTANT_BUFFER(GS, ShaderStage::GEOMETRY);
        SET_CONSTANT_BUFFER(PS, ShaderStage::FRAGMENT);
        SET_CONSTANT_BUFFER(CS, ShaderStage::COMPUTE);
    }
}

void PipelineLayoutD3D11::SetConstants(const VersionedContext& deferredContext, uint32_t pushConstantIndex, const Vec4* data, uint32_t size) const
{
    MaybeUnused(size);

    const ConstantBuffer& cb = m_ConstantBuffers[pushConstantIndex];
    deferredContext->UpdateSubresource(cb.buffer, 0, nullptr, data, 0, 0);
}

void PipelineLayoutD3D11::BindDescriptorSet(BindingState& currentBindingState, const VersionedContext& deferredContext,
    uint32_t setIndexInPipelineLayout, const DescriptorSetD3D11& descriptorSet, const uint32_t* dynamicConstantBufferOffsets) const
{
    if (m_IsGraphicsPipelineLayout)
        BindDescriptorSetImpl<true>(currentBindingState, deferredContext, setIndexInPipelineLayout, descriptorSet, dynamicConstantBufferOffsets);
    else
        BindDescriptorSetImpl<false>(currentBindingState, deferredContext, setIndexInPipelineLayout, descriptorSet, dynamicConstantBufferOffsets);
}

template<bool isGraphics>
void PipelineLayoutD3D11::BindDescriptorSetImpl(BindingState& currentBindingState, const VersionedContext& deferredContext, uint32_t setIndexInPipelineLayout,
    const DescriptorSetD3D11& descriptorSet, const uint32_t* dynamicConstantBufferOffsets) const
{
    const BindingSet& bindingSet = m_BindingSets[setIndexInPipelineLayout];
    bool isStorageRebindNeededInGraphics = false;

    uint8_t* memory = STACK_ALLOC(uint8_t, bindingSet.descriptorNum * (sizeof(void*) + sizeof(uint32_t) * 2));

    void** descriptors = (void**)memory;
    memory += bindingSet.descriptorNum * sizeof(void*);

    uint32_t* constantFirst = (uint32_t*)memory;
    memory += bindingSet.descriptorNum * sizeof(uint32_t);

    uint32_t* constantNum = (uint32_t*)memory;

    for (uint32_t j = bindingSet.rangeStart; j < bindingSet.rangeEnd; j++)
    {
        const BindingRange& bindingRange = m_BindingRanges[j];

        uint32_t hasNonZeroOffset = 0;
        uint32_t descriptorIndex = bindingRange.descriptorOffset;

        for (uint32_t i = 0; i < bindingRange.descriptorNum; i++)
        {
            const DescriptorD3D11* descriptor = descriptorSet.GetDescriptor(descriptorIndex++);

            if (descriptor)
            {
                descriptors[i] = *descriptor;

                if (bindingRange.descriptorType >= DescriptorTypeDX11::CONSTANT)
                {
                    uint32_t offset = descriptor->GetElementOffset();
                    if (bindingRange.descriptorType == DescriptorTypeDX11::DYNAMIC_CONSTANT)
                        offset += (*dynamicConstantBufferOffsets++) >> 4;
                    hasNonZeroOffset |= offset;

                    constantFirst[i] = offset;
                    constantNum[i] = descriptor->GetElementNum();
                }
                else if (bindingRange.descriptorType == DescriptorTypeDX11::STORAGE)
                    currentBindingState.TrackSubresource_UnbindIfNeeded_PostponeGraphicsStorageBinding(deferredContext, descriptor->GetSubresourceInfo(), *descriptor, bindingRange.baseSlot + i, isGraphics, true);
                else if (bindingRange.descriptorType == DescriptorTypeDX11::RESOURCE)
                    currentBindingState.TrackSubresource_UnbindIfNeeded_PostponeGraphicsStorageBinding(deferredContext, descriptor->GetSubresourceInfo(), *descriptor, bindingRange.baseSlot + i, isGraphics, false);
            }
            else
            {
                descriptors[i] = nullptr;
                constantFirst[i] = 0;
                constantNum[i] = 0;
            }
        }

        if (bindingRange.descriptorType >= DescriptorTypeDX11::CONSTANT)
        {
            if (hasNonZeroOffset)
            {
                if (isGraphics)
                {
                    SET_CONSTANT_BUFFERS1(VS, ShaderStage::VERTEX);
                    SET_CONSTANT_BUFFERS1(HS, ShaderStage::TESS_CONTROL);
                    SET_CONSTANT_BUFFERS1(DS, ShaderStage::TESS_EVALUATION);
                    SET_CONSTANT_BUFFERS1(GS, ShaderStage::GEOMETRY);
                    SET_CONSTANT_BUFFERS1(PS, ShaderStage::FRAGMENT);
                }
                else
                {
                    SET_CONSTANT_BUFFERS1(CS, ShaderStage::COMPUTE);
                }
            }
            else
            {
                if (isGraphics)
                {
                    SET_CONSTANT_BUFFERS(VS, ShaderStage::VERTEX);
                    SET_CONSTANT_BUFFERS(HS, ShaderStage::TESS_CONTROL);
                    SET_CONSTANT_BUFFERS(DS, ShaderStage::TESS_EVALUATION);
                    SET_CONSTANT_BUFFERS(GS, ShaderStage::GEOMETRY);
                    SET_CONSTANT_BUFFERS(PS, ShaderStage::FRAGMENT);
                }
                else
                {
                    SET_CONSTANT_BUFFERS(CS, ShaderStage::COMPUTE);
                }
            }
        }
        else if (bindingRange.descriptorType == DescriptorTypeDX11::RESOURCE)
        {
            if (isGraphics)
            {
                SET_SHADER_RESOURCES(VS, ShaderStage::VERTEX);
                SET_SHADER_RESOURCES(HS, ShaderStage::TESS_CONTROL);
                SET_SHADER_RESOURCES(DS, ShaderStage::TESS_EVALUATION);
                SET_SHADER_RESOURCES(GS, ShaderStage::GEOMETRY);
                SET_SHADER_RESOURCES(PS, ShaderStage::FRAGMENT);
            }
            else
            {
                SET_SHADER_RESOURCES(CS, ShaderStage::COMPUTE);
            }
        }
        else if (bindingRange.descriptorType == DescriptorTypeDX11::SAMPLER)
        {
            if (isGraphics)
            {
                SET_SAMPLERS(VS, ShaderStage::VERTEX);
                SET_SAMPLERS(HS, ShaderStage::TESS_CONTROL);
                SET_SAMPLERS(DS, ShaderStage::TESS_EVALUATION);
                SET_SAMPLERS(GS, ShaderStage::GEOMETRY);
                SET_SAMPLERS(PS, ShaderStage::FRAGMENT);
            }
            else
            {
                SET_SAMPLERS(CS, ShaderStage::COMPUTE);
            }
        }
        else if (bindingRange.descriptorType == DescriptorTypeDX11::STORAGE)
        {
            if (isGraphics)
                isStorageRebindNeededInGraphics = true;
            else
            {
                if (IsShaderVisible(bindingRange.shaderVisibility, ShaderStage::COMPUTE))
                    deferredContext->CSSetUnorderedAccessViews(bindingRange.baseSlot, bindingRange.descriptorNum, (ID3D11UnorderedAccessView**)descriptors, nullptr);
            }
        }
    }

    if (isStorageRebindNeededInGraphics)
    {
        uint32_t num = (uint32_t)currentBindingState.graphicsStorageDescriptors.size();
        ID3D11UnorderedAccessView** storages = currentBindingState.graphicsStorageDescriptors.data();

        deferredContext->OMSetRenderTargetsAndUnorderedAccessViews(D3D11_KEEP_RENDER_TARGETS_AND_DEPTH_STENCIL, nullptr, nullptr, 0, num, storages, nullptr);
    }
}
