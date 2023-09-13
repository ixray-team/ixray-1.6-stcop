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
struct PipelineLayoutVK;

struct PipelineVK
{
    inline PipelineVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline operator VkPipeline() const
    { return m_Handle; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline VkPipelineBindPoint GetBindPoint() const
    { return m_BindPoint; }

    ~PipelineVK();

    Result Create(const GraphicsPipelineDesc& graphicsPipelineDesc);
    Result Create(const ComputePipelineDesc& computePipelineDesc);
    Result Create(const RayTracingPipelineDesc& rayTracingPipelineDesc);
    Result CreateGraphics(NRIVkPipeline vkPipeline);
    Result CreateCompute(NRIVkPipeline vkPipeline);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    Result WriteShaderGroupIdentifiers(uint32_t baseShaderGroupIndex, uint32_t shaderGroupNum, void* buffer) const;

private:
    Result SetupShaderStage(VkPipelineShaderStageCreateInfo& stage, const ShaderDesc& shaderDesc, VkShaderModule*& modules);
    Result CreateRenderPass(const OutputMergerDesc* outputMerger, const RasterizationDesc* rasterizationDesc);
    void FillVertexInputState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineVertexInputStateCreateInfo& state) const;
    void FillInputAssemblyState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineInputAssemblyStateCreateInfo& state) const;
    void FillTessellationState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineTessellationStateCreateInfo& state) const;
    void FillViewportState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineViewportStateCreateInfo& state);
    void FillRasterizationState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineRasterizationStateCreateInfo& state, VkPipelineRasterizationConservativeStateCreateInfoEXT& conservativeRasterState) const;
    void FillMultisampleState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineMultisampleStateCreateInfo& state) const;
    void FillDepthStencilState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineDepthStencilStateCreateInfo& state) const;
    void FillColorBlendState(const GraphicsPipelineDesc& graphicsPipelineDesc, VkPipelineColorBlendStateCreateInfo& state) const;
    void FillDynamicState(VkPipelineDynamicStateCreateInfo& state) const;
    void FillGroupIndices(const RayTracingPipelineDesc& rayTracingPipelineDesc, uint32_t* groupIndices);

private:
    DeviceVK& m_Device;
    VkPipeline m_Handle = VK_NULL_HANDLE;
    VkPipelineBindPoint m_BindPoint = (VkPipelineBindPoint)0;
    VkRenderPass m_RenderPass = VK_NULL_HANDLE;
    bool m_OwnsNativeObjects = false;
};

}