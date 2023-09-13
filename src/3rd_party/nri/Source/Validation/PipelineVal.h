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

struct PipelineVal : public DeviceObjectVal<Pipeline>
{
    PipelineVal(DeviceVal& device, Pipeline& pipeline);
    PipelineVal(DeviceVal& device, Pipeline& pipeline, const GraphicsPipelineDesc& graphicsPipelineDesc);
    PipelineVal(DeviceVal& device, Pipeline& pipeline, const ComputePipelineDesc& computePipelineDesc);
    PipelineVal(DeviceVal& device, Pipeline& pipeline, const RayTracingPipelineDesc& rayTracingPipelineDesc);

    inline const PipelineLayout* GetPipelineLayout() const
    { return m_PipelineLayout; }

    //================================================================================================================
    // NRI
    //================================================================================================================
    void SetDebugName(const char* name);
    Result WriteShaderGroupIdentifiers(uint32_t baseShaderGroupIndex, uint32_t shaderGroupNum, void* buffer);

private:
    const RayTracingInterface& m_RayTracingAPI;
    const PipelineLayout* m_PipelineLayout = nullptr;
};

}
