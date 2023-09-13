/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static void NRI_CALL SetDescriptorPoolDebugName(DescriptorPool& descriptorPool, const char* name)
{
    ((DescriptorPoolD3D11&)descriptorPool).SetDebugName(name);
}

static Result NRI_CALL AllocateDescriptorSets(DescriptorPool& descriptorPool, const PipelineLayout& pipelineLayout, uint32_t setIndexInPipelineLayout,
    DescriptorSet** descriptorSets, uint32_t instanceNum, uint32_t physicalDeviceMask, uint32_t variableDescriptorNum)
{
    return ((DescriptorPoolD3D11&)descriptorPool).AllocateDescriptorSets(pipelineLayout, setIndexInPipelineLayout, descriptorSets, instanceNum,
        physicalDeviceMask, variableDescriptorNum);
}

static void NRI_CALL ResetDescriptorPool(DescriptorPool& descriptorPool)
{
    ((DescriptorPoolD3D11&)descriptorPool).Reset();
}

#pragma endregion

Define_Core_DescriptorPool_PartiallyFillFunctionTable(D3D11)
