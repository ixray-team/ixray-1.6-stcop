/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

NRI_NAMESPACE_BEGIN

NRI_STRUCT(MeshShaderInterface)
{
    void (NRI_CALL *CmdDispatchMeshTasks)(NRI_REF_NAME(CommandBuffer) commandBuffer, uint32_t x, uint32_t y, uint32_t z);
};

NRI_NAMESPACE_END
