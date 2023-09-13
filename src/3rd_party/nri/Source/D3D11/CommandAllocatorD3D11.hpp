/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static void NRI_CALL SetCommandAllocatorDebugName(CommandAllocator& commandAllocator, const char* name)
{
    ((CommandAllocatorD3D11&)commandAllocator).SetDebugName(name);
}

static Result NRI_CALL CreateCommandBuffer(CommandAllocator& commandAllocator, CommandBuffer*& commandBuffer)
{
    return ((CommandAllocatorD3D11&)commandAllocator).CreateCommandBuffer(commandBuffer);
}

static void NRI_CALL ResetCommandAllocator(CommandAllocator& commandAllocator)
{
    ((CommandAllocatorD3D11&)commandAllocator).Reset();
}

#pragma endregion

Define_Core_CommandAllocator_PartiallyFillFunctionTable(D3D11)