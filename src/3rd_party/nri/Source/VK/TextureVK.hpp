/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static void NRI_CALL SetTextureDebugName(Texture& texture, const char* name)
{
    ((TextureVK&)texture).SetDebugName(name);
}

static uint64_t NRI_CALL GetTextureNativeObject(const Texture& texture, uint32_t physicalDeviceIndex)
{
    return uint64_t( ((TextureVK&)texture).GetHandle(physicalDeviceIndex) );
}

static void NRI_CALL GetTextureMemoryInfo(const Texture& texture, MemoryLocation memoryLocation, MemoryDesc& memoryDesc)
{
    ((TextureVK&)texture).GetMemoryInfo(memoryLocation, memoryDesc);
}

#pragma endregion

Define_Core_Texture_PartiallyFillFunctionTable(VK)
