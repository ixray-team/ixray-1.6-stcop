/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static void NRI_CALL SetBufferDebugName(Buffer& buffer, const char* name)
{
    ((BufferD3D12&)buffer).SetDebugName(name);
}

static uint64_t NRI_CALL GetBufferNativeObject(const Buffer& buffer, uint32_t physicalDeviceIndex)
{
    MaybeUnused(physicalDeviceIndex);

    return uint64_t((ID3D12Resource*)((BufferD3D12&)buffer));
}

static void NRI_CALL GetBufferMemoryInfo(const Buffer& buffer, MemoryLocation memoryLocation, MemoryDesc& memoryDesc)
{
    ((BufferD3D12&)buffer).GetMemoryInfo(memoryLocation, memoryDesc);
}

static void* NRI_CALL MapBuffer(Buffer& buffer, uint64_t offset, uint64_t size)
{
    return ((BufferD3D12&)buffer).Map(offset, size);
}

static void NRI_CALL UnmapBuffer(Buffer& buffer)
{
    ((BufferD3D12&)buffer).Unmap();
}

#pragma endregion

Define_Core_Buffer_PartiallyFillFunctionTable(D3D12)
