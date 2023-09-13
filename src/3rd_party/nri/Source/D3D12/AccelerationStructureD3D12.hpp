/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  RayTracing  ]

static Result NRI_CALL CreateAccelerationStructureDescriptor(const AccelerationStructure& accelerationStructure, uint32_t physicalDeviceIndex, Descriptor*& descriptor)
{
    MaybeUnused(physicalDeviceIndex); // TODO: use it

    return ((AccelerationStructureD3D12&)accelerationStructure).CreateDescriptor(descriptor);
}

static void NRI_CALL GetAccelerationStructureMemoryInfo(const AccelerationStructure& accelerationStructure, MemoryDesc& memoryDesc)
{
    ((AccelerationStructureD3D12&)accelerationStructure).GetMemoryInfo(memoryDesc);
}

static uint64_t NRI_CALL GetAccelerationStructureUpdateScratchBufferSize(const AccelerationStructure& accelerationStructure)
{
    return ((AccelerationStructureD3D12&)accelerationStructure).GetUpdateScratchBufferSize();
}

static uint64_t NRI_CALL GetAccelerationStructureBuildScratchBufferSize(const AccelerationStructure& accelerationStructure)
{
    return ((AccelerationStructureD3D12&)accelerationStructure).GetBuildScratchBufferSize();
}

static uint64_t NRI_CALL GetAccelerationStructureHandle(const AccelerationStructure& accelerationStructure, uint32_t physicalDeviceIndex)
{
    MaybeUnused(physicalDeviceIndex); // TODO: use it

    return ((AccelerationStructureD3D12&)accelerationStructure).GetHandle();
}

static void NRI_CALL SetAccelerationStructureDebugName(AccelerationStructure& accelerationStructure, const char* name)
{
    ((AccelerationStructureD3D12&)accelerationStructure).SetDebugName(name);
}

static uint64_t NRI_CALL GetAccelerationStructureNativeObject(const AccelerationStructure& accelerationStructure, uint32_t physicalDeviceIndex)
{
    MaybeUnused(physicalDeviceIndex); // TODO: use it

    return uint64_t((ID3D12Resource*)((AccelerationStructureD3D12&)accelerationStructure));
}

#pragma endregion

Define_RayTracing_AccelerationStructure_PartiallyFillFunctionTable(D3D12)
