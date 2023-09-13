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

struct DeviceD3D12;
struct BufferD3D12;

struct AccelerationStructureD3D12
{
    inline AccelerationStructureD3D12(DeviceD3D12& device)
        : m_Device(device)
    {}

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    inline operator BufferD3D12* () const
    { return m_Buffer; }

    ~AccelerationStructureD3D12();

    Result Create(const AccelerationStructureDesc& accelerationStructureDesc);
    Result Create(const AccelerationStructureD3D12Desc& accelerationStructureDesc);
    void GetMemoryInfo(MemoryDesc& memoryDesc) const;
    uint64_t GetUpdateScratchBufferSize() const;
    uint64_t GetBuildScratchBufferSize() const;
    Result BindMemory(Memory* memory, uint64_t offset);
    Result CreateDescriptor(Descriptor*& descriptor) const;

    uint64_t GetHandle() const;
    operator ID3D12Resource* () const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);

private:
    DeviceD3D12& m_Device;
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_PREBUILD_INFO m_PrebuildInfo = {};
    BufferD3D12* m_Buffer = nullptr;
};

}
