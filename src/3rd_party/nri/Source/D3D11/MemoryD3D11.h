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

struct DeviceD3D11;

enum class MemoryResidencyPriority
{
    DEFAULT,
    MINIMUM,
    LOW,
    NORMAL,
    HIGH,
    MAXIMUM
};

struct MemoryD3D11
{
    inline MemoryD3D11(DeviceD3D11& device, MemoryType memoryType) :
        m_Device(device)
        , m_Location((MemoryLocation)memoryType)
    {}

    inline ~MemoryD3D11()
    {}

    inline DeviceD3D11& GetDevice() const
    { return m_Device; }

    inline MemoryLocation GetType() const
    { return m_Location; }

    uint32_t GetResidencyPriority(uint64_t size) const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { MaybeUnused(name); }

private:
    DeviceD3D11& m_Device;
    MemoryLocation m_Location;
    MemoryResidencyPriority m_ResidencyPriority = MemoryResidencyPriority::DEFAULT;
};

}
