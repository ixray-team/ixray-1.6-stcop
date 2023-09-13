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

struct AccelerationStructureVK
{
    inline AccelerationStructureVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline VkAccelerationStructureKHR GetHandle(uint32_t physicalDeviceIndex) const
    { return m_Handles[physicalDeviceIndex]; }

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline BufferVK* GetBuffer() const
    { return m_Buffer; }

    ~AccelerationStructureVK();

    Result Create(const AccelerationStructureDesc& accelerationStructureDesc);
    Result Create(const AccelerationStructureVulkanDesc& accelerationStructureDesc);
    Result FinishCreation();

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline uint64_t GetUpdateScratchBufferSize() const
    { return m_UpdateScratchSize; }

    inline uint64_t GetBuildScratchBufferSize() const
    { return m_BuildScratchSize; }

    inline VkDeviceAddress GetNativeHandle(uint32_t physicalDeviceIndex) const
    { return m_DeviceAddresses[physicalDeviceIndex]; }

    void SetDebugName(const char* name);
    void GetMemoryInfo(MemoryDesc& memoryDesc) const;
    Result CreateDescriptor(uint32_t physicalDeviceMask, Descriptor*& descriptor) const;

private:
    void PrecreateBottomLevel(const AccelerationStructureDesc& accelerationStructureDesc);
    void PrecreateTopLevel(const AccelerationStructureDesc& accelerationStructureDesc);

private:
    DeviceVK& m_Device;
    std::array<VkAccelerationStructureKHR, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_Handles = {};
    std::array<VkDeviceAddress, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_DeviceAddresses = {};
    BufferVK* m_Buffer = nullptr;
    uint64_t m_BuildScratchSize = 0;
    uint64_t m_UpdateScratchSize = 0;
    uint64_t m_AccelerationStructureSize = 0;
    uint32_t m_PhysicalDeviceMask = 0;
    VkAccelerationStructureTypeKHR m_Type = (VkAccelerationStructureTypeKHR)0;
    VkBuildAccelerationStructureFlagsKHR m_BuildFlags = (VkBuildAccelerationStructureFlagsKHR)0;
    bool m_OwnsNativeObjects = false;
};

}