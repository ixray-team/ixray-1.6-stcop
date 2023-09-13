/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedVK.h"
#include "BufferVK.h"
#include "AccelerationStructureVK.h"
#include "DescriptorVK.h"
#include "CommandQueueVK.h"
#include "ConversionVK.h"

using namespace nri;

AccelerationStructureVK::~AccelerationStructureVK()
{
    const auto& vk = m_Device.GetDispatchTable();

    if (!m_OwnsNativeObjects)
        return;

    for (uint32_t i = 0; i < GetCountOf(m_Handles); i++)
    {
        if (m_Handles[i] != VK_NULL_HANDLE)
            vk.DestroyAccelerationStructureKHR(m_Device, m_Handles[i], m_Device.GetAllocationCallbacks());
    }

    if (m_Buffer != nullptr)
        m_Device.DestroyBuffer(*(Buffer*)m_Buffer);
}

Result AccelerationStructureVK::Create(const AccelerationStructureDesc& accelerationStructureDesc)
{
    m_OwnsNativeObjects = true;
    m_Type = GetAccelerationStructureType(accelerationStructureDesc.type);
    m_BuildFlags = GetAccelerationStructureBuildFlags(accelerationStructureDesc.flags);

    uint32_t physicalDeviceMask = accelerationStructureDesc.physicalDeviceMask;
    physicalDeviceMask = (physicalDeviceMask == WHOLE_DEVICE_GROUP) ? 0xff : physicalDeviceMask;
    m_PhysicalDeviceMask = physicalDeviceMask;

    if (accelerationStructureDesc.type == AccelerationStructureType::BOTTOM_LEVEL)
        PrecreateBottomLevel(accelerationStructureDesc);
    else
        PrecreateTopLevel(accelerationStructureDesc);

    BufferDesc bufferDesc = {};
    bufferDesc.physicalDeviceMask = m_PhysicalDeviceMask;
    bufferDesc.size = m_AccelerationStructureSize;
    bufferDesc.usageMask = BufferUsageBits::RAY_TRACING_BUFFER;

    Buffer* buffer = nullptr;
    const Result result = m_Device.CreateBuffer(bufferDesc, buffer);
    m_Buffer = (BufferVK*)buffer;

    return result;
}

Result AccelerationStructureVK::Create(const AccelerationStructureVulkanDesc& accelerationStructureDesc)
{
    m_OwnsNativeObjects = false;
    m_Type = VK_ACCELERATION_STRUCTURE_TYPE_MAX_ENUM_KHR;
    m_BuildFlags = 0;

    uint32_t physicalDeviceMask = GetPhysicalDeviceGroupMask(accelerationStructureDesc.physicalDeviceMask);

    VkAccelerationStructureDeviceAddressInfoKHR deviceAddressInfo = {};
    deviceAddressInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR;
    deviceAddressInfo.accelerationStructure = (VkAccelerationStructureKHR)accelerationStructureDesc.vkAccelerationStructure;

    const auto& vk = m_Device.GetDispatchTable();
    const uint64_t deviceAddress = vk.GetAccelerationStructureDeviceAddressKHR(m_Device, &deviceAddressInfo);

    if (deviceAddress == 0)
        return Result::FAILURE;

    for (uint32_t i = 0; i < m_Device.GetPhysicalDeviceGroupSize(); i++)
    {
        if ((1 << i) & physicalDeviceMask)
        {
            m_Handles[i] = (VkAccelerationStructureKHR)accelerationStructureDesc.vkAccelerationStructure;
            m_DeviceAddresses[i] = deviceAddress;
        }
    }

    m_BuildScratchSize = accelerationStructureDesc.buildScratchSize;
    m_UpdateScratchSize = accelerationStructureDesc.updateScratchSize;

    return Result::SUCCESS;
}

void AccelerationStructureVK::PrecreateBottomLevel(const AccelerationStructureDesc& accelerationStructureDesc)
{
    const auto buildType = VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR;

    VkAccelerationStructureBuildGeometryInfoKHR buildInfo = {};
    buildInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
    buildInfo.type = m_Type;
    buildInfo.flags = m_BuildFlags;
    buildInfo.geometryCount = accelerationStructureDesc.instanceOrGeometryObjectNum;

    VkAccelerationStructureBuildSizesInfoKHR sizeInfo = {};
    sizeInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;

    Vector<VkAccelerationStructureGeometryKHR> geometries(m_Device.GetStdAllocator());
    uint32_t* primitiveMaxNums = ALLOCATE_SCRATCH(m_Device, uint32_t, accelerationStructureDesc.instanceOrGeometryObjectNum);

    geometries.resize(accelerationStructureDesc.instanceOrGeometryObjectNum);
    ConvertGeometryObjectSizesVK(0, geometries.data(), primitiveMaxNums, accelerationStructureDesc.geometryObjects, (uint32_t)geometries.size());
    buildInfo.pGeometries = geometries.data();

    const auto& vk = m_Device.GetDispatchTable();
    vk.GetAccelerationStructureBuildSizesKHR(m_Device, buildType, &buildInfo, primitiveMaxNums, &sizeInfo);

    m_BuildScratchSize = sizeInfo.buildScratchSize;
    m_UpdateScratchSize = sizeInfo.updateScratchSize;
    m_AccelerationStructureSize = sizeInfo.accelerationStructureSize;

    FREE_SCRATCH(m_Device, primitiveMaxNums, accelerationStructureDesc.instanceOrGeometryObjectNum);
}

void AccelerationStructureVK::PrecreateTopLevel(const AccelerationStructureDesc& accelerationStructureDesc)
{
    const auto buildType = VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR;

    VkAccelerationStructureGeometryKHR geometry = {};
    geometry.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
    geometry.geometryType = VK_GEOMETRY_TYPE_INSTANCES_KHR;
    geometry.geometry.triangles.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR;
    geometry.geometry.aabbs.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR;
    geometry.geometry.instances.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR;

    VkAccelerationStructureBuildGeometryInfoKHR buildInfo = {};
    buildInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
    buildInfo.type = m_Type;
    buildInfo.flags = m_BuildFlags;
    buildInfo.geometryCount = 1;
    buildInfo.pGeometries = &geometry;

    const uint32_t instanceMaxNum = accelerationStructureDesc.instanceOrGeometryObjectNum;

    VkAccelerationStructureBuildSizesInfoKHR sizeInfo = {};
    sizeInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;

    const auto& vk = m_Device.GetDispatchTable();
    vk.GetAccelerationStructureBuildSizesKHR(m_Device, buildType, &buildInfo, &instanceMaxNum, &sizeInfo);

    m_BuildScratchSize = sizeInfo.buildScratchSize;
    m_UpdateScratchSize = sizeInfo.updateScratchSize;
    m_AccelerationStructureSize = sizeInfo.accelerationStructureSize;
}

Result AccelerationStructureVK::FinishCreation()
{
    if (m_Buffer == nullptr)
        return Result::FAILURE;

    VkAccelerationStructureCreateInfoKHR accelerationStructureCreateInfo = {};
    accelerationStructureCreateInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR;
    accelerationStructureCreateInfo.type = m_Type;
    accelerationStructureCreateInfo.size = m_AccelerationStructureSize;

    const auto& vk = m_Device.GetDispatchTable();

    for (uint32_t i = 0; i < m_Device.GetPhysicalDeviceGroupSize(); i++)
    {
        if ((1 << i) & m_PhysicalDeviceMask)
        {
            accelerationStructureCreateInfo.buffer = m_Buffer->GetHandle(i);

            const VkResult result = vk.CreateAccelerationStructureKHR(m_Device, &accelerationStructureCreateInfo,
                m_Device.GetAllocationCallbacks(), &m_Handles[i]);

            RETURN_ON_FAILURE(m_Device.GetLog(), result == VK_SUCCESS, GetReturnCode(result),
                "Can't create an acceleration structure: vkCreateAccelerationStructureKHR returned %d.", (int32_t)result);

            VkAccelerationStructureDeviceAddressInfoKHR deviceAddressInfo = {};
            deviceAddressInfo.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR;
            deviceAddressInfo.accelerationStructure = m_Handles[i];

            m_DeviceAddresses[i] = vk.GetAccelerationStructureDeviceAddressKHR(m_Device, &deviceAddressInfo);
        }
    }

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

inline void AccelerationStructureVK::SetDebugName(const char* name)
{
    std::array<uint64_t, PHYSICAL_DEVICE_GROUP_MAX_SIZE> handles;
    for (size_t i = 0; i < handles.size(); i++)
        handles[i] = (uint64_t)m_Handles[i];

    m_Device.SetDebugNameToDeviceGroupObject(VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR, handles.data(), name);
    m_Buffer->SetDebugName(name);
}

inline void AccelerationStructureVK::GetMemoryInfo(MemoryDesc& memoryDesc) const
{
    m_Buffer->GetMemoryInfo(MemoryLocation::DEVICE, memoryDesc);
}

inline Result AccelerationStructureVK::CreateDescriptor(uint32_t physicalDeviceMask, Descriptor*& descriptor) const
{
    DescriptorVK& descriptorImpl = *Allocate<DescriptorVK>(m_Device.GetStdAllocator(), m_Device);
    descriptorImpl.Create(m_Handles.data(), physicalDeviceMask);
    descriptor = (Descriptor*)&descriptorImpl;

    return Result::SUCCESS;
}

#include "AcceleratrionStructureVK.hpp"