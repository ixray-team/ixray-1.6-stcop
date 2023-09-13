/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static void NRI_CALL SetDescriptorDebugName(Descriptor& descriptor, const char* name)
{
    ((DescriptorVK&)descriptor).SetDebugName(name);
}

static uint64_t NRI_CALL GetDescriptorNativeObject(const Descriptor& descriptor, uint32_t physicalDeviceIndex)
{
    const DescriptorVK& d = ((DescriptorVK&)descriptor);

    uint64_t handle = 0;
    if (d.GetType() == DescriptorTypeVK::BUFFER_VIEW)
        handle = (uint64_t)d.GetBufferView(physicalDeviceIndex);
    else if (d.GetType() == DescriptorTypeVK::IMAGE_VIEW)
        handle = (uint64_t)d.GetImageView(physicalDeviceIndex);
    else if (d.GetType() == DescriptorTypeVK::SAMPLER)
        handle = (uint64_t)d.GetSampler();
    else if (d.GetType() == DescriptorTypeVK::ACCELERATION_STRUCTURE)
        handle = (uint64_t)d.GetAccelerationStructure(physicalDeviceIndex);

    return handle;
}

#pragma endregion

Define_Core_Descriptor_PartiallyFillFunctionTable(VK)
