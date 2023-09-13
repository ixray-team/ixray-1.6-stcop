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
    ((DescriptorD3D11&)descriptor).SetDebugName(name);
}

static uint64_t NRI_CALL GetDescriptorNativeObject(const Descriptor& descriptor, uint32_t physicalDeviceIndex)
{
    MaybeUnused(physicalDeviceIndex);

    return uint64_t( (ID3D11View*)((DescriptorD3D11&)descriptor) );
}

#pragma endregion

Define_Core_Descriptor_PartiallyFillFunctionTable(D3D11)