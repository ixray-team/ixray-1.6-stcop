/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D11.h"
#include "CommandAllocatorD3D11.h"
#include "CommandBufferD3D11.h"
#include "CommandBufferEmuD3D11.h"

using namespace nri;

Result CreateCommandBuffer(DeviceD3D11& device, ID3D11DeviceContext* precreatedContext, CommandBuffer*& commandBuffer)
{
    bool isImmediate = false;
    if (precreatedContext)
        isImmediate = precreatedContext->GetType() == D3D11_DEVICE_CONTEXT_IMMEDIATE;
    else
        isImmediate = device.GetDevice().isDeferredContextEmulated;

    void* impl;
    if (isImmediate)
        impl = Allocate<CommandBufferEmuD3D11>(device.GetStdAllocator(), device);
    else
        impl = Allocate<CommandBufferD3D11>(device.GetStdAllocator(), device);

    const nri::Result result = ((CommandBufferHelper*)impl)->Create(precreatedContext);

    if (result == nri::Result::SUCCESS)
    {
        commandBuffer = (CommandBuffer*)impl;
        return nri::Result::SUCCESS;
    }

    if (isImmediate)
        Deallocate(device.GetStdAllocator(), (CommandBufferEmuD3D11*)impl);
    else
        Deallocate(device.GetStdAllocator(), (CommandBufferD3D11*)impl);

    return result;
}

inline Result CommandAllocatorD3D11::CreateCommandBuffer(CommandBuffer*& commandBuffer)
{
    return ::CreateCommandBuffer(m_Device, nullptr, commandBuffer);
}

//================================================================================================================
// NRI
//================================================================================================================

#include "CommandAllocatorD3D11.hpp"
