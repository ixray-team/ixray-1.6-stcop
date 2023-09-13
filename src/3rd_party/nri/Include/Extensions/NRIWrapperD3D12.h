/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include "NRIDeviceCreation.h"

#if defined(NRI_CPP)
    struct ID3D12Device;
    struct ID3D12CommandQueue;
    struct ID3D12Resource;
    struct ID3D12Heap;
    struct ID3D12GraphicsCommandList;
    struct ID3D12CommandAllocator;
    struct IDXGIAdapter;
#else
    typedef struct ID3D12Device ID3D12Device;
    typedef struct ID3D12CommandQueue ID3D12CommandQueue;
    typedef struct ID3D12Resource ID3D12Resource;
    typedef struct ID3D12Heap ID3D12Heap;
    typedef struct ID3D12GraphicsCommandList ID3D12GraphicsCommandList;
    typedef struct ID3D12CommandAllocator ID3D12CommandAllocator;
    typedef struct IDXGIAdapter IDXGIAdapter;
#endif

NRI_NAMESPACE_BEGIN

NRI_FORWARD_STRUCT(AccelerationStructure);

NRI_STRUCT(DeviceCreationD3D12Desc)
{
    ID3D12Device* d3d12Device;
    ID3D12CommandQueue* d3d12GraphicsQueue;
    ID3D12CommandQueue* d3d12ComputeQueue;
    ID3D12CommandQueue* d3d12CopyQueue;
    IDXGIAdapter* d3d12PhysicalAdapter;
    NRI_NAME(CallbackInterface) callbackInterface;
    NRI_NAME(MemoryAllocatorInterface) memoryAllocatorInterface;
    bool enableNRIValidation;
    bool enableAPIValidation;
};

NRI_STRUCT(CommandBufferD3D12Desc)
{
    ID3D12GraphicsCommandList* d3d12CommandList;
    ID3D12CommandAllocator* d3d12CommandAllocator;
};

NRI_STRUCT(BufferD3D12Desc)
{
    ID3D12Resource* d3d12Resource;
    uint32_t structureStride;
};

NRI_STRUCT(TextureD3D12Desc)
{
    ID3D12Resource* d3d12Resource;
};

NRI_STRUCT(MemoryD3D12Desc)
{
    ID3D12Heap* d3d12Heap;
};

NRI_STRUCT(AccelerationStructureD3D12Desc)
{
    ID3D12Resource* d3d12Resource;
    uint64_t scratchDataSizeInBytes;
    uint64_t updateScratchDataSizeInBytes;
};

NRI_STRUCT(WrapperD3D12Interface)
{
    NRI_NAME(Result) (NRI_CALL *CreateCommandBufferD3D12)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(CommandBufferD3D12Desc) commandBufferDesc, NRI_REF_NAME(CommandBuffer*) commandBuffer);
    NRI_NAME(Result) (NRI_CALL *CreateBufferD3D12)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(BufferD3D12Desc) bufferDesc, NRI_REF_NAME(Buffer*) buffer);
    NRI_NAME(Result) (NRI_CALL *CreateTextureD3D12)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(TextureD3D12Desc) textureDesc, NRI_REF_NAME(Texture*) texture);
    NRI_NAME(Result) (NRI_CALL *CreateMemoryD3D12)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(MemoryD3D12Desc) memoryDesc, NRI_REF_NAME(Memory*) memory);
    NRI_NAME(Result) (NRI_CALL *CreateAccelerationStructureD3D12)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(AccelerationStructureD3D12Desc) accelerationStructureDesc, NRI_REF_NAME(AccelerationStructure*) accelerationStructure);
};

#if defined(NRI_CPP)
    NRI_API Result NRI_CALL CreateDeviceFromD3D12Device(const DeviceCreationD3D12Desc& deviceDesc, Device*& device);
    NRI_API Format NRI_CALL ConvertDXGIFormatToNRI(uint32_t dxgiFormat);
    NRI_API uint32_t NRI_CALL ConvertNRIFormatToDXGI(Format format);
#endif

NRI_NAMESPACE_END

NRIC_API uint8_t NRI_CALL nri_CreateDeviceFromD3D12Device(const void* deviceDesc, void** device);
NRIC_API uint8_t NRI_CALL nri_ConvertDXGIFormatToNRI(uint32_t dxgiFormat);
NRIC_API uint32_t NRI_CALL nri_ConvertNRIFormatToDXGI(uint8_t format);
