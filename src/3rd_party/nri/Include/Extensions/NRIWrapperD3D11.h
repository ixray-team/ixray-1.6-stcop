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
    struct ID3D11Device;
    struct ID3D11Resource;
    struct ID3D11DeviceContext;
#else
    typedef struct ID3D11Device ID3D11Device;
    typedef struct ID3D11Resource ID3D11Resource;
    typedef struct ID3D11DeviceContext ID3D11DeviceContext;
#endif

NRI_NAMESPACE_BEGIN

NRI_STRUCT(DeviceCreationD3D11Desc)
{
    ID3D11Device* d3d11Device;
    void* agsContextAssociatedWithDevice;
    NRI_NAME(CallbackInterface) callbackInterface;
    NRI_NAME(MemoryAllocatorInterface) memoryAllocatorInterface;
    bool enableNRIValidation;
    bool enableAPIValidation;
};

NRI_STRUCT(CommandBufferD3D11Desc)
{
    ID3D11DeviceContext* d3d11DeviceContext;
};

NRI_STRUCT(BufferD3D11Desc)
{
    ID3D11Resource* d3d11Resource;
};

NRI_STRUCT(TextureD3D11Desc)
{
    ID3D11Resource* d3d11Resource;
};

NRI_STRUCT(WrapperD3D11Interface)
{
    NRI_NAME(Result) (NRI_CALL *CreateCommandBufferD3D11)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(CommandBufferD3D11Desc) commandBufferDesc, NRI_REF_NAME(CommandBuffer*) commandBuffer);
    NRI_NAME(Result) (NRI_CALL *CreateBufferD3D11)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(BufferD3D11Desc) bufferDesc, NRI_REF_NAME(Buffer*) buffer);
    NRI_NAME(Result) (NRI_CALL *CreateTextureD3D11)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(TextureD3D11Desc) textureDesc, NRI_REF_NAME(Texture*) texture);
};

#if defined(NRI_CPP)
    NRI_API Result NRI_CALL CreateDeviceFromD3D11Device(const DeviceCreationD3D11Desc& deviceDesc, Device*& device);
    NRI_API Format NRI_CALL ConvertDXGIFormatToNRI(uint32_t dxgiFormat);
    NRI_API uint32_t NRI_CALL ConvertNRIFormatToDXGI(Format format);
#endif

NRI_NAMESPACE_END

NRIC_API uint8_t NRI_CALL nri_CreateDeviceFromD3D11Device(const void* deviceDesc, void** device);
NRIC_API uint8_t NRI_CALL nri_ConvertDXGIFormatToNRI(uint32_t dxgiFormat);
NRIC_API uint32_t NRI_CALL nri_ConvertNRIFormatToDXGI(uint8_t format);
