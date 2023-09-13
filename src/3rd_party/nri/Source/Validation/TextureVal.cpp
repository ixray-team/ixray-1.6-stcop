/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedExternal.h"
#include "DeviceBase.h"
#include "DeviceVal.h"
#include "SharedVal.h"
#include "MemoryVal.h"
#include "TextureVal.h"

using namespace nri;

TextureType GetTextureTypeVK(uint32_t vkImageType);
TextureType GetTextureTypeD3D12(void* d3d12Resource);

TextureVal::TextureVal(DeviceVal& device, Texture& texture, const TextureDesc& textureDesc) :
    DeviceObjectVal(device, texture),
    m_TextureDesc(textureDesc)
{
}

#if NRI_USE_D3D11
TextureVal::TextureVal(DeviceVal& device, Texture& texture, const TextureD3D11Desc& textureD3D11Desc) :
    DeviceObjectVal(device, texture)
{
    GetTextureDescD3D11(textureD3D11Desc, m_TextureDesc);
}
#endif

#if NRI_USE_D3D12
TextureVal::TextureVal(DeviceVal& device, Texture& texture, const TextureD3D12Desc& textureD3D12Desc) :
    DeviceObjectVal(device, texture)
{
    GetTextureDescD3D12(textureD3D12Desc, m_TextureDesc);
}
#endif

TextureVal::TextureVal(DeviceVal& device, Texture& texture, const TextureVulkanDesc& textureVulkanDesc) :
    DeviceObjectVal(device, texture)
{
    m_TextureDesc = {};
    m_TextureDesc.type = GetTextureTypeVK(textureVulkanDesc.vkImageType);
    m_TextureDesc.format = ConvertVKFormatToNRI(textureVulkanDesc.vkFormat);

    static_assert(sizeof(TextureUsageBits) == sizeof(uint16_t), "Unexpected TextureUsageBits sizeof");
    m_TextureDesc.usageMask = (TextureUsageBits)0xffff;

    m_TextureDesc.size[0] = textureVulkanDesc.size[0];
    m_TextureDesc.size[1] = textureVulkanDesc.size[1];
    m_TextureDesc.size[2] = textureVulkanDesc.size[2];

    m_TextureDesc.mipNum = textureVulkanDesc.mipNum;
    m_TextureDesc.arraySize = textureVulkanDesc.arraySize;
    m_TextureDesc.sampleNum = textureVulkanDesc.sampleNum;
    m_TextureDesc.physicalDeviceMask = textureVulkanDesc.physicalDeviceMask;
}

TextureVal::~TextureVal()
{
    if (m_Memory != nullptr)
        m_Memory->UnbindTexture(*this);
}

void TextureVal::SetDebugName(const char* name)
{
    m_Name = name;
    m_CoreAPI.SetTextureDebugName(m_ImplObject, name);
}

void TextureVal::GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const
{
    m_CoreAPI.GetTextureMemoryInfo(m_ImplObject, memoryLocation, memoryDesc);
    m_Device.RegisterMemoryType(memoryDesc.type, memoryLocation);
}

#include "TextureVal.hpp"
