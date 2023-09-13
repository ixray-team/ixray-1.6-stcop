/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D12.h"
#include "TextureD3D12.h"
#include "MemoryD3D12.h"

using namespace nri;

extern D3D12_RESOURCE_DIMENSION GetResourceDimension(TextureType textureType);
extern DXGI_FORMAT GetTypelessFormat(Format format);
extern D3D12_RESOURCE_FLAGS GetTextureFlags(TextureUsageBits textureUsageMask);

Result TextureD3D12::Create(const TextureDesc& textureDesc)
{
    uint16_t blockWidth = (uint16_t)GetTexelBlockWidth(textureDesc.format);

    m_TextureDesc.Dimension = GetResourceDimension(textureDesc.type);
    m_TextureDesc.Alignment = textureDesc.sampleNum > 1 ? 0 : D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT;
    m_TextureDesc.Width = Align(textureDesc.size[0], blockWidth);
    m_TextureDesc.Height = Align(textureDesc.size[1], blockWidth);
    m_TextureDesc.DepthOrArraySize = textureDesc.type == TextureType::TEXTURE_3D ? textureDesc.size[2] : textureDesc.arraySize;
    m_TextureDesc.MipLevels = textureDesc.mipNum;
    m_TextureDesc.Format = GetTypelessFormat(textureDesc.format);
    m_TextureDesc.SampleDesc.Count = textureDesc.sampleNum;
    m_TextureDesc.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
    m_TextureDesc.Flags = GetTextureFlags(textureDesc.usageMask);

    m_Format = textureDesc.format;

    return Result::SUCCESS;
}

Result TextureD3D12::Create(const TextureD3D12Desc& textureDesc)
{
    Initialize((ID3D12Resource*)textureDesc.d3d12Resource);

    return Result::SUCCESS;
}

void TextureD3D12::Initialize(ID3D12Resource* resource)
{
    m_Texture = resource;
    m_TextureDesc = resource->GetDesc();
    m_Format = DXGIFormatToNRIFormat((uint32_t)m_TextureDesc.Format);
}

Result TextureD3D12::BindMemory(const MemoryD3D12* memory, uint64_t offset)
{
    const D3D12_HEAP_DESC& heapDesc = memory->GetHeapDesc();
    D3D12_CLEAR_VALUE clearValue = { GetDXGIFormat(m_Format) };
    bool isRenderableSurface = m_TextureDesc.Flags & (D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET | D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);

    if (memory->RequiresDedicatedAllocation())
    {
        HRESULT hr = ((ID3D12Device*)m_Device)->CreateCommittedResource(
            &heapDesc.Properties,
            D3D12_HEAP_FLAG_CREATE_NOT_ZEROED,
            &m_TextureDesc,
            D3D12_RESOURCE_STATE_COMMON,
            isRenderableSurface ? &clearValue : nullptr,
            IID_PPV_ARGS(&m_Texture)
        );

        if (FAILED(hr))
        {
            REPORT_ERROR(m_Device.GetLog(), "ID3D12Device::CreateCommittedResource() failed, error code: 0x%X.", hr);
            return Result::FAILURE;
        }
    }
    else
    {
        HRESULT hr = ((ID3D12Device*)m_Device)->CreatePlacedResource(
            *memory,
            offset,
            &m_TextureDesc,
            D3D12_RESOURCE_STATE_COMMON,
            isRenderableSurface ? &clearValue : nullptr,
            IID_PPV_ARGS(&m_Texture)
        );

        if (FAILED(hr))
        {
            REPORT_ERROR(m_Device.GetLog(), "ID3D12Device::CreatePlacedResource() failed, error code: 0x%X.", hr);
            return Result::FAILURE;
        }
    }

    return Result::SUCCESS;
}

uint16_t TextureD3D12::GetSize(uint32_t dimension, uint32_t mipOffset) const
{
    assert(dimension < 3);

    uint16_t size;
    if (dimension == 0)
        size = (uint16_t)m_TextureDesc.Width;
    else if (dimension == 1)
        size = (uint16_t)m_TextureDesc.Height;
    else
        size = (uint16_t)m_TextureDesc.DepthOrArraySize;

    size = (uint16_t)std::max(size >> mipOffset, 1);
    size = Align(size, dimension < 2 ? (uint16_t)GetTexelBlockWidth(m_Format) : 1);

    return size;
}

//================================================================================================================
// NRI
//================================================================================================================

inline void TextureD3D12::GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const
{
    m_Device.GetMemoryInfo(memoryLocation, m_TextureDesc, memoryDesc);
}

#include "TextureD3D12.hpp"
