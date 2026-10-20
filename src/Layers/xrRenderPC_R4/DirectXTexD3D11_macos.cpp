
// DirectX::CreateTextureEx ported verbatim from Microsoft DirectXTex (MIT): the Windows include
// stack and this port's headers are incompatible in one translation unit, so only this function
// is taken.
#include "stdafx.h"

#include <cassert>
#include <cstdint>
#include <memory>
#include <new>

#include <DirectXTex.h>

static constexpr HRESULT XrHresultNotSupported = static_cast<HRESULT>(0x80070032L);

static constexpr HRESULT XrHresultUnexpected = static_cast<HRESULT>(0x8000FFFFL);

HRESULT DirectX::CreateTextureEx(ID3D11Device* pDevice,
    const Image*                                                      srcImages,
    size_t                                                            nimages,
    const TexMetadata&                                                metadata,
    D3D11_USAGE                                                       usage,
    unsigned int                                                      bindFlags,
    unsigned int                                                      cpuAccessFlags,
    unsigned int                                                      miscFlags,
    CREATETEX_FLAGS                                                   flags,
    ID3D11Resource**                                                  ppResource) noexcept
{
    if (!pDevice || !srcImages || !nimages || !ppResource)
        return E_INVALIDARG;

    *ppResource = nullptr;

    if (!metadata.mipLevels || !metadata.arraySize)
        return E_INVALIDARG;

    if ((metadata.width > UINT32_MAX) || (metadata.height > UINT32_MAX) || (metadata.mipLevels > UINT16_MAX)
        || (metadata.arraySize > UINT16_MAX))
        return E_INVALIDARG;

    std::unique_ptr<D3D11_SUBRESOURCE_DATA[]> initData(new (std::nothrow) D3D11_SUBRESOURCE_DATA[metadata.mipLevels * metadata.arraySize]);
    if (!initData)
        return E_OUTOFMEMORY;

    // Fill out subresource array
    if (metadata.IsVolumemap())
    {
        //--- Volume case -------------------------------------------------------------
        if (!metadata.depth)
            return E_INVALIDARG;

        if (metadata.depth > UINT16_MAX)
            return E_INVALIDARG;

        if (metadata.arraySize > 1)
            // Direct3D 11 doesn't support arrays of 3D textures
            return XrHresultNotSupported;

        size_t depth = metadata.depth;

        size_t idx = 0;
        for (size_t level = 0; level < metadata.mipLevels; ++level)
        {
            const size_t index = metadata.ComputeIndex(level, 0, 0);
            if (index >= nimages)
                return E_FAIL;

            const Image& img = srcImages[index];

            if (img.format != metadata.format)
                return E_FAIL;

            if (!img.pixels)
                return E_POINTER;

            // Verify pixels in image 1 .. (depth-1) are exactly image->slicePitch apart
            // For 3D textures, this relies on all slices of the same miplevel being continous in memory
            // (this is how ScratchImage lays them out), which is why we just give the 0th slice to Direct3D 11
            const uint8_t* pSlice = img.pixels + img.slicePitch;
            for (size_t slice = 1; slice < depth; ++slice)
            {
                const size_t tindex = metadata.ComputeIndex(level, 0, slice);
                if (tindex >= nimages)
                    return E_FAIL;

                const Image& timg = srcImages[tindex];

                if (!timg.pixels)
                    return E_POINTER;

                if (timg.pixels != pSlice || timg.format != metadata.format || timg.rowPitch != img.rowPitch
                    || timg.slicePitch != img.slicePitch)
                    return E_FAIL;

                pSlice = timg.pixels + img.slicePitch;
            }

            assert(idx < (metadata.mipLevels * metadata.arraySize));

            initData[idx].pSysMem          = img.pixels;
            initData[idx].SysMemPitch      = static_cast<DWORD>(img.rowPitch);
            initData[idx].SysMemSlicePitch = static_cast<DWORD>(img.slicePitch);
            ++idx;

            if (depth > 1)
                depth >>= 1;
        }
    }
    else
    {
        //--- 1D or 2D texture case ---------------------------------------------------
        size_t idx = 0;
        for (size_t item = 0; item < metadata.arraySize; ++item)
        {
            for (size_t level = 0; level < metadata.mipLevels; ++level)
            {
                const size_t index = metadata.ComputeIndex(level, item, 0);
                if (index >= nimages)
                    return E_FAIL;

                const Image& img = srcImages[index];

                if (img.format != metadata.format)
                    return E_FAIL;

                if (!img.pixels)
                    return E_POINTER;

                assert(idx < (metadata.mipLevels * metadata.arraySize));

                initData[idx].pSysMem          = img.pixels;
                initData[idx].SysMemPitch      = static_cast<DWORD>(img.rowPitch);
                initData[idx].SysMemSlicePitch = static_cast<DWORD>(img.slicePitch);
                ++idx;
            }
        }
    }

    // Create texture using static initialization data
    HRESULT hr = XrHresultUnexpected;

    DXGI_FORMAT format = metadata.format;
    if (flags & CREATETEX_FORCE_SRGB)
    {
        format = MakeSRGB(format);
    }
    else if (flags & CREATETEX_IGNORE_SRGB)
    {
        format = MakeLinear(format);
    }

    switch (metadata.dimension)
    {
    case TEX_DIMENSION_TEXTURE1D: {
        D3D11_TEXTURE1D_DESC desc = {};
        desc.Width                = static_cast<UINT>(metadata.width);
        desc.MipLevels            = static_cast<UINT>(metadata.mipLevels);
        desc.ArraySize            = static_cast<UINT>(metadata.arraySize);
        desc.Format               = format;
        desc.Usage                = usage;
        desc.BindFlags            = bindFlags;
        desc.CPUAccessFlags       = cpuAccessFlags;
        desc.MiscFlags            = miscFlags & ~static_cast<uint32_t>(D3D11_RESOURCE_MISC_TEXTURECUBE);

        hr = pDevice->CreateTexture1D(&desc, initData.get(), reinterpret_cast<ID3D11Texture1D**>(ppResource));
    }
    break;

    case TEX_DIMENSION_TEXTURE2D: {
        D3D11_TEXTURE2D_DESC desc = {};
        desc.Width                = static_cast<UINT>(metadata.width);
        desc.Height               = static_cast<UINT>(metadata.height);
        desc.MipLevels            = static_cast<UINT>(metadata.mipLevels);
        desc.ArraySize            = static_cast<UINT>(metadata.arraySize);
        desc.Format               = format;
        desc.SampleDesc.Count     = 1;
        desc.SampleDesc.Quality   = 0;
        desc.Usage                = usage;
        desc.BindFlags            = bindFlags;
        desc.CPUAccessFlags       = cpuAccessFlags;
        if (metadata.IsCubemap())
            desc.MiscFlags = miscFlags | D3D11_RESOURCE_MISC_TEXTURECUBE;
        else
            desc.MiscFlags = miscFlags & ~static_cast<uint32_t>(D3D11_RESOURCE_MISC_TEXTURECUBE);

        hr = pDevice->CreateTexture2D(&desc, initData.get(), reinterpret_cast<ID3D11Texture2D**>(ppResource));
    }
    break;

    case TEX_DIMENSION_TEXTURE3D: {
        D3D11_TEXTURE3D_DESC desc = {};
        desc.Width                = static_cast<UINT>(metadata.width);
        desc.Height               = static_cast<UINT>(metadata.height);
        desc.Depth                = static_cast<UINT>(metadata.depth);
        desc.MipLevels            = static_cast<UINT>(metadata.mipLevels);
        desc.Format               = format;
        desc.Usage                = usage;
        desc.BindFlags            = bindFlags;
        desc.CPUAccessFlags       = cpuAccessFlags;
        desc.MiscFlags            = miscFlags & ~static_cast<uint32_t>(D3D11_RESOURCE_MISC_TEXTURECUBE);

        hr = pDevice->CreateTexture3D(&desc, initData.get(), reinterpret_cast<ID3D11Texture3D**>(ppResource));
    }
    break;

    default: return XrHresultNotSupported;
    }

    return hr;
}
