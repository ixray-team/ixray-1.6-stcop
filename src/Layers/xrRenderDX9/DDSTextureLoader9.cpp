//--------------------------------------------------------------------------------------
// File: DDSTextureLoader9.cpp
//
// Functions for loading a DDS texture and creating a Direct3D runtime resource for it
//
// Note these functions are useful as a light-weight runtime loader for DDS files. For
// a full-featured DDS file reader, writer, and texture processing pipeline see
// the 'Texconv' sample and the 'DirectXTex' library.
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/?LinkId=248926
// http://go.microsoft.com/fwlink/?LinkId=248929
//--------------------------------------------------------------------------------------

#include "stdafx.h"
#include "DDSTextureLoader9.h"

#ifdef _MSC_VER
// Off by default warnings
#pragma warning(disable : 4619 4616 4623 4626 5027)
// C4619/4616 #pragma warning warnings
// C4623 default constructor was implicitly defined as deleted
// C4626 assignment operator was implicitly defined as deleted
// C5027 move assignment operator was implicitly defined as deleted
#endif

#ifdef __clang__
#pragma clang diagnostic ignored "-Wcovered-switch-default"
#pragma clang diagnostic ignored "-Wswitch-enum"
#endif

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemory(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    LPDIRECT3DBASETEXTURE9* texture,
    bool generateMipsIfMissing) noexcept
{
    return CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, 0u, D3DPOOL_DEFAULT, generateMipsIfMissing, texture);
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemoryEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    _In_ DWORD usage,
    _In_ D3DPOOL pool,
    bool generateMipsIfMissing,
    LPDIRECT3DBASETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !texture)
    {
        return E_INVALIDARG;
    }

    // Validate DDS file in memory
    const DDS_HEADER* header = nullptr;
    const uint8_t* bitData = nullptr;
    size_t bitSize = 0;

    HRESULT hr = LoadTextureDataFromMemory(ddsData, ddsDataSize,
        &header,
        &bitData,
        &bitSize
    );
    if (FAILED(hr))
        return hr;

    return CreateTextureFromDDS(
        d3dDevice,
        header,
        bitData,
        bitSize,
        usage,
        pool,
        texture,
        generateMipsIfMissing);
}

// Type-specific standard versions
_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemory(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    LPDIRECT3DTEXTURE9* texture,
    bool generateMipsIfMissing) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !ddsDataSize || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, 0u, D3DPOOL_DEFAULT, generateMipsIfMissing, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_TEXTURE)
        {
            *texture = static_cast<LPDIRECT3DTEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemory(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    LPDIRECT3DCUBETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !ddsDataSize || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, 0u, D3DPOOL_DEFAULT, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_CUBETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DCUBETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemory(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    LPDIRECT3DVOLUMETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !ddsDataSize || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, 0u, D3DPOOL_DEFAULT, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_VOLUMETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DVOLUMETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

// Type-specific extended versions
_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemoryEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    DWORD usage,
    D3DPOOL pool,
    bool generateMipsIfMissing,
    LPDIRECT3DTEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !ddsDataSize || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, usage, pool, generateMipsIfMissing, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_TEXTURE)
        {
            *texture = static_cast<LPDIRECT3DTEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemoryEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    DWORD usage,
    D3DPOOL pool,
    LPDIRECT3DCUBETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !ddsDataSize || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, usage, pool, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_CUBETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DCUBETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromMemoryEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const uint8_t* ddsData,
    size_t ddsDataSize,
    DWORD usage,
    D3DPOOL pool,
    LPDIRECT3DVOLUMETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !ddsData || !ddsDataSize || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromMemoryEx(d3dDevice, ddsData, ddsDataSize, usage, pool, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_VOLUMETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DVOLUMETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}


//--------------------------------------------------------------------------------------
_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFile(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* fileName,
    LPDIRECT3DBASETEXTURE9* texture,
    bool generateMipsIfMissing) noexcept
{
    return CreateDDSTextureFromFileEx(d3dDevice, fileName, 0u, D3DPOOL_DEFAULT, generateMipsIfMissing, texture);
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFileEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* fileName,
    _In_ DWORD usage,
    _In_ D3DPOOL pool,
    bool generateMipsIfMissing,
    LPDIRECT3DBASETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !fileName || !texture)
        return E_INVALIDARG;

    const DDS_HEADER* header = nullptr;
    const uint8_t* bitData = nullptr;
    size_t bitSize = 0;

    std::unique_ptr<uint8_t[]> ddsData;
    HRESULT hr = LoadTextureDataFromFile(fileName,
        ddsData,
        &header,
        &bitData,
        &bitSize
    );
    if (FAILED(hr))
    {
        return hr;
    }

    return CreateTextureFromDDS(
        d3dDevice,
        header,
        bitData,
        bitSize,
        usage,
        pool,
        texture,
        generateMipsIfMissing);
}

// Type-specific standard versions
_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFile(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* fileName,
    LPDIRECT3DTEXTURE9* texture,
    bool generateMipsIfMissing) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !fileName || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromFileEx(d3dDevice, fileName, 0u, D3DPOOL_DEFAULT, generateMipsIfMissing, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_TEXTURE)
        {
            *texture = static_cast<LPDIRECT3DTEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFile(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* fileName,
    LPDIRECT3DCUBETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !fileName || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromFileEx(d3dDevice, fileName, 0u, D3DPOOL_DEFAULT, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_CUBETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DCUBETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFile(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* szFileName,
    LPDIRECT3DVOLUMETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !szFileName || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromFileEx(d3dDevice, szFileName, 0u, D3DPOOL_DEFAULT, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_VOLUMETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DVOLUMETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

// Type-specific extended versions
_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFileEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* fileName,
    DWORD usage,
    D3DPOOL pool,
    bool generateMipsIfMissing,
    LPDIRECT3DTEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !fileName || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromFileEx(d3dDevice, fileName, usage, pool, generateMipsIfMissing, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_TEXTURE)
        {
            *texture = static_cast<LPDIRECT3DTEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFileEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* fileName,
    DWORD usage,
    D3DPOOL pool,
    LPDIRECT3DCUBETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !fileName || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromFileEx(d3dDevice, fileName, usage, pool, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_CUBETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DCUBETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}

_Use_decl_annotations_
HRESULT DirectX::CreateDDSTextureFromFileEx(
    LPDIRECT3DDEVICE9 d3dDevice,
    const wchar_t* szFileName,
    DWORD usage,
    D3DPOOL pool,
    LPDIRECT3DVOLUMETEXTURE9* texture) noexcept
{
    if (texture)
    {
        *texture = nullptr;
    }

    if (!d3dDevice || !szFileName || !texture)
        return E_INVALIDARG;

    ComPtr<IDirect3DBaseTexture9> tex;
    HRESULT hr = CreateDDSTextureFromFileEx(d3dDevice, szFileName, usage, pool, false, tex.GetAddressOf());
    if (SUCCEEDED(hr))
    {
        hr = E_FAIL;
        if (tex->GetType() == D3DRTYPE_VOLUMETEXTURE)
        {
            *texture = static_cast<LPDIRECT3DVOLUMETEXTURE9>(tex.Detach());
            return S_OK;
        }
    }

    return hr;
}
