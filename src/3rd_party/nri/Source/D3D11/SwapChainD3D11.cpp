/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include <dxgi1_5.h>

#include "SharedD3D11.h"
#include "SwapChainD3D11.h"
#include "TextureD3D11.h"

using namespace nri;

static std::array<DXGI_FORMAT, 5> g_swapChainFormat =
{
    DXGI_FORMAT_R8G8B8A8_UNORM,                     // BT709_G10_8BIT,
    DXGI_FORMAT_R16G16B16A16_FLOAT,                 // BT709_G10_16BIT,
    DXGI_FORMAT_R8G8B8A8_UNORM,                     // BT709_G22_8BIT,
    DXGI_FORMAT_R10G10B10A2_UNORM,                  // BT709_G22_10BIT,
    DXGI_FORMAT_R10G10B10A2_UNORM,                  // BT2020_G2084_10BIT
};

static std::array<DXGI_COLOR_SPACE_TYPE, 5> g_colorSpace =
{
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709,        // BT709_G10_8BIT,
    DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709,        // BT709_G10_16BIT,
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709,        // BT709_G22_8BIT,
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709,        // BT709_G22_10BIT,
    DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020,     // BT2020_G2084_10BIT
};

static std::array<Format, 5> g_swapChainTextureFormat =
{
    Format::RGBA8_SRGB,                             // BT709_G10_8BIT,
    Format::RGBA16_SFLOAT,                          // BT709_G10_16BIT,
    Format::RGBA8_UNORM,                            // BT709_G22_8BIT,
    Format::R10_G10_B10_A2_UNORM,                   // BT709_G22_10BIT,
    Format::R10_G10_B10_A2_UNORM,                   // BT2020_G2084_10BIT
};

SwapChainD3D11::~SwapChainD3D11()
{
    if (m_IsFullscreenEnabled)
    {
        BOOL fullscreen = FALSE;
        m_SwapChain->GetFullscreenState(&fullscreen, nullptr);
        if (fullscreen)
            m_SwapChain->SetFullscreenState(FALSE, nullptr);
    }

    for (TextureD3D11* texture : m_Textures)
        Deallocate<TextureD3D11>(m_Device.GetStdAllocator(), texture);
}

Result SwapChainD3D11::Create(const SwapChainDesc& swapChainDesc)
{
    HWND hwnd = (HWND)swapChainDesc.window.windows.hwnd;
    if (!hwnd)
        return Result::INVALID_ARGUMENT;

    ComPtr<IDXGIDevice> dxgiDevice;
    HRESULT hr = m_Device.GetDevice()->QueryInterface(IID_PPV_ARGS(&dxgiDevice));
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IUnknown::QueryInterface() - FAILED!");

    ComPtr<IDXGIAdapter> dxgiAdapter;
    hr = dxgiDevice->GetAdapter(&dxgiAdapter);
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGIDevice::GetAdapter() - FAILED!");

    ComPtr<IDXGIFactory2> dxgiFactory2;
    hr = dxgiAdapter->GetParent(IID_PPV_ARGS(&dxgiFactory2));
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGIObject::GetParent() - FAILED!");

    m_IsTearingAllowed = false;
    ComPtr<IDXGIFactory5> dxgiFactory5;
    hr = dxgiFactory2->QueryInterface(IID_PPV_ARGS(&dxgiFactory5));
    if (SUCCEEDED(hr))
    {
        uint32_t tearingSupport = 0;
        hr = dxgiFactory5->CheckFeatureSupport(DXGI_FEATURE_PRESENT_ALLOW_TEARING, &tearingSupport, sizeof(tearingSupport));
        m_IsTearingAllowed = (SUCCEEDED(hr) && tearingSupport) ? true : false;
    }

    DXGI_FORMAT format = g_swapChainFormat[(uint32_t)swapChainDesc.format];
    DXGI_COLOR_SPACE_TYPE colorSpace = g_colorSpace[(uint32_t)swapChainDesc.format];

    DXGI_SWAP_CHAIN_DESC1 desc = {};
    desc.BufferCount = swapChainDesc.textureNum;
    desc.Width = swapChainDesc.width;
    desc.Height = swapChainDesc.height;
    desc.Format = format;
    desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
    desc.SampleDesc.Count = 1;
    desc.Flags = m_IsTearingAllowed ? DXGI_SWAP_CHAIN_FLAG_ALLOW_TEARING : 0;
    desc.Scaling = DXGI_SCALING_NONE;
    desc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;

    ComPtr<IDXGISwapChain1> swapChain;
    hr = dxgiFactory2->CreateSwapChainForHwnd(m_Device.GetDevice().ptr, hwnd, &desc, nullptr, nullptr, &swapChain);
    if (FAILED(hr))
    {
        // are we on Win7?
        desc.Scaling = DXGI_SCALING_STRETCH;
        hr = dxgiFactory2->CreateSwapChainForHwnd(m_Device.GetDevice().ptr, hwnd, &desc, nullptr, nullptr, &swapChain);
    }
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGIFactory2::CreateSwapChainForHwnd() - FAILED!");

    hr = dxgiFactory2->MakeWindowAssociation(hwnd, DXGI_MWA_NO_ALT_ENTER);
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGIFactory::MakeWindowAssociation() - FAILED!");

    hr = swapChain->QueryInterface(__uuidof(IDXGISwapChain4), (void**)&m_SwapChain.ptr);
    m_SwapChain.version = 4;
    if (FAILED(hr))
    {
        REPORT_WARNING(m_Device.GetLog(), "QueryInterface(IDXGISwapChain4) - FAILED!");
        hr = m_Device.GetDevice()->QueryInterface(__uuidof(IDXGISwapChain3), (void**)&m_SwapChain.ptr);
        m_SwapChain.version = 3;
        if (FAILED(hr))
        {
            REPORT_WARNING(m_Device.GetLog(), "QueryInterface(IDXGISwapChain3) - FAILED!");
            hr = m_Device.GetDevice()->QueryInterface(__uuidof(IDXGISwapChain2), (void**)&m_SwapChain.ptr);
            m_SwapChain.version = 2;
            if (FAILED(hr))
            {
                REPORT_WARNING(m_Device.GetLog(), "QueryInterface(IDXGISwapChain2) - FAILED!");
                hr = m_Device.GetDevice()->QueryInterface(__uuidof(IDXGISwapChain1), (void**)&m_SwapChain.ptr);
                m_SwapChain.version = 1;
                if (FAILED(hr))
                {
                    REPORT_WARNING(m_Device.GetLog(), "QueryInterface(IDXGISwapChain1) - FAILED!");
                    m_SwapChain.ptr = (IDXGISwapChain4*)swapChain.GetInterface();
                    m_SwapChain.version = 0;
                }
            }
        }
    }

    if (m_SwapChain.version >= 3)
    {
        UINT colorSpaceSupport = 0;
        hr = m_SwapChain->CheckColorSpaceSupport(colorSpace, &colorSpaceSupport);

        if ( !(colorSpaceSupport & DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG_PRESENT) )
            hr = E_FAIL;

        if (SUCCEEDED(hr))
            hr = m_SwapChain->SetColorSpace1(colorSpace);

        if (FAILED(hr))
           REPORT_WARNING(m_Device.GetLog(), "IDXGISwapChain3::SetColorSpace1() - FAILED!");
    }
    else
        REPORT_ERROR(m_Device.GetLog(), "IDXGISwapChain3::SetColorSpace1() is not supported by the OS!");

    if (m_SwapChain.version >= 1)
    {
        DXGI_RGBA color = {};
        hr = m_SwapChain->SetBackgroundColor(&color);
        if (FAILED(hr))
            REPORT_WARNING(m_Device.GetLog(), "IDXGISwapChain1::SetBackgroundColor() - FAILED!");
    }

    if (swapChainDesc.display != nullptr)
    {
        ComPtr<IDXGIOutput> output;
        if (!m_Device.GetOutput(swapChainDesc.display, output))
        {
            REPORT_ERROR(m_Device.GetLog(), "Failed to get IDXGIOutput for the specified display.");
            return Result::UNSUPPORTED;
        }

        hr = m_SwapChain->SetFullscreenState(TRUE, output);
        RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGISwapChain::SetFullscreenState() failed, error code: 0x%X.", hr);

        hr = m_SwapChain->ResizeBuffers(desc.BufferCount, desc.Width, desc.Height, desc.Format, desc.Flags);
        RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGISwapChain::ResizeBuffers() failed, error code: 0x%X.", hr);

        m_IsTearingAllowed = false;
        m_IsFullscreenEnabled = true;
    }

    // Use "swapChainDesc.textureNum" to explicitly limit maximum frames in flight
    ComPtr<IDXGIDevice1> dxgiDevice1;
    hr = dxgiDevice->QueryInterface(IID_PPV_ARGS(&dxgiDevice1));
    if (SUCCEEDED(hr))
        dxgiDevice1->SetMaximumFrameLatency(swapChainDesc.textureNum);

    m_Format = g_swapChainTextureFormat[(uint32_t)swapChainDesc.format];
    m_SwapChainDesc = swapChainDesc;

    // In DX11 only 'bufferIndex = 0' can be used to create render targets, so set BufferCount to '1' and ignore 'desc.BufferCount'
    m_SwapChainDesc.textureNum = 1;

    m_Textures.reserve(m_SwapChainDesc.textureNum);
    for (uint32_t i = 0; i < m_SwapChainDesc.textureNum; i++)
    {
        ComPtr<ID3D11Texture2D> textureNative;
        hr = m_SwapChain->GetBuffer(i, IID_PPV_ARGS(&textureNative));
        RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGISwapChain::GetBuffer() - FAILED!");

        TextureD3D11Desc textureDesc = {};
        textureDesc.d3d11Resource = textureNative;

        TextureD3D11* texture = Allocate<TextureD3D11>(m_Device.GetStdAllocator(), m_Device);
        const nri::Result res = texture->Create(textureDesc);
        if (res != nri::Result::SUCCESS)
            return res;

        m_Textures.push_back(texture);
    }

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

inline Texture* const* SwapChainD3D11::GetTextures(uint32_t& textureNum, Format& format) const
{
    textureNum = m_SwapChainDesc.textureNum;
    format = m_Format;

    return (Texture**)m_Textures.data();
}

inline uint32_t SwapChainD3D11::AcquireNextTexture()
{
    uint32_t nextTextureIndex = 0;
    if (m_SwapChain.version >= 3)
        nextTextureIndex = m_SwapChain->GetCurrentBackBufferIndex();

    return nextTextureIndex;
}

inline Result SwapChainD3D11::Present()
{
    BOOL fullscreen = FALSE;
    m_SwapChain->GetFullscreenState(&fullscreen, nullptr);
    if (fullscreen != BOOL(m_IsFullscreenEnabled))
        return Result::SWAPCHAIN_RESIZE;

    UINT flags = (!m_SwapChainDesc.verticalSyncInterval && m_IsTearingAllowed) ? DXGI_PRESENT_ALLOW_TEARING : 0;

    const HRESULT result = m_SwapChain->Present(m_SwapChainDesc.verticalSyncInterval, flags);

    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), result, "Can't present the swapchain: IDXGISwapChain::Present() returned %d.", (int32_t)result);

    return Result::SUCCESS;
}

inline Result SwapChainD3D11::SetHdrMetadata(const HdrMetadata& hdrMetadata)
{
    if (m_SwapChain.version < 4)
        return Result::UNSUPPORTED;

    DXGI_HDR_METADATA_HDR10 data = {};
    data.RedPrimary[0] = uint16_t(hdrMetadata.displayPrimaryRed[0] * 50000.0f);
    data.RedPrimary[1] = uint16_t(hdrMetadata.displayPrimaryRed[1] * 50000.0f);
    data.GreenPrimary[0] = uint16_t(hdrMetadata.displayPrimaryGreen[0] * 50000.0f);
    data.GreenPrimary[1] = uint16_t(hdrMetadata.displayPrimaryGreen[1] * 50000.0f);
    data.BluePrimary[0] = uint16_t(hdrMetadata.displayPrimaryBlue[0] * 50000.0f);
    data.BluePrimary[1] = uint16_t(hdrMetadata.displayPrimaryBlue[1] * 50000.0f);
    data.WhitePoint[0] = uint16_t(hdrMetadata.whitePoint[0] * 50000.0f);
    data.WhitePoint[1] = uint16_t(hdrMetadata.whitePoint[1] * 50000.0f);
    data.MaxMasteringLuminance = uint32_t(hdrMetadata.luminanceMax);
    data.MinMasteringLuminance = uint32_t(hdrMetadata.luminanceMin);
    data.MaxContentLightLevel = uint16_t(hdrMetadata.contentLightLevelMax);
    data.MaxFrameAverageLightLevel = uint16_t(hdrMetadata.frameAverageLightLevelMax);

    HRESULT hr = m_SwapChain->SetHDRMetaData(DXGI_HDR_METADATA_TYPE_HDR10, sizeof(DXGI_HDR_METADATA_HDR10), &data);
    RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "IDXGISwapChain4::SetHDRMetaData() - FAILED");

    return Result::SUCCESS;
}

#include "SwapChainD3D11.hpp"
