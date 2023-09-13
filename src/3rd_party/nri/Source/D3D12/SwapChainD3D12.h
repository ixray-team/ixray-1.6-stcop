/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

struct IDXGISwapChain4;

namespace nri
{

struct DeviceD3D12;
struct TextureD3D12;

struct VersionedSwapChain
{
    inline ~VersionedSwapChain()
    {}

    inline IDXGISwapChain4* operator->() const
    { return ptr; }

    ComPtr<IDXGISwapChain4> ptr;
    uint8_t version = 0;
};

struct SwapChainD3D12
{
    inline SwapChainD3D12(DeviceD3D12& device) :
        m_Device(device)
        , m_Textures(device.GetStdAllocator())
    {}

    inline DeviceD3D12& GetDevice() const
    { return m_Device; }

    ~SwapChainD3D12();

    Result Create(const SwapChainDesc& swapChainDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_SwapChain.ptr, name); }

    Texture* const* GetTextures(uint32_t& textureNum, Format& format) const;
    uint32_t AcquireNextTexture();
    Result Present();
    Result SetHdrMetadata(const HdrMetadata& hdrMetadata);

private:
    DeviceD3D12& m_Device;
    VersionedSwapChain m_SwapChain;
    Vector<TextureD3D12*> m_Textures;
    SwapChainDesc m_SwapChainDesc = {};
    Format m_Format = Format::UNKNOWN;
    bool m_IsTearingAllowed = false;
    bool m_IsFullscreenEnabled = false;
};

}
