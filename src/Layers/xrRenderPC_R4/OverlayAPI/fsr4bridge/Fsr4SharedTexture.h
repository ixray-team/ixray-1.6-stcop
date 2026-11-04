#pragma once

#include <d3d11_4.h>
#include <d3d12.h>
#include <wrl/client.h>

// ---------------------------------------------------------------------------
// A D3D12 resource + ID3D11Texture2D view sharing the same GPU memory.
// Created on the private D3D12 device (HEAP_FLAG_SHARED), opened into the
// game's D3D11 device via OpenSharedResource1.
//
// For depth input the source (D24_UNORM_S8_UINT) can't be shared directly:
// we instead create a colour-compatible R32_FLOAT intermediate, and the
// backend fills it with a fullscreen depth→float pass. That is what the
// `isDepthIntermediate` flag selects.
// ---------------------------------------------------------------------------
class Fsr4SharedTexture
{
public:
    bool Ensure(ID3D11Device1*   d3d11Device1,
                ID3D12Device*    d3d12Device,
                ID3D11Texture2D* gameTexture,
                bool             needUav,
                bool             isDepthIntermediate,
                const char*      debugName);

    void Destroy();

    bool IsValid() const { return m_Tex11 && m_Res12; }

    ID3D11Texture2D* GetD3D11Texture()  const { return m_Tex11.Get(); }
    ID3D12Resource*  GetD3D12Resource() const { return m_Res12.Get(); }

    bool IsDepthIntermediate() const { return m_IsDepthIntermediate; }

private:
    bool Matches(ID3D11Texture2D* gameTexture, const D3D11_TEXTURE2D_DESC& desc) const
    {
        return m_SourcePtr           == gameTexture
            && m_Width               == desc.Width
            && m_Height              == desc.Height
            && m_SourceFormat        == desc.Format
            && m_IsDepthIntermediate == m_IsDepthIntermediate;
    }

    Microsoft::WRL::ComPtr<ID3D11Texture2D> m_Tex11;
    Microsoft::WRL::ComPtr<ID3D12Resource>  m_Res12;

    ID3D11Texture2D* m_SourcePtr           = nullptr;
    UINT             m_Width               = 0;
    UINT             m_Height              = 0;
    DXGI_FORMAT      m_SourceFormat        = DXGI_FORMAT_UNKNOWN;   // формат gameTexture
    DXGI_FORMAT      m_SharedFormat        = DXGI_FORMAT_UNKNOWN;   // формат shared-ресурса
    bool             m_IsDepthIntermediate = false;
};