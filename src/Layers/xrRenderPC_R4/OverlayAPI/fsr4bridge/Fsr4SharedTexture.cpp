#include "stdafx.h"
#include "Fsr4SharedTexture.h"
#include "Fsr4BridgeCommon.h"

static DXGI_FORMAT CoerceTypedFormat(DXGI_FORMAT f)
{
    switch (f)
    {
    case DXGI_FORMAT_R32G32B32A32_TYPELESS: return DXGI_FORMAT_R32G32B32A32_FLOAT;
    case DXGI_FORMAT_R16G16B16A16_TYPELESS: return DXGI_FORMAT_R16G16B16A16_FLOAT;
    case DXGI_FORMAT_R10G10B10A2_TYPELESS:  return DXGI_FORMAT_R10G10B10A2_UNORM;
    case DXGI_FORMAT_R8G8B8A8_TYPELESS:     return DXGI_FORMAT_R8G8B8A8_UNORM;
    case DXGI_FORMAT_B8G8R8A8_TYPELESS:     return DXGI_FORMAT_B8G8R8A8_UNORM;
    case DXGI_FORMAT_R32G32_TYPELESS:       return DXGI_FORMAT_R32G32_FLOAT;
    case DXGI_FORMAT_R16G16_TYPELESS:       return DXGI_FORMAT_R16G16_FLOAT;
    case DXGI_FORMAT_R32_TYPELESS:          return DXGI_FORMAT_R32_FLOAT;
    case DXGI_FORMAT_R16_TYPELESS:          return DXGI_FORMAT_R16_FLOAT;
    case DXGI_FORMAT_R8_TYPELESS:           return DXGI_FORMAT_R8_UNORM;
    case DXGI_FORMAT_R24G8_TYPELESS:        return DXGI_FORMAT_R24_UNORM_X8_TYPELESS;
    case DXGI_FORMAT_R32G8X24_TYPELESS:     return DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS;
    default:                                return f;
    }
}

bool Fsr4SharedTexture::Ensure(ID3D11Device1*   d3d11Device1,
                               ID3D12Device*    d3d12Device,
                               ID3D11Texture2D* gameTexture,
                               bool             needUav,
                               bool             isDepthIntermediate,
                               const char*      debugName)
{
    D3D11_TEXTURE2D_DESC desc = {};
    gameTexture->GetDesc(&desc);

    if (Matches(gameTexture, desc))
        return true;

    Destroy();

    if (desc.SampleDesc.Count > 1)
    {
        Msg("[FSR4] %s is MSAA — unsupported", debugName);
        return false;
    }

    // ---- Какой формат реально создавать в D3D12 / открывать в D3D11 ----
    DXGI_FORMAT targetFormat;
    if (isDepthIntermediate)
    {
        // Depth копируется полноэкранным проходом в R32_FLOAT — обычный
        // цветовой формат, sharing через NT-handle работает.
        targetFormat = DXGI_FORMAT_R32_FLOAT;
    }
    else
    {
        targetFormat = CoerceTypedFormat(desc.Format);
    }

    D3D12_HEAP_PROPERTIES heap = {};
    heap.Type = D3D12_HEAP_TYPE_DEFAULT;

    const D3D12_RESOURCE_FLAGS uavFlag = needUav
        ? D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS
        : D3D12_RESOURCE_FLAG_NONE;

    // Для промежуточной depth-текстуры (R32_FLOAT) нужны только цветовые
    // комбинации — depth-флаги здесь несовместимы с R32_FLOAT.
    struct Combo { D3D12_RESOURCE_FLAGS flags; const char* name; };
    xr_vector<Combo> combos;
    combos.push_back({ D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET | uavFlag,                                                 "RT"    });
    combos.push_back({ uavFlag,                                                                                           "plain" });
    combos.push_back({ D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET | uavFlag | D3D12_RESOURCE_FLAG_ALLOW_SIMULTANEOUS_ACCESS, "RT+SA" });

    for (const Combo& c : combos)
    {
        Microsoft::WRL::ComPtr<ID3D12Resource> res;
        D3D12_RESOURCE_DESC rd = {};
        rd.Dimension        = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        rd.Width            = desc.Width;
        rd.Height           = desc.Height;
        rd.DepthOrArraySize = 1;
        rd.MipLevels        = 1;
        rd.Format           = targetFormat;
        rd.SampleDesc       = {1, 0};
        rd.Layout           = D3D12_TEXTURE_LAYOUT_UNKNOWN;
        rd.Flags            = c.flags;

        HRESULT hr = d3d12Device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_SHARED, &rd,
                                                          D3D12_RESOURCE_STATE_COMMON, nullptr,
                                                          IID_PPV_ARGS(&res));
        if (FAILED(hr))
        {
            Msg("[FSR4] %s [%s]: CreateCommittedResource failed (hr=0x%08X, fmt=%d)",
                debugName, c.name, hr, targetFormat);
            continue;
        }

        HANDLE handle = nullptr;
        hr = d3d12Device->CreateSharedHandle(res.Get(), nullptr, GENERIC_ALL, nullptr, &handle);
        if (FAILED(hr))
        {
            Msg("[FSR4] %s [%s]: CreateSharedHandle failed (hr=0x%08X)", debugName, c.name, hr);
            continue;
        }

        Microsoft::WRL::ComPtr<ID3D11Texture2D> tex11;
        hr = d3d11Device1->OpenSharedResource1(handle, IID_PPV_ARGS(&tex11));
        CloseHandle(handle);
        if (FAILED(hr))
        {
            Msg("[FSR4] %s [%s]: OpenSharedResource1 failed (hr=0x%08X, fmt=%d)",
                debugName, c.name, hr, targetFormat);
            continue;
        }

        m_Res12               = res;
        m_Tex11               = tex11;
        m_SourcePtr           = gameTexture;
        m_Width               = desc.Width;
        m_Height              = desc.Height;
        m_SourceFormat        = desc.Format;
        m_SharedFormat        = targetFormat;
        m_IsDepthIntermediate = isDepthIntermediate;

        Msg("[FSR4] %s shared via %s (source fmt=%d, shared fmt=%d)",
            debugName, c.name, m_SourceFormat, m_SharedFormat);
        return true;
    }

    Msg("[FSR4] ALL sharing combos failed for %s (%ux%u source fmt=%d, target fmt=%d)",
        debugName, desc.Width, desc.Height, desc.Format, targetFormat);
    return false;
}

void Fsr4SharedTexture::Destroy()
{
    m_Tex11.Reset();
    m_Res12.Reset();
    m_SourcePtr           = nullptr;
    m_Width               = 0;
    m_Height              = 0;
    m_SourceFormat        = DXGI_FORMAT_UNKNOWN;
    m_SharedFormat        = DXGI_FORMAT_UNKNOWN;
    m_IsDepthIntermediate = false;
}