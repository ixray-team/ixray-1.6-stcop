#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/ResourceManager.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "dx10TextureUtils.h"

CRT::CRT()
{
    pSurface = NULL;
    pRT = NULL;
    pZRT = NULL;
    pUAView = NULL;
    dwWidth = 0;
    dwHeight = 0;
    fmt = DxgiFormat::DXGI_FORMAT_UNKNOWN;
}
CRT::~CRT()
{
    destroy();

    // release external reference
    DEV->_DeleteRT(this);
}

void CRT::create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, bool useUAV)
{
    if (pSurface)	return;

    R_ASSERT(HW.pDevice && Name && Name[0] && w && h);
    _order = CPU::GetCLK();

    dwWidth = w;
    dwHeight = h;
    fmt = f;

    // Check width-and-height of render target surface
    if (w > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION)		return;
    if (h > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION)		return;

    // Select usage
    u32 usage = 0;
    if (DxgiFormat::DXGI_FORMAT_D24_UNORM_S8_UINT == fmt)		    usage = D3DUSAGE_DEPTHSTENCIL;
    else if (DxgiFormat::DXGI_FORMAT_R24_UNORM_X8_TYPELESS == fmt)	usage = D3DUSAGE_DEPTHSTENCIL;
    else if (DxgiFormat::DXGI_FORMAT_D16_UNORM == fmt) {
        usage = D3DUSAGE_DEPTHSTENCIL;
        fmt = DxgiFormat::DXGI_FORMAT_R16_TYPELESS;
    }
    else if (DxgiFormat::DXGI_FORMAT_D32_FLOAT == fmt) {
        usage = D3DUSAGE_DEPTHSTENCIL;
        fmt = DxgiFormat::DXGI_FORMAT_R32_TYPELESS;
    }
    else if (DxgiFormat::DXGI_FORMAT_R24G8_TYPELESS == fmt)			usage = D3DUSAGE_DEPTHSTENCIL;
    else														    usage = D3DUSAGE_RENDERTARGET;

    DXGI_FORMAT dx10FMT = (DXGI_FORMAT)fmt;
    bool bUseAsDepth = (usage == D3DUSAGE_RENDERTARGET) ? false : true;

    // Try to create texture/surface
    DEV->Evict();

    // Create the render target texture
    D3D_TEXTURE2D_DESC desc;
    ZeroMemory(&desc, sizeof(desc));
    desc.Width = dwWidth;
    desc.Height = dwHeight;
    desc.MipLevels = 1;
    desc.ArraySize = 1;
    desc.Format = dx10FMT;
    desc.SampleDesc.Count = SampleCount;
    desc.Usage = D3D_USAGE_DEFAULT;
    if (SampleCount <= 1)
        desc.BindFlags = D3D_BIND_SHADER_RESOURCE | (bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : D3D_BIND_RENDER_TARGET);
    else {
        desc.BindFlags = (bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : (D3D_BIND_SHADER_RESOURCE | D3D_BIND_RENDER_TARGET));
    }

    if (HW.FeatureLevel >= D3D_FEATURE_LEVEL_11_0 && !bUseAsDepth && SampleCount == 1 && useUAV)
        desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;

    CHK_DX(HW.pDevice->CreateTexture2D(&desc, NULL, &pSurface));
    HW.stats_manager.increment_stats_rtarget(pSurface);
    // OK
#ifdef DEBUG
    Msg("* created RT(%s), %dx%d, format = %d samples = %d", Name, w, h, dx10FMT, SampleCount);
#endif // DEBUG
    //R_CHK		(pSurface->GetSurfaceLevel	(0,&pRT));
    if (bUseAsDepth)
    {
        D3D_DEPTH_STENCIL_VIEW_DESC	ViewDesc;
        ZeroMemory(&ViewDesc, sizeof(ViewDesc));

        ViewDesc.Format = DXGI_FORMAT_UNKNOWN;
        if (SampleCount <= 1)
        {
            ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2D;
        }
        else
        {
            ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2DMS;
            ViewDesc.Texture2DMS.UnusedField_NothingToDefine = 0;
        }

        ViewDesc.Texture2D.MipSlice = 0;
        switch (desc.Format)
        {
        case DXGI_FORMAT_R24G8_TYPELESS:
            ViewDesc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
            break;
        case DXGI_FORMAT_R32_TYPELESS:
            ViewDesc.Format = DXGI_FORMAT_D32_FLOAT;
            break;    
        case DXGI_FORMAT_R16_TYPELESS:
            ViewDesc.Format = DXGI_FORMAT_D16_UNORM;
            break;
        }

        CHK_DX(HW.pDevice->CreateDepthStencilView(pSurface, &ViewDesc, &pZRT));
    }
    else
        CHK_DX(HW.pDevice->CreateRenderTargetView(pSurface, 0, &pRT));

    if (HW.FeatureLevel >= D3D_FEATURE_LEVEL_11_0 && !bUseAsDepth && SampleCount == 1 && useUAV)
    {
        D3D11_UNORDERED_ACCESS_VIEW_DESC UAVDesc;
        ZeroMemory(&UAVDesc, sizeof(D3D11_UNORDERED_ACCESS_VIEW_DESC));
        UAVDesc.Format = dx10FMT;
        UAVDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
        UAVDesc.Buffer.FirstElement = 0;
        UAVDesc.Buffer.NumElements = dwWidth * dwHeight;
        CHK_DX(HW.pDevice->CreateUnorderedAccessView(pSurface, &UAVDesc, &pUAView));
    }

    pTexture = DEV->_CreateTexture(Name);
    pTexture->surface_set(pSurface);
}

void CRT::destroy()
{
    if (pTexture._get()) {
        pTexture->surface_set(0);
        pTexture = NULL;
    }
    _RELEASE(pRT);
    _RELEASE(pZRT);

    HW.stats_manager.decrement_stats_rtarget(pSurface);
    _RELEASE(pSurface);
    _RELEASE(pUAView);
}

void CRT::reset_begin()
{
    destroy();
}

void CRT::reset_end()
{
    create(*cName, dwWidth, dwHeight, fmt);
}

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, bool useUAV)
{
    _set(DEV->_CreateRT(Name, w, h, f, SampleCount, useUAV));
}
