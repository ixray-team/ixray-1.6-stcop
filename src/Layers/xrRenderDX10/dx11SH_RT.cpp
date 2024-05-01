#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/ResourceManager.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "dx10TextureUtils.h"

CRT::CRT()
{
	pSurface = nullptr;
	pRT = nullptr;
	pZRT = nullptr;
	pUAView = nullptr;
	dwWidth = 0;
	dwHeight = 0;
	fmt = FMT_UNKNOWN;
}

CRT::~CRT()
{
	destroy();

	// release external reference
	DEV->_DeleteRT(this);
}

void CRT::create(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount, bool useUAV)
{
	if (pSurface)	return;

	R_ASSERT(RDevice && Name && Name[0] && w && h);
	_order = CPU::GetCLK();

	dwWidth = w;
	dwHeight = h;
	fmt = f;

	// Check width-and-height of render target surface
	if (w > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION)		return;
	if (h > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION)		return;

	// Select usage
	u32 usage = 0;
	if (FMT_D24S8 == fmt)
		usage = eUsageDepthStencil;
	else if (FMT_D24X8 == fmt)
		usage = eUsageDepthStencil;
	else
		usage = eUsageRenderTarget;

	// To ForserX: DXGI_FORMAT_D32_FLOAT pls

	//u32 usage = 0;
	//if (DxgiFormat::DXGI_FORMAT_D24_UNORM_S8_UINT == fmt)		    usage = D3DUSAGE_DEPTHSTENCIL;
	//else if (DxgiFormat::DXGI_FORMAT_R24_UNORM_X8_TYPELESS == fmt)	usage = D3DUSAGE_DEPTHSTENCIL;
	//else if (DxgiFormat::DXGI_FORMAT_D16_UNORM == fmt) {
	//	usage = D3DUSAGE_DEPTHSTENCIL;
	//	fmt = DxgiFormat::DXGI_FORMAT_R16_TYPELESS;
	//}
	//else if (DxgiFormat::DXGI_FORMAT_D32_FLOAT == fmt) {
	//	usage = D3DUSAGE_DEPTHSTENCIL;
	//	fmt = DxgiFormat::DXGI_FORMAT_R32_TYPELESS;
	//}
	//else if (DxgiFormat::DXGI_FORMAT_R24G8_TYPELESS == fmt)			usage = D3DUSAGE_DEPTHSTENCIL;
	//else														    usage = D3DUSAGE_RENDERTARGET;

	bool bUseAsDepth = (usage == eUsageRenderTarget) ? false : true;

	// Try to create texture/surface
	DEV->Evict();

	// to forserx: desc.SampleDesc.Count = SampleCount !!!

	// Create the render target texture
	TextureDesc desc;
	desc.Width = dwWidth;
	desc.Height = dwHeight;
	desc.NumMips = 1;
	desc.DepthOrSliceNum = 1;
	desc.Format = fmt;

	if (bUseAsDepth)
		desc.Usage = eUsageDefault | eUsageDepthStencil;
	else
		desc.Usage = eUsageDefault | eUsageRenderTarget;
	
	pSurface = g_RenderRHI->CreateAPITexture(&desc, nullptr);

	// #TODO : TO THINK !!!
	//if (!bUseAsDepth && SampleCount == 1 && useUAV )
	//	desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;

	// OK
#ifdef DEBUG
	Msg("* created RT(%s), %dx%d, format = %d samples = %d", Name, w, h, fmt, SampleCount);
#endif // DEBUG

	if (bUseAsDepth)
	{
		//pZRT = g_RenderRHI->CreateAPIDepthStencilSurface(dwWidth, dwHeight, fmt, SampleCount, 1, false);
		pZRT = g_RenderRHI->CreateAPIDepthStencilView(pSurface, nullptr);
		R_ASSERT(pZRT);
	}
	else
	{
		pRT = g_RenderRHI->CreateAPIRenderTargetView(pSurface, nullptr);
	}

	if (!bUseAsDepth && SampleCount == 1 && useUAV)
	{
		RHI_UNORDERED_ACCESS_VIEW_DESC UAVDesc;
		ZeroMemory(&UAVDesc, sizeof(RHI_UNORDERED_ACCESS_VIEW_DESC));
		UAVDesc.Format = fmt;
		UAVDesc.ViewDimension = RHI_UAV_DIMENSION_TEXTURE2D;
		UAVDesc.Buffer.FirstElement = 0;
		UAVDesc.Buffer.NumElements = dwWidth * dwHeight;
		pUAView = g_RenderRHI->CreateAPIUnorderedAccessView(pSurface, UAVDesc);
	}

	pTexture = DEV->_CreateTexture(Name);
	pTexture->surface_set(pSurface);
}

void CRT::destroy()
{
	if (pTexture._get()) {
		pTexture->surface_set(0);
		pTexture = nullptr;
	}
	_RELEASE(pRT);
	_RELEASE(pZRT);

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

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount, bool useUAV)
{
	_set(DEV->_CreateRT(Name, w, h, f, SampleCount, useUAV));
}
