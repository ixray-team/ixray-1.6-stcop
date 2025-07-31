#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/ResourceManager.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "dx10TextureUtils.h"

CRT::CRT() {
	pSurface = nullptr;
	pRT = nullptr;
	pZRT = nullptr;
	pUAView = nullptr;

	dwWidth = 0;
	dwHeight = 0;

	fmt = DxgiFormat::DXGI_FORMAT_UNKNOWN;
	pMippedRT.clear();
}

CRT::~CRT() {
	destroy();

	// release external reference
	DEV->_DeleteRT(this);
}

void CRT::create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags) {
	if(pSurface) return;
	PROF_EVENT("CRT::create");
	R_ASSERT(RDevice && Name && Name[0] && w && h);
	_order = CPU::GetCLK();

	dwWidth = w;
	dwHeight = h;
	fmt = f;

	// Check width-and-height of render target surface
	if(w > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;
	if(h > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;

	// Select usage
	u32 usage = D3DUSAGE_RENDERTARGET;

	switch(fmt) {
		case DxgiFormat::DXGI_FORMAT_R24G8_TYPELESS:
		case DxgiFormat::DXGI_FORMAT_D24_UNORM_S8_UINT:
		case DxgiFormat::DXGI_FORMAT_R24_UNORM_X8_TYPELESS:
		{
			usage = D3DUSAGE_DEPTHSTENCIL;
			break;
		}
		case DxgiFormat::DXGI_FORMAT_D16_UNORM:
		{
			fmt = DxgiFormat::DXGI_FORMAT_R16_TYPELESS;
			usage = D3DUSAGE_DEPTHSTENCIL;
			break;
		}
		case DxgiFormat::DXGI_FORMAT_D32_FLOAT:
		{
			fmt = DxgiFormat::DXGI_FORMAT_R32_TYPELESS;
			usage = D3DUSAGE_DEPTHSTENCIL;
			break;
		}
	}

	DXGI_FORMAT dx10FMT = (DXGI_FORMAT)fmt;
	bool bUseAsDepth = (usage != D3DUSAGE_RENDERTARGET);

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

	if(SampleCount <= 1) {
		desc.BindFlags = D3D_BIND_SHADER_RESOURCE | (bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : D3D_BIND_RENDER_TARGET);
	}
	else {
		desc.BindFlags = bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : (D3D_BIND_SHADER_RESOURCE | D3D_BIND_RENDER_TARGET);
	}

	if(!bUseAsDepth) {
		if(CreationFlags & CRTCreationFlags::MIPPED_RT_FLAG) {
			auto dwSize = std::min(dwWidth, dwHeight);

			while((dwSize /= 2) >= 4) {
				++desc.MipLevels;
			}
		}

		if(SampleCount == 1 && CreationFlags & CRTCreationFlags::USE_UAV_FLAG) {
			desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;
		}
	}

	CHK_DX(RDevice->CreateTexture2D(&desc, nullptr, &pSurface));

#ifdef DEBUG
	Msg("* created RT(%s), %dx%d, format = %d samples = %d", Name, w, h, dx10FMT, SampleCount);
#endif

	if(bUseAsDepth) {
		D3D_DEPTH_STENCIL_VIEW_DESC	ViewDesc;
		ZeroMemory(&ViewDesc, sizeof(ViewDesc));

		ViewDesc.Format = DXGI_FORMAT_UNKNOWN;
		ViewDesc.Texture2D.MipSlice = 0;

		if(SampleCount <= 1) {
			ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2D;
		}
		else {
			ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2DMS;
			ViewDesc.Texture2DMS.UnusedField_NothingToDefine = 0;
		}

		switch(desc.Format) {
			case DXGI_FORMAT_R24G8_TYPELESS:
			ViewDesc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
			break;
			case DXGI_FORMAT_R32_TYPELESS:
			ViewDesc.Format = DXGI_FORMAT_D32_FLOAT;
			break;
		}

		CHK_DX(RDevice->CreateDepthStencilView(pSurface, &ViewDesc, &pZRT));
	}
	else {
		D3D_RENDER_TARGET_VIEW_DESC descRTV{};
		ZeroMemory(&descRTV, sizeof(descRTV));

		descRTV.Format = desc.Format;
		descRTV.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
		descRTV.Texture2D.MipSlice = 0;

		CHK_DX(RDevice->CreateRenderTargetView(pSurface, &descRTV, &pRT));

		if(SampleCount == 1) {
			if(CreationFlags & CRTCreationFlags::USE_UAV_FLAG) {
				D3D11_UNORDERED_ACCESS_VIEW_DESC UAVDesc;
				ZeroMemory(&UAVDesc, sizeof(D3D11_UNORDERED_ACCESS_VIEW_DESC));

				UAVDesc.Format = dx10FMT;
				UAVDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
				UAVDesc.Buffer.FirstElement = 0;
				UAVDesc.Buffer.NumElements = dwWidth * dwHeight;

				CHK_DX(RDevice->CreateUnorderedAccessView(pSurface, &UAVDesc, &pUAView));
			}
		}

		pMippedRT.resize(desc.MipLevels);
		pMippedRT[0] = pRT; pRT->AddRef();

		for(UINT mip_level = 1; mip_level < desc.MipLevels; ++mip_level) {
			descRTV.Texture2D.MipSlice = mip_level;
			CHK_DX(RDevice->CreateRenderTargetView(pSurface, &descRTV, &pMippedRT[mip_level]));
		}
	}

	pTexture = DEV->_CreateTexture(Name);
	pTexture->surface_set(pSurface);
}

void CRT::destroy()
{
	if(pTexture._get()) {
		pTexture->surface_set(0);
		pTexture = nullptr;
	}

	_RELEASE(pRT);
	_RELEASE(pZRT);

	_RELEASE(pSurface);
	_RELEASE(pUAView);

	for(auto& MippedRT : pMippedRT) {
		_RELEASE(MippedRT);
	}

	pMippedRT.clear();
}

void CRT::reset_begin() {
	destroy();
}

void CRT::reset_end() {
	create(*cName, dwWidth, dwHeight, fmt);
}

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags) {
	_set(DEV->_CreateRT(Name, w, h, f, SampleCount, CreationFlags));
}

CRTC::CRTC()
{
	if(pSurface) return;

	fmt = DxgiFormat::DXGI_FORMAT_UNKNOWN;
	dwSize = 0;

	ZeroMemory(pRT, sizeof(pRT));
	pSurface = nullptr;
}

CRTC::~CRTC()
{
	destroy();
	DEV->_DeleteRTC(this);
}

void CRTC::create(LPCSTR Name, u32 size, DxgiFormat f, CRT::CRTCreationFlags CreationFlags)
{
	R_ASSERT(RDevice && Name && Name[0] && size && btwIsPow2(size));
	_order = CPU::GetCLK();

	dwSize = size;
	fmt = f;

	// Check width-and-height of render target surface
	if(size > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;

	// Create the render target texture
	D3D_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));

	desc.Width = dwSize;
	desc.Height = dwSize;

	desc.ArraySize = 6;

	desc.Format = (DXGI_FORMAT)fmt;
	desc.SampleDesc.Count = 1;

	desc.Usage = D3D_USAGE_DEFAULT;
	desc.BindFlags = D3D_BIND_SHADER_RESOURCE | D3D_BIND_RENDER_TARGET;
	desc.MiscFlags = D3D_RESOURCE_MISC_TEXTURECUBE;

	if(CreationFlags & CRT::CRTCreationFlags::MIPPED_RT_FLAG) {
		desc.MiscFlags |= D3D_RESOURCE_MISC_GENERATE_MIPS;
		desc.MipLevels = log2(dwSize) + 1;
	}

	CHK_DX(RDevice->CreateTexture2D(&desc, nullptr, &pSurface));

	D3D_RENDER_TARGET_VIEW_DESC descRTV{};
	ZeroMemory(&descRTV, sizeof(descRTV));

	descRTV.Format = desc.Format;
	descRTV.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2DARRAY;
	descRTV.Texture2D.MipSlice = 0;
	descRTV.Texture2DArray.ArraySize = 1;

	for(UINT i = 0; i < 6; i++)
	{
		descRTV.Texture2DArray.FirstArraySlice = i;
		CHK_DX(RDevice->CreateRenderTargetView(pSurface, &descRTV, &pRT[i]));
	}

	pTexture = DEV->_CreateTexture(Name);
	pTexture->surface_set(pSurface);
}

void CRTC::destroy()
{
	pTexture->surface_set(0);
	pTexture = nullptr;

	for(UINT i = 0; i < 6; ++i) {
		_RELEASE(pRT[i]);
	}

	_RELEASE(pSurface);
}

void CRTC::reset_begin() {
	destroy();
}

void CRTC::reset_end() {
	create(*cName, dwSize, fmt);
}

void resptrcode_crtc::create(LPCSTR Name, u32 size, DxgiFormat f, CRT::CRTCreationFlags CreationFlags) {
	_set(DEV->_CreateRTC(Name, size, f, CreationFlags));
}