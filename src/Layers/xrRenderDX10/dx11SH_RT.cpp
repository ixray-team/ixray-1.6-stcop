#include "stdafx.h"

#include "../../xrRHI/RHITextureInterfaces.h"
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

	fmt = ERHI_FORMAT::UNKNOWN;
	pMippedRT.clear();
}

CRT::~CRT()
{
	destroy();

	// release external reference
	DEV->_DeleteRT(this);
}

void CRT::create(LPCSTR Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
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

	switch(fmt)
	{
		case ERHI_FORMAT::R24G8_TYPELESS:
		case ERHI_FORMAT::D24_UNORM_S8_UINT:
		case ERHI_FORMAT::R24_UNORM_X8_TYPELESS:
		{
			usage = D3DUSAGE_DEPTHSTENCIL;
			break;
		}
		case ERHI_FORMAT::D16_UNORM:
		{
			fmt = ERHI_FORMAT::R16_TYPELESS;
			usage = D3DUSAGE_DEPTHSTENCIL;
			break;
		}
		case ERHI_FORMAT::D32_FLOAT:
		{
			fmt = ERHI_FORMAT::R32_TYPELESS;
			usage = D3DUSAGE_DEPTHSTENCIL;
			break;
		}
	}

	bool bUseAsDepth = (usage != D3DUSAGE_RENDERTARGET);

	// Try to create texture/surface
	DEV->Evict();

	// Create the render target texture
	RHITextureDesc desc;
	ZeroMemory(&desc, sizeof(desc));

	desc.Width = dwWidth;
	desc.Height = dwHeight;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = fmt;
	desc.SampleDescCount = SampleCount;
	desc.Usage = (u32)ERHI_USAGE::USAGE_DEFAULT;

	if(SampleCount <= 1)
	{
		desc.BindFlags = D3D_BIND_SHADER_RESOURCE | (bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : D3D_BIND_RENDER_TARGET);
	}
	else
	{
		desc.BindFlags = bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : (D3D_BIND_SHADER_RESOURCE | D3D_BIND_RENDER_TARGET);
	}

	if(!bUseAsDepth)
	{
		if(CreationFlags & CRTCreationFlags::MIPPED_RT_FLAG)
		{
			auto dwSize = std::min(dwWidth, dwHeight);

			while((dwSize /= 2) >= 4)
			{
				++desc.MipLevels;
			}
		}

		if(SampleCount == 1 && CreationFlags & CRTCreationFlags::USE_UAV_FLAG)
		{
			desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;
		}
	}

	// Use GRHI to create the surface
	pSurface = GRHI->CreateRenderTarget(desc);

	if(bUseAsDepth)
	{
		RHIDepthStencilViewDesc	ViewDesc;

		ViewDesc.Format = ERHI_FORMAT::UNKNOWN;
		ViewDesc.MipSlice = 0;

		if(SampleCount <= 1)
		{
			ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2D;
		}
		else
		{
			ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2DMS;
		}

		switch(desc.Format)
		{
		case ERHI_FORMAT::R24G8_TYPELESS:
			ViewDesc.Format = ERHI_FORMAT::D24_UNORM_S8_UINT;
			break;
			case ERHI_FORMAT::R32_TYPELESS:
			ViewDesc.Format = ERHI_FORMAT::D32_FLOAT;
			break;
		}

		RHIDepthStencilViewDesc dsvDesc;
		dsvDesc.Format = ViewDesc.Format;
		dsvDesc.ViewDimension = ViewDesc.ViewDimension;
		dsvDesc.Flags = 0;
		dsvDesc.MipSlice = 0;
		dsvDesc.FirstArraySlice = 0;
		dsvDesc.ArraySize = 1;
		
		pZRT = GRHI->CreateDepthStencilView(pSurface, dsvDesc);
	}
	else
	{
		RHIRenderTargetViewDesc rtvDesc = {};
		rtvDesc.Format = fmt;
		rtvDesc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
		rtvDesc.MipSlice = 0;
		rtvDesc.FirstArraySlice = 0;
		rtvDesc.ArraySize = 1;
		
		// Convert TYPELESS formats for RTV
		switch(rtvDesc.Format)
		{
			case ERHI_FORMAT::R24G8_TYPELESS:
				rtvDesc.Format = ERHI_FORMAT::R24_UNORM_X8_TYPELESS;
				break;
			case ERHI_FORMAT::R32_TYPELESS:
				rtvDesc.Format = ERHI_FORMAT::R32_FLOAT;
				break;
			case ERHI_FORMAT::R16_TYPELESS:
				rtvDesc.Format = ERHI_FORMAT::R16_UNORM;
				break;
		}
		
		pRT = GRHI->CreateRenderTargetView(pSurface, rtvDesc);

		if(SampleCount == 1)
		{
			if(CreationFlags & CRTCreationFlags::USE_UAV_FLAG)
			{
				D3D11_UNORDERED_ACCESS_VIEW_DESC UAVDesc;
				ZeroMemory(&UAVDesc, sizeof(D3D11_UNORDERED_ACCESS_VIEW_DESC));

				UAVDesc.Format = (DXGI_FORMAT)fmt;
				UAVDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
				UAVDesc.Buffer.FirstElement = 0;
				UAVDesc.Buffer.NumElements = dwWidth * dwHeight;

				// UAV creation - keep DirectX specific for now
				CHK_DX(RDevice->CreateUnorderedAccessView((ID3D11Resource*)pSurface->GetRawTexture(), &UAVDesc, &pUAView));
			}
		}

		pMippedRT.resize(desc.MipLevels);
		pMippedRT[0] = pRT; pRT->AddRef();

		for(UINT mip_level = 1; mip_level < desc.MipLevels; ++mip_level)
		{
			// Create a copy of rtvDesc for each mip level
			RHIRenderTargetViewDesc mipRtvDesc = rtvDesc;
			mipRtvDesc.MipSlice = mip_level;
			pMippedRT[mip_level] = GRHI->CreateRenderTargetView(pSurface, mipRtvDesc);
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

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
	_set(DEV->_CreateRT(Name, w, h, f, SampleCount, CreationFlags));
}

CRTC::CRTC()
{
	if(pSurface) return;

	fmt = ERHI_FORMAT::UNKNOWN;
	dwSize = 0;

	ZeroMemory(pRT, sizeof(pRT));
	pSurface = nullptr;
}

CRTC::~CRTC()
{
	destroy();
	DEV->_DeleteRTC(this);
}

void CRTC::create(LPCSTR Name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags)
{
	R_ASSERT(RDevice && Name && Name[0] && size && btwIsPow2(size));
	_order = CPU::GetCLK();

	dwSize = size;
	fmt = f;

	// Check width-and-height of render target surface
	if(size > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;

	// Create the render target texture
	RHITextureDesc desc;
	ZeroMemory(&desc, sizeof(desc));

	desc.Width = dwSize;
	desc.Height = dwSize;

	desc.ArraySize = 6;

	desc.Format = fmt;
	desc.SampleDescCount = 1;

	desc.Usage = D3D_USAGE_DEFAULT;
	desc.BindFlags = D3D_BIND_SHADER_RESOURCE | D3D_BIND_RENDER_TARGET;
	desc.MiscFlags = D3D_RESOURCE_MISC_TEXTURECUBE;

	if(CreationFlags & CRT::CRTCreationFlags::MIPPED_RT_FLAG)
	{
		desc.MiscFlags |= D3D_RESOURCE_MISC_GENERATE_MIPS;
		desc.MipLevels = log2(dwSize) + 1;
	}

	pSurface = GRHI->CreateRenderTarget(desc);

	RHIRenderTargetViewDesc descRTV = {};
	descRTV.Format = fmt; // Use the converted format
	descRTV.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2DARRAY;
	descRTV.MipSlice = 0;
	descRTV.ArraySize = 1;
	
	// Convert TYPELESS formats for RTV
	switch(descRTV.Format)
	{
		case ERHI_FORMAT::R24G8_TYPELESS:
			descRTV.Format = ERHI_FORMAT::R24_UNORM_X8_TYPELESS;
			break;
		case ERHI_FORMAT::R32_TYPELESS:
			descRTV.Format = ERHI_FORMAT::R32_FLOAT;
			break;
		case ERHI_FORMAT::R16_TYPELESS:
			descRTV.Format = ERHI_FORMAT::R16_UNORM;
			break;
	}

	for(UINT i = 0; i < 6; i++)
	{
		// Create a copy for each face to avoid modifying the original
		RHIRenderTargetViewDesc faceRtvDesc = descRTV;
		faceRtvDesc.FirstArraySlice = i;
		pRT[i] = GRHI->CreateRenderTargetView(pSurface, faceRtvDesc);
	}

	pTexture = DEV->_CreateTexture(Name);
	pTexture->surface_set(pSurface);
}

void CRTC::destroy()
{
	pTexture->surface_set(0);
	pTexture = nullptr;

	for(UINT i = 0; i < 6; ++i)
	{
		_RELEASE(pRT[i]);
	}

	_RELEASE(pSurface);
}

void CRTC::reset_begin()
{
	destroy();
}

void CRTC::reset_end()
{
	create(*cName, dwSize, fmt);
}

void resptrcode_crtc::create(LPCSTR Name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags)
{
	_set(DEV->_CreateRTC(Name, size, f, CreationFlags));
}