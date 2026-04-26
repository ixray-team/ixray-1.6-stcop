#include "stdafx.h"

#include "ResourceManager.h"
#include "dxRenderDeviceRender.h"

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

void CRT::create(const char* Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
	if(pSurface) return;
	PROF_EVENT("CRT::create");
	R_ASSERT(RDevice && Name && Name[0] && w && h);
	_order = CPU::GetCLK();

	dwWidth = w;
	dwHeight = h;
	fmt = f;

	// Check width-and-height of render target surface
	if(w > RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;
	if(h > RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;

	// Select usage
	bool UsageDepth = false;

	switch(fmt)
	{
		case ERHI_FORMAT::R24G8_TYPELESS:
		case ERHI_FORMAT::D24_UNORM_S8_UINT:
		case ERHI_FORMAT::R24_UNORM_X8_TYPELESS:
		{
			fmt = ERHI_FORMAT::R24G8_TYPELESS;
			UsageDepth = true;
			break;
		}
		case ERHI_FORMAT::D16_UNORM:
		{
			fmt = ERHI_FORMAT::R16_TYPELESS;
			UsageDepth = true;
			break;
		}
		case ERHI_FORMAT::D32_FLOAT:
		{
			fmt = ERHI_FORMAT::R32_TYPELESS;
			UsageDepth = true;
			break;
		}
	}

	// Try to create texture/surface
	DEV->Evict();

	// Create the render target texture
	RHITextureDesc desc;
	desc.Width = dwWidth;
	desc.Height = dwHeight;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = fmt;
	desc.SampleDescCount = SampleCount;

	if(SampleCount <= 1)
	{
		desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE | (UsageDepth ? ERHI_BIND_FLAG::DEPTH_STENCIL : ERHI_BIND_FLAG::RENDER_TARGET);
	}
	else
	{
		desc.BindFlags = UsageDepth ? ERHI_BIND_FLAG::DEPTH_STENCIL : (ERHI_BIND_FLAG::SHADER_RESOURCE | ERHI_BIND_FLAG::RENDER_TARGET);
	}

	if(!UsageDepth)
	{
		if(CreationFlags & CRTCreationFlags::MIPPED_RT_FLAG)
		{
			auto dwSize = std::min(dwWidth, dwHeight);

			while ((dwSize /= 2) >= 4)
			{
				++desc.MipLevels;
			}

			// It is convenient to have 
			// an even number of MIP levels.

			if (desc.MipLevels % 2 == 0) 
			{
				++desc.MipLevels;
			}
		}

		if (CreationFlags & CRT::CRTCreationFlags::AUTOGEN_MIP_MAPS)
		{
			desc.MiscFlags |= D3D_RESOURCE_MISC_GENERATE_MIPS;
			desc.MipLevels = 0;
		}

		if (SampleCount == 1 && CreationFlags & CRTCreationFlags::USE_UAV_FLAG)
		{
			desc.BindFlags |= ERHI_BIND_FLAG::UNORDERED_ACCESS;
		}
	}

	// Use GRHI to create the surface
	pSurface = GRHI->CreateRenderTarget(desc);

	if(UsageDepth)
	{
		RHIDepthStencilViewDesc	ViewDesc;

		ViewDesc.Format = ERHI_FORMAT::UNKNOWN;
		ViewDesc.MipSlice = 0;

		if(SampleCount <= 1)
		{
			ViewDesc.ViewDimension = ERHI_DSV_DIMENSION::TEXTURE2D;
		}
		else
		{
			ViewDesc.ViewDimension = ERHI_DSV_DIMENSION::TEXTURE2DMS;
		}

		switch(desc.Format)
		{
		case ERHI_FORMAT::R24G8_TYPELESS:
			ViewDesc.Format = ERHI_FORMAT::D24_UNORM_S8_UINT;
			break;
		case ERHI_FORMAT::R32_TYPELESS:
			ViewDesc.Format = ERHI_FORMAT::D32_FLOAT;
			break;
		case ERHI_FORMAT::R16_TYPELESS:
			ViewDesc.Format = ERHI_FORMAT::D16_UNORM;
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
		rtvDesc.ViewDimension = ERHI_RTV_DIMENSION::TEXTURE2D;
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
				RHIUAVDesc uavDesc = {};

				uavDesc.Format = fmt;
				uavDesc.ViewDimension = ERHI_VIEW_DIMENSION::Texture2D;
				uavDesc.NumElements = dwWidth * dwHeight;

				pUAView = GRHI->CreateUAV(pSurface, uavDesc);
			}

			pMippedRT.resize(0);

			if (CreationFlags & CRTCreationFlags::MIPPED_RT_FLAG)
			{
				pMippedRT.resize(desc.MipLevels);
				pMippedRT[0] = pRT; pRT->AddRef();
			}
		}

		for(UINT mip_level = 1, count = pMippedRT.size(); mip_level < count; ++mip_level)
		{
			rtvDesc.MipSlice = mip_level;
			pMippedRT[mip_level] = GRHI->CreateRenderTargetView(pSurface, rtvDesc);
		}
	}

	pTexture = DEV->_CreateTexture(Name);
	pTexture->surface_set(pSurface);
}

void CRT::destroy()
{
	if (pTexture._get())
	{
		pTexture->surface_set(nullptr);
		pTexture = nullptr;
	}

	_RELEASE(pRT);
	_RELEASE(pZRT);

	_RELEASE(pSurface);
	_RELEASE(pUAView);

	for (IRHIRenderTargetView* MippedRT : pMippedRT)
	{
		_RELEASE(MippedRT);
	}

	pMippedRT.clear();
}

void CRT::reset_begin()
{
	destroy();
}

void CRT::reset_end()
{
	create(*cName, dwWidth, dwHeight, fmt);
}

void resptrcode_crt::create(const char* Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
	_set(DEV->_CreateRT(Name, w, h, f, SampleCount, CreationFlags));
}

#ifdef USE_DX11
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

void CRTC::create(const char* Name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags)
{
	R_ASSERT(RDevice && Name && Name[0] && size && btwIsPow2(size));
	_order = CPU::GetCLK();

	dwSize = size;
	fmt = f;

	// Check width-and-height of render target surface
	if(size > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;

	// Create the render target texture
	RHITextureDesc desc;
	desc.Width = dwSize;
	desc.Height = dwSize;

	desc.ArraySize = 6;

	desc.Format = fmt;
	desc.SampleDescCount = 1;

	desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE | ERHI_BIND_FLAG::RENDER_TARGET;
	desc.MiscFlags = D3D_RESOURCE_MISC_TEXTURECUBE;

	if(CreationFlags & CRT::CRTCreationFlags::MIPPED_RT_FLAG)
	{
		desc.MipLevels = log2(dwSize) + 1;
	}

	if(CreationFlags & CRT::CRTCreationFlags::AUTOGEN_MIP_MAPS)
	{
		desc.MiscFlags |= D3D_RESOURCE_MISC_GENERATE_MIPS;
	}

	pSurface = GRHI->CreateRenderTarget(desc);

	RHIRenderTargetViewDesc descRTV = {};
	descRTV.Format = fmt; // Use the converted format
	descRTV.ViewDimension = ERHI_RTV_DIMENSION::TEXTURE2DARRAY;
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

void resptrcode_crtc::create(const char* Name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags)
{
	_set(DEV->_CreateRTC(Name, size, f, CreationFlags));
}
#endif