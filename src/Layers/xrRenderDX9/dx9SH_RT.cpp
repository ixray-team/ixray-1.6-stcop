#include "stdafx.h"


#include "../xrRender/ResourceManager.h"

#include "../xrRender/dxRenderDeviceRender.h"

CRT::CRT			()
{
	pSurface		= nullptr;
	pRT				= nullptr;
	dwWidth			= 0;
	dwHeight		= 0;
	fmt				= ERHI_FORMAT::UNKNOWN;
}
CRT::~CRT			()
{
	destroy			();

	// release external reference
	DEV->_DeleteRT	(this);
}

void CRT::create(LPCSTR Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
	if (pSurface)	return;

	R_ASSERT	(RDevice && Name && Name[0] && w && h);
	_order		= CPU::GetCLK()	;

	HRESULT		_hr;

	dwWidth		= w;
	dwHeight	= h;
	fmt			= f;

	// Check width-and-height of render target surface
	if (w > RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;
	if (h > RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION) return;

	// Select usage
	u32 usage = D3DUSAGE_RENDERTARGET;

	switch (fmt)
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

	DEV->Evict				();
	
	// Create RHITextureDesc for the render target
	RHITextureDesc rhiDesc;
	rhiDesc.Width = w;
	rhiDesc.Height = h;
	rhiDesc.Depth = 1;
	rhiDesc.MipLevels = 1;
	rhiDesc.Format = f;
	rhiDesc.Usage = usage;
	rhiDesc.BindFlags = 0;
	rhiDesc.CPUAccessFlags = 0;
	rhiDesc.MiscFlags = 0;
	
	// Use GRHI to create the surface
	IRHISurface* rhiSurface = GRHI->CreateRenderTarget(rhiDesc);
	if (!rhiSurface)
	{
		Msg("Cannot create surface for %s", Name);
		return;
	}
	
	// Store the RHI surface
	pSurface = rhiSurface;
	
	// Create RHI render target view
	pRT = GRHI->CreateRenderTargetView(rhiSurface);
	pTexture	= DEV->_CreateTexture	(Name);
	pTexture->surface_set	(pSurface);
}

void CRT::destroy		()
{
	if (pTexture._get())	{
		pTexture->surface_set	(0);
		pTexture				= nullptr;
	}
	
	_RELEASE	(pRT		);

	_RELEASE	(pSurface	);
}

void CRT::reset_begin	()
{
	destroy		();
}

void CRT::reset_end()
{
	create(*cName, dwWidth, dwHeight, fmt);
}

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
	_set(DEV->_CreateRT(Name, w, h, f));
}