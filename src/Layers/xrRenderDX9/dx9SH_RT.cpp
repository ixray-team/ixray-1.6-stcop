#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/ResourceManager.h"

#include "../xrRender/dxRenderDeviceRender.h"

CRT::CRT			()
{
	pSurface		= nullptr;
	pRT				= nullptr;
	dwWidth			= 0;
	dwHeight		= 0;
	fmt				= FMT_UNKNOWN;
}
CRT::~CRT			()
{
	destroy			();

	// release external reference
	DEV->_DeleteRT	(this);
}

void CRT::create	(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount )
{
	if (pSurface)	return;

	R_ASSERT	(RDevice && Name && Name[0] && w && h);
	_order		= CPU::GetCLK()	;	//RDEVICE.GetTimerGlobal()->GetElapsed_clk();

	HRESULT		_hr;

	dwWidth		= w;
	dwHeight	= h;
	fmt			= f;

	// Get caps
	D3DCAPS9	caps{};
	R_CHK		(RDevice->GetDeviceCaps(&caps));

	// Check width-and-height of render target surface
	if (w>caps.MaxTextureWidth)			return;
	if (h>caps.MaxTextureHeight)		return;

	// Select usage
	u32 usage = FMT_D24X8 == fmt ? D3DUSAGE_DEPTHSTENCIL : D3DUSAGE_RENDERTARGET;

	// Try to create texture/surface
	DEV->Evict();

	if (!RHIUtils::CreateTexture(w, h, 1, usage, f, true, &pSurface, nullptr) || (0 == pSurface))
	{
		Msg("Cannot create surface for %s", Name);
		return;
	}

	// OK
#ifdef DEBUG
	Msg			("* created RT(%s), %dx%d",Name,w,h);
#endif // DEBUG
	R_ASSERT(pSurface->GetSurfaceLevel	(0,&pRT));
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

void CRT::reset_end		()
{
	create		(*cName,dwWidth,dwHeight,fmt);
}

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount )
{
	_set			(DEV->_CreateRT(Name,w,h,f));
}