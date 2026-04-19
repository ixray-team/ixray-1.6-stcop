#include "stdafx.h"


#include "../xrRender/HWCaps.h"
#include "../xrRender/HW.h"

ECORE_API CHWCaps Caps;

void CHWCaps::Update()
{
	D3DCAPS9 caps;
	RDevice->GetDeviceCaps	(&caps);

	// ***************** GEOMETRY
	DWORD cnt					= (caps.MaxVertexShaderConst);
	clamp<DWORD>(cnt,0,256);
	geometry.dwRegisters		= cnt;

	// ***************** PIXEL processing
	raster.dwStages				= caps.MaxSimultaneousTextures;

	bTableFog			=	false;	//bool	(caps.RasterCaps&D3DPRASTERCAPS_FOGTABLE);

	// Detect if stencil available
	bStencil			=	false;
	IDirect3DSurface9*	surfZS=nullptr;
	D3DSURFACE_DESC		surfDESC;
	CHK_DX		(RDevice->GetDepthStencilSurface(&surfZS));
	R_ASSERT	(surfZS);
	CHK_DX		(surfZS->GetDesc(&surfDESC));
	_RELEASE	(surfZS);

	switch		(surfDESC.Format)
	{
	case D3DFMT_D15S1:		bStencil = true;	break;
	case D3DFMT_D24S8:		bStencil = true;	break;
	case D3DFMT_D24X4S4:	bStencil = true;	break;
	}

	// Scissoring
	if (caps.RasterCaps & D3DPRASTERCAPS_SCISSORTEST)	bScissor	= true;
	else												bScissor	= false;

	// DEV INFO
	iGPUNum = GRHI->DriverExt->GetGPUCount();
}