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

	bTableFog			=	FALSE;	//BOOL	(caps.RasterCaps&D3DPRASTERCAPS_FOGTABLE);

	// Detect if stencil available
	bStencil			=	FALSE;
	IDirect3DSurface9*	surfZS=0;
	D3DSURFACE_DESC		surfDESC;
	CHK_DX		(RDevice->GetDepthStencilSurface(&surfZS));
	R_ASSERT	(surfZS);
	CHK_DX		(surfZS->GetDesc(&surfDESC));
	_RELEASE	(surfZS);

	switch		(surfDESC.Format)
	{
	case D3DFMT_D15S1:		bStencil = TRUE;	break;
	case D3DFMT_D24S8:		bStencil = TRUE;	break;
	case D3DFMT_D24X4S4:	bStencil = TRUE;	break;
	}

	// Scissoring
	if (caps.RasterCaps & D3DPRASTERCAPS_SCISSORTEST)	bScissor	= TRUE;
	else												bScissor	= FALSE;

	// DEV INFO
	iGPUNum = GRHI->DriverExt->GetGPUCount();
}