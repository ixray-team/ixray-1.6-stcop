#include "stdafx.h"


#include "../xrRender/HWCaps.h"
#include "../xrRender/HW.h"

ECORE_API CHWCaps Caps;

void CHWCaps::Update()
{
	// ***************** GEOMETRY
	DWORD cnt = 256;
	clamp<DWORD>(cnt, 0, 256);
	geometry.dwRegisters = cnt;

	// ***************** PIXEL processing
	raster.dwStages = 15; //unused

	bTableFog = FALSE;	//bool	(caps.RasterCaps&D3DPRASTERCAPS_FOGTABLE);

	// Detect if stencil available
	bStencil = TRUE;

	// Scissoring
	bScissor = TRUE;

	// DEV INFO

	iGPUNum = GRHI->DriverExt->GetGPUCount();;
}