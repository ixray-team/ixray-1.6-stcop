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

	bTableFog = false;	//bool	(caps.RasterCaps&D3DPRASTERCAPS_FOGTABLE);

	// Detect if stencil available
	bStencil = true;

	// Scissoring
	bScissor = true;

	// DEV INFO

	if (GRHI->DriverExt != nullptr)
	{
		iGPUNum = GRHI->DriverExt->GetGPUCount();
	}
}