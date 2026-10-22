#include "stdafx.h"
#include "R_Backend_LOD.h"
#include "dx10FixedConstants.h"
#include "xrRender_console.h"

R_LOD::R_LOD()
{
	unmap();
}

void	R_LOD::set_LOD(float LOD)
{
	float factor = clampr<float>(ceil(LOD*LOD*LOD*LOD*LOD*8.0f), 1, 7);

	// The tessellation factor is capped by a console variable: see ps_r4_tess_max.
	if ((float)ps_r4_tess_max < factor)
		factor = (float)ps_r4_tess_max;

	if (c_LOD)
		RCache.set_c(c_LOD, factor);
	FixedConstants::SetTriLOD(factor);
}