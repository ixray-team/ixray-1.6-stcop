#include "stdafx.h"
#include "R_Backend_LOD.h"
#include "../xrRenderDX10/dx10FixedConstants.h"

R_LOD::R_LOD()
{
	unmap();
}

void	R_LOD::set_LOD(float LOD)
{
	float factor = clampr<float>(ceil(LOD*LOD*LOD*LOD*LOD*8.0f), 1, 7);
	if (c_LOD)
		RCache.set_c(c_LOD, factor);
	FixedConstants::SetTriLOD(factor);
}