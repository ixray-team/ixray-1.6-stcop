#include "stdafx.h"
#include "R_Backend_LOD.h"
#include "dx10FixedConstants.h"
#include "xrRender_console.h"

R_LOD::R_LOD()
{
	unmap();
}

void	R_LOD::set_LOD(float LOD, float dist)
{
	float factor = clampr<float>(ceil(LOD*LOD*LOD*LOD*LOD*8.0f), 1, 7);

	// Tessellation is worth its cost only close to the camera: beyond r4_tess_dist_far the
	// factor drops to 1, which DXMT renders as a flat patch, i.e. no subdivision at all.
	// Windows and Linux keep the upstream behaviour: their hardware tessellator is cheap.
#if defined(__APPLE__)
	if (dist >= 0.f && ps_r4_tess_dist_far > ps_r4_tess_dist_near)
	{
		const float ramp = clampr<float>((ps_r4_tess_dist_far - dist) /
			(ps_r4_tess_dist_far - ps_r4_tess_dist_near), 0.f, 1.f);
		factor = clampr<float>(ceil(factor * ramp), 1.f, 7.f);
	}
#endif

	// The tessellation factor is capped by a console variable: see ps_r4_tess_max.
	if ((float)ps_r4_tess_max < factor)
		factor = (float)ps_r4_tess_max;

	if (c_LOD)
		RCache.set_c(c_LOD, factor);
	FixedConstants::SetTriLOD(factor);
}