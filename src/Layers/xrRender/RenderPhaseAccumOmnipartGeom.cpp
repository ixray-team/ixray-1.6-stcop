#include "stdafx.h"
#include "du_sphere_part.h"

void CRenderTarget::accum_omnip_geom_create()
{
	u32 vCount = DU_SPHERE_PART_NUMVERTEX;
	u32 vSize = 3 * 4;
	R_ASSERT(RHIUtils::CreateVertexBuffer(&g_accum_omnip_vb, du_sphere_part_vertices, vCount * vSize));

	u32 iCount = DU_SPHERE_PART_NUMFACES * 3;
	R_ASSERT(RHIUtils::CreateIndexBuffer(&g_accum_omnip_ib, du_sphere_part_faces, iCount * 2));
}

void CRenderTarget::accum_omnip_geom_destroy()
{
	_RELEASE(g_accum_omnip_ib);
	_RELEASE(g_accum_omnip_vb);
}
