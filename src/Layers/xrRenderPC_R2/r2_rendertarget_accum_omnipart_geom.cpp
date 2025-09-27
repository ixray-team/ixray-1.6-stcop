#include "stdafx.h"
#include "../xrRender/du_sphere_part.h"

void CRenderTarget::accum_omnip_geom_create()
{
	// vertices
	{
		u32 vCount = DU_SPHERE_PART_NUMVERTEX;
		u32 vSize = 3 * 4;
		VERIFY(RHIUtils::CreateVertexBuffer(&g_accum_omnip_vb, du_sphere_part_vertices, vCount * vSize));
	}

	// Indices
	{
		u32 iCount = DU_SPHERE_PART_NUMFACES * 3;
		VERIFY(RHIUtils::CreateIndexBuffer(&g_accum_omnip_ib, du_sphere_part_faces, iCount * 2));
	}
}

void CRenderTarget::accum_omnip_geom_destroy()
{
	_RELEASE(g_accum_omnip_ib);
	_RELEASE(g_accum_omnip_vb);
}
