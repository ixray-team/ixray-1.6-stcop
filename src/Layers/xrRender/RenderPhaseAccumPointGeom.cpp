#include "stdafx.h"
#include "du_sphere.h"

void CRenderTarget::accum_point_geom_create()
{
	u32 vCount = DU_SPHERE_NUMVERTEX;
	u32 vSize = 3 * 4;
	R_ASSERT(RHIUtils::CreateVertexBuffer(&g_accum_point_vb, du_sphere_vertices, vCount * vSize));

	// Indices
	u32	iCount = DU_SPHERE_NUMFACES * 3;
	R_ASSERT(RHIUtils::CreateIndexBuffer(&g_accum_point_ib, du_sphere_faces, iCount * 2));
}

void CRenderTarget::accum_point_geom_destroy()
{
	_RELEASE(g_accum_point_ib);
	_RELEASE(g_accum_point_vb);
}