#include "stdafx.h"
#include "du_sphere_part.h"

void CRenderTarget::accum_omnip_geom_create()
{
	g_accum_omnip_alloc = RImplementation.Geometry.AllocPositions(du_sphere_part_vertices, DU_SPHERE_PART_NUMVERTEX, du_sphere_part_faces, DU_SPHERE_PART_NUMFACES * 3);
	g_accum_omnipart.create(EGeomSource::Static, g_accum_omnip_alloc.Page);
}

void CRenderTarget::accum_omnip_geom_destroy()
{
	g_accum_omnipart.destroy();
	RImplementation.Geometry.Free(g_accum_omnip_alloc);
}
