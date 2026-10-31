#include "stdafx.h"
#include "du_sphere.h"

void CRenderTarget::accum_point_geom_create()
{
	g_accum_point_alloc = RImplementation.Geometry.AllocPositions(du_sphere_vertices, DU_SPHERE_NUMVERTEX, du_sphere_faces, DU_SPHERE_NUMFACES * 3);
	g_accum_point.create(EGeomSource::Static, g_accum_point_alloc.Page);
}

void CRenderTarget::accum_point_geom_destroy()
{
	g_accum_point.destroy();
	RImplementation.Geometry.Free(g_accum_point_alloc);
}