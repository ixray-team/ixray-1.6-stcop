#include "stdafx.h"
#include "du_cone.h"

void CRenderTarget::accum_spot_geom_create()
{
	g_accum_spot_alloc = RImplementation.Geometry.AllocPositions(du_cone_vertices, DU_CONE_NUMVERTEX, du_cone_faces, DU_CONE_NUMFACES * 3);
	g_accum_spot.create(EGeomSource::Static, g_accum_spot_alloc.Page);
}

void CRenderTarget::accum_spot_geom_destroy()
{
	g_accum_spot.destroy();
	RImplementation.Geometry.Free(g_accum_spot_alloc);
}

struct Slice
{
	Fvector	m_Vert[4];
};

void CRenderTarget::accum_volumetric_geom_create()
{
	Slice pSlice[VOLUMETRIC_SLICES];
	u16 Indices[VOLUMETRIC_SLICES * 6];
	float t = 0;
	float dt = 1.0f / (VOLUMETRIC_SLICES - 1);
	for (int i = 0; i < VOLUMETRIC_SLICES; ++i)
	{
		pSlice[i].m_Vert[0] = Fvector().set(0, 0, t);
		pSlice[i].m_Vert[1] = Fvector().set(0, 1, t);
		pSlice[i].m_Vert[2] = Fvector().set(1, 0, t);
		pSlice[i].m_Vert[3] = Fvector().set(1, 1, t);
		t += dt;

		const u16 basevert = u16(i * 4);
		u16* pInd = Indices + i * 6;
		pInd[0] = basevert;
		pInd[1] = basevert + 1;
		pInd[2] = basevert + 2;
		pInd[3] = basevert + 2;
		pInd[4] = basevert + 1;
		pInd[5] = basevert + 3;
	}
	g_accum_volumetric_alloc = RImplementation.Geometry.AllocPositions(pSlice[0].m_Vert, VOLUMETRIC_SLICES * 4, Indices, VOLUMETRIC_SLICES * 6);
	g_accum_volumetric.create(EGeomSource::Static, g_accum_volumetric_alloc.Page);
}

void CRenderTarget::accum_volumetric_geom_destroy()
{
	g_accum_volumetric.destroy();
	RImplementation.Geometry.Free(g_accum_volumetric_alloc);
}
