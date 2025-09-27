#include "stdafx.h"

#include "../xrRender/du_cone.h"
#include "../xrRenderDX10/dx10BufferUtils.h"

/*
Fvector du_cone_vertices[DU_CONE_NUMVERTEX]=
{
	0.0000f,    0.0000f, 0.0000f,
	0.5000f,   0.0000f,	 1.0000f,
	0.4619f,   0.1913f,	 1.0000f,
	0.3536f,   0.3536f,	 1.0000f,
	0.1913f,   0.4619f,	 1.0000f,
	-0.0000f,  0.5000f,	 1.0000f,
	-0.1913f,  0.4619f,	 1.0000f,
	-0.3536f,  0.3536f,	 1.0000f,
	-0.4619f,  0.1913f,	 1.0000f,
	-0.5000f,  -0.0000f, 1.0000f,
	-0.4619f,  -0.1913f, 1.0000f,
	-0.3536f,  -0.3536f, 1.0000f,
	-0.1913f,  -0.4619f, 1.0000f,
	0.0000f,   -0.5000f, 1.0000f,
	0.1913f,   -0.4619f, 1.0000f,
	0.3536f,   -0.3536f, 1.0000f,
	0.4619f,   -0.1913f, 1.0000f,
	0.0000f,   0.0000f,	 1.0000f+EPS_L
};
u16 du_cone_faces[DU_CONE_NUMFACES*3]=
{
	0,	2,	1,
	0,	3,	2,
	0,	4,	3,
	0,  5,	4,
	0,	6,	5,
	0,  7,  6,
	0,  8,  7,
	0,  9,  8,
	0, 10,  9,
	0, 11, 10,
	0, 12, 11,
	0, 13, 12,
	0, 14, 13,
	0, 15, 14,
	0, 16, 15,
	0,  1, 16,
	17, 1, 2,
	17, 2, 3,
	17, 3, 4,
	17, 4, 5,
	17, 5, 6,
	17, 6, 7,
	17, 7, 8,
	17, 8, 9,
	17, 9, 10,
	17,10, 11,
	17,11, 12,
	17,12, 13,
	17,13, 14,
	17,14, 15,
	17,15, 16,
	17,16, 1
};
*/

void CRenderTarget::accum_spot_geom_create()
{
	// vertices
	u32 vCount = DU_CONE_NUMVERTEX;
	u32 vSize = 3 * 4;
	VERIFY(RHIUtils::CreateVertexBuffer(&g_accum_spot_vb, du_cone_vertices, vCount * vSize));

	// Indices
	u32 iCount = DU_CONE_NUMFACES * 3;
	VERIFY(RHIUtils::CreateIndexBuffer(&g_accum_spot_ib, du_cone_faces, iCount * 2));
}

void CRenderTarget::accum_spot_geom_destroy()
{
	_RELEASE	(g_accum_spot_ib);
	_RELEASE	(g_accum_spot_vb);
}

struct Slice
{
	Fvector	m_Vert[4];
};

void CRenderTarget::accum_volumetric_geom_create()
{
	// vertices
	{
		//	VOLUMETRIC_SLICES quads
		static const u32		vCount = VOLUMETRIC_SLICES * 4;
		u32 vSize = 3 * 4;

		Slice	pSlice[VOLUMETRIC_SLICES];

		float t = 0;
		float dt = 1.0f / (VOLUMETRIC_SLICES - 1);
		for (int i = 0; i < VOLUMETRIC_SLICES; ++i)
		{
			pSlice[i].m_Vert[0] = Fvector().set(0, 0, t);
			pSlice[i].m_Vert[1] = Fvector().set(0, 1, t);
			pSlice[i].m_Vert[2] = Fvector().set(1, 0, t);
			pSlice[i].m_Vert[3] = Fvector().set(1, 1, t);
			t += dt;
		}

		VERIFY(RHIUtils::CreateVertexBuffer(&g_accum_volumetric_vb, &pSlice, vCount * vSize));
	}

	// Indices
	{
		const u32 iCount = VOLUMETRIC_SLICES * 6;
		BYTE Datap[iCount * 2];

		u16* pInd = (u16*)Datap;
		for (u16 i = 0; i < VOLUMETRIC_SLICES; ++i, pInd += 6)
		{
			u16 basevert = i * 4;
			pInd[0] = basevert;
			pInd[1] = basevert + 1;
			pInd[2] = basevert + 2;
			pInd[3] = basevert + 2;
			pInd[4] = basevert + 1;
			pInd[5] = basevert + 3;
		}

		VERIFY(RHIUtils::CreateIndexBuffer(&g_accum_volumetric_ib, &Datap, iCount * 2));
	}
}

void CRenderTarget::accum_volumetric_geom_destroy()
{
	_RELEASE(g_accum_volumetric_ib);
	_RELEASE(g_accum_volumetric_vb);
}