#include "stdafx.h"

#include "../xrRender/du_cone.h"
#include "../xrRenderDX10/dx10BufferUtils.h"

void CRenderTarget::accum_spot_geom_create	()
{
//	u32	dwUsage				= D3DUSAGE_WRITEONLY;

	// vertices
	{
		u32		vCount		= DU_CONE_NUMVERTEX;
		u32		vSize		= 3*4;
		R_ASSERT(RHIUtils::CreateVertexBuffer( &g_accum_spot_vb, du_cone_vertices,vCount*vSize));
	}

	// Indices
	{
		u32		iCount		= DU_CONE_NUMFACES*3;
		R_ASSERT( RHIUtils::CreateIndexBuffer( &g_accum_spot_ib, du_cone_faces, iCount*2 ) );
	}
}

void CRenderTarget::accum_spot_geom_destroy()
{
	delete g_accum_spot_ib;
	delete g_accum_spot_vb;
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
		static const u32		vCount		= VOLUMETRIC_SLICES*4;
		u32		vSize		= 3*4;
		Slice	pSlice[VOLUMETRIC_SLICES];

		float t=0;
		float dt = 1.0f/(VOLUMETRIC_SLICES-1);
		for ( int i=0; i<VOLUMETRIC_SLICES; ++i)
		{
			pSlice[i].m_Vert[0] = Fvector().set(0,0,t);
			pSlice[i].m_Vert[1] = Fvector().set(0,1,t);
			pSlice[i].m_Vert[2] = Fvector().set(1,0,t);
			pSlice[i].m_Vert[3] = Fvector().set(1,1,t);
			t += dt;
		}

		R_ASSERT( RHIUtils::CreateVertexBuffer( &g_accum_volumetric_vb, &pSlice, vCount*vSize) );
	}

	// Indices
	{
		const u32		iCount		= VOLUMETRIC_SLICES*6;
		BYTE	Datap[iCount*2];

		u16 *pInd = (u16*) Datap;
		for ( u16 i=0; i<VOLUMETRIC_SLICES; ++i, pInd+=6)
		{
			u16 basevert = i*4;
			pInd[0] = basevert;
			pInd[1] = basevert+1;
			pInd[2] = basevert+2;
			pInd[3] = basevert+2;
			pInd[4] = basevert+1;
			pInd[5] = basevert+3;
		}

		R_ASSERT(RHIUtils::CreateIndexBuffer( &g_accum_volumetric_ib, &Datap, iCount*2 ) );
	}
}

void CRenderTarget::accum_volumetric_geom_destroy()
{
	delete g_accum_volumetric_ib;
	delete g_accum_volumetric_vb;
}