#include "stdafx.h"

#include "../xrRender/du_sphere.h"
#include "../xrRenderDX10/dx10BufferUtils.h"

void CRenderTarget::accum_point_geom_create()
{
	// vertices
	{
		u32		vCount		= DU_SPHERE_NUMVERTEX;
		u32		vSize		= 3*4;
		R_ASSERT( RHIUtils::CreateVertexBuffer( &g_accum_point_vb, du_sphere_vertices, vCount*vSize ) );
	}

	// Indices
	{
		u32		iCount		= DU_SPHERE_NUMFACES*3;
		R_ASSERT(RHIUtils::CreateIndexBuffer( &g_accum_point_ib, du_sphere_faces, iCount*2) );
	}
}

void CRenderTarget::accum_point_geom_destroy()
{
	delete g_accum_point_ib;
	delete g_accum_point_vb;
}
