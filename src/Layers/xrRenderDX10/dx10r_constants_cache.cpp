#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/r_constants_cache.h"
#include "dx11Backend.h"

dx10ConstantBuffer& R_constants_DX11::GetCBuffer(R_constant* C, BufferType BType)
{
	if (BType==BT_PixelBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination&RC_dest_pixel_cb_index_mask)>>RC_dest_pixel_cb_index_shift;

		VERIFY(iBufferIndex< CBackend_DX11::MaxCBuffers);
		VERIFY(backend_dx11_impl.m_aPixelConstants[iBufferIndex]);
		return *backend_dx11_impl.m_aPixelConstants[iBufferIndex];
	}
	else if (BType==BT_VertexBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination&RC_dest_vertex_cb_index_mask)>>RC_dest_vertex_cb_index_shift;

		VERIFY(iBufferIndex< CBackend_DX11::MaxCBuffers);
		VERIFY(backend_dx11_impl.m_aVertexConstants[iBufferIndex]);
		return *backend_dx11_impl.m_aVertexConstants[iBufferIndex];
	}
	else if (BType==BT_GeometryBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination&RC_dest_geometry_cb_index_mask)>>RC_dest_geometry_cb_index_shift;

		VERIFY(iBufferIndex< CBackend_DX11::MaxCBuffers);
		VERIFY(backend_dx11_impl.m_aGeometryConstants[iBufferIndex]);
		return *backend_dx11_impl.m_aGeometryConstants[iBufferIndex];
	}
	else if (BType==BT_HullBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination&RC_dest_hull_cb_index_mask)>>RC_dest_hull_cb_index_shift;

		VERIFY(iBufferIndex< CBackend_DX11::MaxCBuffers);
		VERIFY(backend_dx11_impl.m_aHullConstants[iBufferIndex]);
		return *backend_dx11_impl.m_aHullConstants[iBufferIndex];
	}
	else if (BType==BT_DomainBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination&RC_dest_domain_cb_index_mask)>>RC_dest_domain_cb_index_shift;

		VERIFY(iBufferIndex< CBackend_DX11::MaxCBuffers);
		VERIFY(backend_dx11_impl.m_aDomainConstants[iBufferIndex]);
		return *backend_dx11_impl.m_aDomainConstants[iBufferIndex];
	}
	else if (BType==BT_Compute)
	{
		//	Decode index
		int iBufferIndex = (C->destination&RC_dest_compute_cb_index_mask)>>RC_dest_compute_cb_index_shift;

		VERIFY(iBufferIndex<CBackend_DX11::MaxCBuffers);
		VERIFY(backend_dx11_impl.m_aComputeConstants[iBufferIndex]);
		return *backend_dx11_impl.m_aComputeConstants[iBufferIndex];
	}


	FATAL("Unreachable code");
	//Just hack to avoid warning;
	dx10ConstantBuffer* ptr = 0;
	return *ptr;
}

void R_constants_DX11::flush_cache()
{
	for (int i=0; i < CBackend_DX11::MaxCBuffers; ++i)
	{
		if (backend_dx11_impl.m_aVertexConstants[i])
			backend_dx11_impl.m_aVertexConstants[i]->Flush();

		if (backend_dx11_impl.m_aPixelConstants[i])
			backend_dx11_impl.m_aPixelConstants[i]->Flush();

		if (backend_dx11_impl.m_aGeometryConstants[i])
			backend_dx11_impl.m_aGeometryConstants[i]->Flush();

		if (backend_dx11_impl.m_aHullConstants[i])
			backend_dx11_impl.m_aHullConstants[i]->Flush();

		if (backend_dx11_impl.m_aDomainConstants[i])
			backend_dx11_impl.m_aDomainConstants[i]->Flush();

        if (backend_dx11_impl.m_aComputeConstants[i])
			backend_dx11_impl.m_aComputeConstants[i]->Flush();
	}
}

/*
void R_constants::flush_cache()
{
	if (a_pixel.b_dirty)
	{
		// fp
		R_constant_array::t_f&	F	= a_pixel.c_f;
		{
			//if (F.r_lo() <= 32) //. hack
			{		
				void	*pBuffer;
				const int iVectorElements = 4;
				const int iVectorNumber = 256;
				RCache.m_pPixelConstants->Map(D3Dxx_MAP_WRITE_DISCARD, 0, &pBuffer);
				CopyMemory(pBuffer, F.access(0), iVectorNumber*iVectorElements*sizeof(float));
				RCache.m_pPixelConstants->Unmap();
			}
		}
		a_pixel.b_dirty		= false;
	}
	if (a_vertex.b_dirty)
	{
		// fp
		R_constant_array::t_f&	F	= a_vertex.c_f;
		{
			u32		count		= F.r_hi()-F.r_lo();
			if (count)			{
#ifdef DEBUG
				if (F.r_hi() > HW.Caps.geometry.dwRegisters)
				{
					Debug.fatal(DEBUG_INFO,"Internal error setting VS-constants: overflow\nregs[%d],hi[%d]",
						HW.Caps.geometry.dwRegisters,F.r_hi()
						);
				}
				PGO		(Msg("PGO:V_CONST:%d",count));
#endif
				{		
					void	*pBuffer;
					const int iVectorElements = 4;
					const int iVectorNumber = 256;
					RCache.m_pVertexConstants->Map(D3Dxx_MAP_WRITE_DISCARD, 0, &pBuffer);
					CopyMemory(pBuffer, F.access(0), iVectorNumber*iVectorElements*sizeof(float));
					RCache.m_pVertexConstants->Unmap();
				}
				F.flush	();
			}
		}
		a_vertex.b_dirty	= false;
	}
}
*/
