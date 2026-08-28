#include "stdafx.h"
#include "../xrRender/r_constants_cache.h"

dx10ConstantBuffer& R_constants::GetCBuffer(RHIShaderConstant* C, BufferType BType)
{
	if (BType == BT_PixelBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_pixel_cb_index_mask) >> RC_dest_pixel_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		VERIFY(RCache.m_aPixelConstants[iBufferIndex]);
		MarkDirty(*RCache.m_aPixelConstants[iBufferIndex]);
		return *RCache.m_aPixelConstants[iBufferIndex];
	}
	else if (BType == BT_VertexBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_vertex_cb_index_mask) >> RC_dest_vertex_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		VERIFY(RCache.m_aVertexConstants[iBufferIndex]);
		MarkDirty(*RCache.m_aVertexConstants[iBufferIndex]);
		return *RCache.m_aVertexConstants[iBufferIndex];
	}
	else if (BType == BT_GeometryBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_geometry_cb_index_mask) >> RC_dest_geometry_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		VERIFY(RCache.m_aGeometryConstants[iBufferIndex]);
		MarkDirty(*RCache.m_aGeometryConstants[iBufferIndex]);
		return *RCache.m_aGeometryConstants[iBufferIndex];
	}
	else if (BType == BT_HullBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_hull_cb_index_mask) >> RC_dest_hull_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		VERIFY(RCache.m_aHullConstants[iBufferIndex]);
		MarkDirty(*RCache.m_aHullConstants[iBufferIndex]);
		return *RCache.m_aHullConstants[iBufferIndex];
	}
	else if (BType == BT_DomainBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_domain_cb_index_mask) >> RC_dest_domain_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		VERIFY(RCache.m_aDomainConstants[iBufferIndex]);
		MarkDirty(*RCache.m_aDomainConstants[iBufferIndex]);
		return *RCache.m_aDomainConstants[iBufferIndex];
	}
	else if (BType == BT_Compute)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_compute_cb_index_mask) >> RC_dest_compute_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		VERIFY(RCache.m_aComputeConstants[iBufferIndex]);
		MarkDirty(*RCache.m_aComputeConstants[iBufferIndex]);
		return *RCache.m_aComputeConstants[iBufferIndex];
	}

	FATAL("Unreachable code");
	//Just hack to avoid warning;
	dx10ConstantBuffer* ptr = 0;
	return *ptr;
}

void R_constants::MarkDirty(dx10ConstantBuffer& Buffer)
{
	if (Buffer.IsQueued())
		return;

	VERIFY(m_dirty_count < std::size(m_dirty));
	Buffer.SetQueued(true);
	m_dirty[m_dirty_count++] = &Buffer;
}

void R_constants::flush_cache()
{
	for (u32 i = 0; i < m_dirty_count; ++i)
	{
		m_dirty[i]->SetQueued(false);
		m_dirty[i]->Flush();
	}

	m_dirty_count = 0;
}