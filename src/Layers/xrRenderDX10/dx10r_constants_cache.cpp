#include "stdafx.h"
#include "../xrRender/r_constants_cache.h"
#include "dx10FixedConstants.h"

dx10ConstantBuffer* R_constants::GetCBuffer(RHIShaderConstant* C, BufferType BType)
{
	if (BType == BT_PixelBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_pixel_cb_index_mask) >> RC_dest_pixel_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		dx10ConstantBuffer* buf = RCache.m_aPixelConstants[iBufferIndex]._get();
		if (!buf) return nullptr;
		MarkDirty(*buf);
		return buf;
	}
	else if (BType == BT_VertexBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_vertex_cb_index_mask) >> RC_dest_vertex_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		dx10ConstantBuffer* buf = RCache.m_aVertexConstants[iBufferIndex]._get();
		if (!buf) return nullptr;
		MarkDirty(*buf);
		return buf;
	}
	else if (BType == BT_GeometryBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_geometry_cb_index_mask) >> RC_dest_geometry_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		dx10ConstantBuffer* buf = RCache.m_aGeometryConstants[iBufferIndex]._get();
		if (!buf) return nullptr;
		MarkDirty(*buf);
		return buf;
	}
	else if (BType == BT_HullBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_hull_cb_index_mask) >> RC_dest_hull_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		dx10ConstantBuffer* buf = RCache.m_aHullConstants[iBufferIndex]._get();
		if (!buf) return nullptr;
		MarkDirty(*buf);
		return buf;
	}
	else if (BType == BT_DomainBuffer)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_domain_cb_index_mask) >> RC_dest_domain_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		dx10ConstantBuffer* buf = RCache.m_aDomainConstants[iBufferIndex]._get();
		if (!buf) return nullptr;
		MarkDirty(*buf);
		return buf;
	}
	else if (BType == BT_Compute)
	{
		//	Decode index
		int iBufferIndex = (C->destination & RC_dest_compute_cb_index_mask) >> RC_dest_compute_cb_index_shift;

		VERIFY(iBufferIndex < CBackend::MaxCBuffers);
		dx10ConstantBuffer* buf = RCache.m_aComputeConstants[iBufferIndex]._get();
		if (!buf) return nullptr;
		MarkDirty(*buf);
		return buf;
	}

	FATAL("Unreachable code");
	return nullptr;
}

void R_constants::MarkDirty(dx10ConstantBuffer& Buffer)
{
	if (Buffer.IsQueued() || Buffer.IsFixed())
		return;

	VERIFY(m_dirty_count < std::size(m_dirty));
	Buffer.SetQueued(true);
	m_dirty[m_dirty_count++] = &Buffer;
}

void R_constants::flush_cache()
{
	FixedConstants::Flush();

	for (u32 i = 0; i < m_dirty_count; ++i)
	{
		m_dirty[i]->SetQueued(false);
		m_dirty[i]->Flush();
	}

	m_dirty_count = 0;
}