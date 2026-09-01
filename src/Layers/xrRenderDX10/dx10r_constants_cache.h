#ifndef dx10r_constants_cacheH
#define dx10r_constants_cacheH
#pragma once

class	ECORE_API  R_constants
{
	enum	BufferType
	{
		BT_PixelBuffer,
		BT_VertexBuffer,
		BT_GeometryBuffer,
		BT_HullBuffer,
		BT_DomainBuffer,
		BT_Compute
	};
public:
	void					flush_cache();

public:
	template<typename T>
	ICF void				set(RHIShaderConstant* C, const T& A) {
		if (C->fixed_id > 0) return;
		if (C->destination & RC_dest_pixel) { set(C, C->ps, A, BT_PixelBuffer); }	// a_pixel.b_dirty=true;		}
		if (C->destination & RC_dest_vertex) { set(C, C->vs, A, BT_VertexBuffer); }	//  a_vertex.b_dirty=true;		}
		if (C->destination & RC_dest_geometry) { set(C, C->gs, A, BT_GeometryBuffer); }	//  a_vertex.b_dirty=true;		}
		if (C->destination & RC_dest_hull) { set(C, C->hs, A, BT_HullBuffer); }	//  a_vertex.b_dirty=true;		}
		if (C->destination & RC_dest_domain) { set(C, C->ds, A, BT_DomainBuffer); }	//  a_vertex.b_dirty=true;		}
		if (C->destination & RC_dest_compute) { set(C, C->cs, A, BT_Compute); }	//  a_vertex.b_dirty=true;		}
	}

	template<typename T>
	ICF void				seta(RHIShaderConstant* C, u32 e, const T& A) {
		if (C->fixed_id > 0) return;
		if (C->destination & RC_dest_pixel) { seta(C, C->ps, e, A, BT_PixelBuffer); }	//  a_pixel.b_dirty=true;	}
		if (C->destination & RC_dest_vertex) { seta(C, C->vs, e, A, BT_VertexBuffer); }	//  a_vertex.b_dirty=true;	}
		if (C->destination & RC_dest_geometry) { seta(C, C->gs, e, A, BT_GeometryBuffer); }	//  a_vertex.b_dirty=true;	}
		if (C->destination & RC_dest_hull) { seta(C, C->hs, e, A, BT_HullBuffer); }	//  a_vertex.b_dirty=true;		}
		if (C->destination & RC_dest_domain) { seta(C, C->ds, e, A, BT_DomainBuffer); }	//  a_vertex.b_dirty=true;		}
		if (C->destination & RC_dest_compute) { seta(C, C->cs, e, A, BT_Compute); }	//  a_vertex.b_dirty=true;		}
	}

	ICF void				set(RHIShaderConstant* C, float x, float y, float z, float w) {
		Fvector4 data;		data.set(x, y, z, w);
		set(C, data);
	}

	ICF void				seta(RHIShaderConstant* C, u32 e, float x, float y, float z, float w) {
		Fvector4 data;		data.set(x, y, z, w);
		seta(C, e, data);
	}

	ICF void				flush()
	{
		flush_cache();
	}

	void					MarkDirty(dx10ConstantBuffer& Buffer);

	ICF void				access_direct(RHIShaderConstant* C, u32 DataSize, void** ppVData, void** ppGData, void** ppPData)
	{
		if (ppPData)
		{
			if (C->destination & RC_dest_pixel) { access_direct(C, C->ps, ppPData, DataSize, BT_PixelBuffer); }
			else *ppPData = 0;
		}

		if (ppVData)
		{
			if (C->destination & RC_dest_vertex) { access_direct(C, C->vs, ppVData, DataSize, BT_VertexBuffer); }
			else *ppVData = 0;
		}

		if (ppGData)
		{
			if (C->destination & RC_dest_geometry) { access_direct(C, C->gs, ppGData, DataSize, BT_GeometryBuffer); }
			else *ppGData = 0;
		}
	}

private:


	void					set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, const Fmatrix& A, BufferType BType)
	{
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		if (Buffer) Buffer->set(C, L, A);
	}

	void					set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, const Fvector4& A, BufferType BType)
	{
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		if (Buffer) Buffer->set(C, L, A);
	}

	void					set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, float A, BufferType BType)
	{
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		if (Buffer) Buffer->set(C, L, A);
	}

	void					set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, int A, BufferType BType)
	{
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		if (Buffer) Buffer->set(C, L, A);
	}

	void					seta(RHIShaderConstant* C, RHIShaderConstant::Loader& L, u32 e, const Fmatrix& A, BufferType BType)
	{
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		if (Buffer) Buffer->seta(C, L, e, A);
	}

	void					seta(RHIShaderConstant* C, RHIShaderConstant::Loader& L, u32 e, const Fvector4& A, BufferType BType)
	{
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		if (Buffer) Buffer->seta(C, L, e, A);
	}

	void					access_direct(RHIShaderConstant* C, RHIShaderConstant::Loader& L, void** ppData, u32 DataSize, BufferType BType)
	{
		if (C->fixed_id > 0) { *ppData = nullptr; return; }
		dx10ConstantBuffer* Buffer = GetCBuffer(C, BType);
		*ppData = Buffer ? Buffer->AccessDirect(L, DataSize) : nullptr;	// fixed buffers included: the caller writes through this
	}

	dx10ConstantBuffer*		GetCBuffer(RHIShaderConstant* C, BufferType BType);

	// Every write reaches a buffer through GetCBuffer, so registering there is exact and
	// avoids rescanning all MaxCBuffers slots of all six stages on every draw call.
	dx10ConstantBuffer*		m_dirty[RHI_MAX_CONSTANT_BUFFERS * RHI_SHADERS_TYPE_SIZE];
	u32						m_dirty_count = 0;
};
#endif	//	dx10r_constants_cacheH