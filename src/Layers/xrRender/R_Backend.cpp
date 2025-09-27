#include "stdafx.h"

#include "R_Backend.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10BufferUtils.h"
#endif // USE_DX11

CBackend			RCache;

// Create Quad-IB
#ifdef USE_DX11

// Igor: is used to test bug with rain, particles corruption
void CBackend::RestoreQuadIBData()
{
	// Igor: never seen this corruption for DX10
	;
}

void CBackend::CreateQuadIB()
{
	static const u32 dwTriCount = 4 * 1024;
	static const u32 dwIdxCount = dwTriCount * 2 * 3;
	u16	IndexBuffer[dwIdxCount];
	u16* Indices = IndexBuffer;

	RHIBufferDesc desc;
	desc.Size = dwIdxCount * 2;
	desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	desc.Type = ERHI_BUFFER_TYPE::INDEX;

	RHIBufferSubresource subData;
	subData.pSysMem = IndexBuffer;

	int	Cnt = 0;
	int	ICnt = 0;
	for (int i = 0; i < dwTriCount; i++)
	{
		Indices[ICnt++] = u16(Cnt + 0);
		Indices[ICnt++] = u16(Cnt + 1);
		Indices[ICnt++] = u16(Cnt + 2);

		Indices[ICnt++] = u16(Cnt + 3);
		Indices[ICnt++] = u16(Cnt + 2);
		Indices[ICnt++] = u16(Cnt + 1);

		Cnt += 4;
	}

	QuadIB = GRHI->CreateBuffer(desc, &subData);
}

#else //USE_DX11

// Igor: is used to test bug with rain, particles corruption
void CBackend::RestoreQuadIBData()
{
	const u32 dwTriCount	= 4*1024;
	u16		IndexBuffer[dwTriCount * 2 * 3];
	u16* Indices = IndexBuffer;
	int	Cnt = 0;
	int	ICnt = 0;
	for (int i = 0; i < dwTriCount; ++i)
	{
		Indices[ICnt++] = u16(Cnt + 0);
		Indices[ICnt++] = u16(Cnt + 1);
		Indices[ICnt++] = u16(Cnt + 2);

		Indices[ICnt++] = u16(Cnt + 3);
		Indices[ICnt++] = u16(Cnt + 2);
		Indices[ICnt++] = u16(Cnt + 1);

		Cnt += 4;
	}

	// Update QuadIB via Map/Unmap
	RHIMappedSubresource mapped = {};
	if (QuadIB->Map(ERHI_BUFFER_MAP::WRITE, 0, &mapped))
	{
		memcpy(mapped.pData, IndexBuffer, dwTriCount * 2 * 3 * sizeof(u16));
		QuadIB->Unmap();
	}
}


void CBackend::CreateQuadIB		()
{
	const u32 dwTriCount	= 4*1024;
	const u32 dwIdxCount	= dwTriCount*2*3;
	u16 IndexBufferStatic[dwIdxCount];
	u16* Indices = IndexBufferStatic;

	int Cnt = 0;
	int ICnt = 0;
	for (int i = 0; i < dwTriCount; ++i)
	{
		Indices[ICnt++] = u16(Cnt + 0);
		Indices[ICnt++] = u16(Cnt + 1);
		Indices[ICnt++] = u16(Cnt + 2);

		Indices[ICnt++] = u16(Cnt + 3);
		Indices[ICnt++] = u16(Cnt + 2);
		Indices[ICnt++] = u16(Cnt + 1);

		Cnt += 4;
	}

	// Create immutable index buffer with initial data
	VERIFY(RHIUtils::CreateIndexBuffer(&QuadIB, IndexBufferStatic, dwIdxCount * 2));
}

#endif

// Device dependance
void CBackend::OnDeviceCreate	()
{
	CreateQuadIB		();

	// streams
	Vertex.Create		();
	Index.Create		();

	// invalidate caching
	Invalidate			();
}

void CBackend::OnDeviceDestroy()
{
	// streams
	Index.Destroy		();
	Vertex.Destroy		();

	// Quad
	_RELEASE							(QuadIB);
}

void CBackend::set_Vertices(IRHIBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		vb = _vb;
		vb_stride = _vb_stride;

		if (vb)
		{
			vb->SetVertexBuffer(0, _vb_stride, 0);
		}
		else
		{
			GRHI->ClearVertexBuffer(_vb_stride);
		}
	}
}

void CBackend::set_Indices(IRHIBuffer* _ib)
{
	if (ib != _ib)
	{
		ib = _ib;

		if (ib)
		{
			ib->SetIndexBuffer(false, 0);
		}
		else
		{
			GRHI->ClearIndexBuffer();
		}
	}
}