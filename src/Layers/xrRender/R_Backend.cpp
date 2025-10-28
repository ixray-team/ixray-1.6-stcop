#include "stdafx.h"
#include "R_Backend.h"

CBackend RCache;

// Create Quad-IB
void CBackend::CreateQuadIB()
{
	const u32 dwTriCount = 4 * (4096 * 8);
	const u32 dwIdxCount = dwTriCount * 2 * 3;
	static u16 IndexBufferStatic[dwIdxCount];
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
	R_ASSERT(RHIUtils::CreateIndexBuffer(&QuadIB, IndexBufferStatic, dwIdxCount * 2));
}

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