#include "stdafx.h"
#pragma hdrstop
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

void CBackend::CreateQuadIB		()
{
	static const u32 dwTriCount = 4 * 1024;
	static const u32 dwIdxCount = dwTriCount * 2 * 3;
	u16		IndexBuffer[dwIdxCount];
	u16* Indices = IndexBuffer;
	{
		int		Cnt = 0;
		int		ICnt = 0;
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
	}

	R_ASSERT(RHIUtils::CreateIndexBuffer(&QuadIB, IndexBuffer, dwIdxCount * 2));
}

#else //USE_DX11

// Igor: is used to test bug with rain, particles corruption
void CBackend::RestoreQuadIBData()
{
	const u32 dwTriCount	= 4*1024;
	u16		*Indices		= 0;
	R_CHK(QuadIB->Lock(0,0,(void**)&Indices,0));
	{
		int		Cnt = 0;
		int		ICnt= 0;
		for (int i=0; i<dwTriCount; i++)
		{
			Indices[ICnt++]=u16(Cnt+0);
			Indices[ICnt++]=u16(Cnt+1);
			Indices[ICnt++]=u16(Cnt+2);

			Indices[ICnt++]=u16(Cnt+3);
			Indices[ICnt++]=u16(Cnt+2);
			Indices[ICnt++]=u16(Cnt+1);

			Cnt+=4;
		}
	}
	R_CHK(QuadIB->Unlock());
}


void CBackend::CreateQuadIB		()
{
	const u32 dwTriCount	= 4*1024;
	const u32 dwIdxCount	= dwTriCount*2*3;
	u16		*Indices		= 0;
	u32		dwUsage			= D3DUSAGE_WRITEONLY;
	if (Caps.geometry.bSoftware)	dwUsage|=D3DUSAGE_SOFTWAREPROCESSING;
	R_CHK(RDevice->CreateIndexBuffer	(dwIdxCount*2,dwUsage,D3DFMT_INDEX16,D3DPOOL_DEFAULT,&QuadIB,nullptr));
//	Msg("CBackend::CreateQuadIB(). Created buffer size = %d ", dwIdxCount*2 );
	R_CHK(QuadIB->Lock(0,0,(void**)&Indices,0));
	{
		int		Cnt = 0;
		int		ICnt= 0;
		for (int i=0; i<dwTriCount; i++)
		{
			Indices[ICnt++]=u16(Cnt+0);
			Indices[ICnt++]=u16(Cnt+1);
			Indices[ICnt++]=u16(Cnt+2);

			Indices[ICnt++]=u16(Cnt+3);
			Indices[ICnt++]=u16(Cnt+2);
			Indices[ICnt++]=u16(Cnt+1);

			Cnt+=4;
		}
	}
	R_CHK(QuadIB->Unlock());
}

#endif

// Device dependance
void CBackend::OnDeviceCreate	()
{
#ifdef USE_DX11
	//CreateConstantBuffers();
#endif //USE_DX11

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

#ifdef USE_DX11
	//DestroyConstantBuffers();
#endif //USE_DX11
}

#ifdef USE_DX11
/*
void CBackend::CreateConstantBuffers()
{
	const int iVectorElements = 4;
	const int iVectorNumber = 256;
	dx10BufferUtils::CreateConstantBuffer(&m_pPixelConstants, iVectorNumber*iVectorElements*sizeof(float));
	dx10BufferUtils::CreateConstantBuffer(&m_pVertexConstants, iVectorNumber*iVectorElements*sizeof(float));
}

void CBackend::DestroyConstantBuffers()
{
	_RELEASE(m_pVertexConstants);
	_RELEASE(m_pPixelConstants);
}
*/
#endif