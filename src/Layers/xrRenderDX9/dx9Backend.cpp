#include "stdafx.h"
#include "dx9Backend.h"

struct Buffer_DX9
{
	IDirect3DVertexBuffer9* pVB;
	IDirect3DIndexBuffer9* pIB;
};

CBackend_DX9 backend_dx9_impl;

CBackend_DX9::CBackend_DX9() :
	CBackendBase()
{
}

CBackend_DX9::~CBackend_DX9()
{
}

IVertexBuffer* CBackend_DX9::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::IMMUTABLE)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(RDevice->CreateVertexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, 0, dwPool, &buffer->pVB, NULL));

	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pVB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pVB->Unlock();
	}

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackend_DX9::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::IMMUTABLE)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(RDevice->CreateIndexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, dwPool, &buffer->pIB, NULL));
	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pIB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pIB->Unlock();
	}

	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

void CBackend_DX9::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		PGO(Msg("PGO:VB:%x,%d", _vb, _vb_stride));
#ifdef DEBUG
		//stat.vb++;
#endif
		vb = _vb;
		vb_stride = _vb_stride;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_vb->m_InternalResource.get());
		R_CHK(RDevice->SetStreamSource(0, pBuffer->pVB, 0, _vb_stride));
	}
}

void CBackend_DX9::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		PGO(Msg("PGO:IB:%x", _ib));
#ifdef DEBUG
		//stat.ib++;
#endif
		ib = _ib;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_ib->m_InternalResource.get());
		R_CHK(RDevice->SetIndices(pBuffer->pIB));
	}
}

void CBackend_DX9::set_Scissor(Irect* rect)
{
	if (rect)
	{
		CHK_DX(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, TRUE));
		RECT* clip = (RECT*)rect;
		CHK_DX(RDevice->SetScissorRect(clip));
	}
	else
	{
		CHK_DX(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, FALSE));
	}
}
