#include "stdafx.h"
#include "R_IBackend.h"

#ifdef USE_DX11
struct Buffer_DX11
{

};
#else // DX9
struct Buffer_DX9
{
	IDirect3DVertexBuffer9* pVB;
	IDirect3DIndexBuffer9* pIB;
};
#endif // USE_DX11

CBackendBase::CBackendBase()
{
}

CBackendBase::~CBackendBase()
{
}

#ifdef USE_DX11
IVertexBuffer* CBackendBase::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	return nullptr;
}

#else
IVertexBuffer* CBackendBase::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::STATIC)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(HW.pDevice->CreateVertexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, 0, dwPool, &buffer->pVB, NULL));
	
	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pVB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pVB->Unlock();
	}

	IVertexBuffer* pVertexBuffer = xr_new<IVertexBuffer>();
	pVertexBuffer->m_InternalResource = buffer;
	return pVertexBuffer;
}

IIndexBuffer* CBackendBase::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::STATIC)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(HW.pDevice->CreateIndexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, dwPool, &buffer->pIB, NULL));
	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pIB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pIB->Unlock();
	}

	IIndexBuffer* pIndexBuffer = xr_new<IIndexBuffer>();
	pIndexBuffer->m_InternalResource = buffer;
	return pIndexBuffer;
}

void CBackendBase::set_VB(IVertexBuffer* _vb, u32 _vb_stride)
{
	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_vb->m_InternalResource.get());
	R_CHK(HW.pDevice->SetStreamSource(0, pBuffer->pVB, 0, _vb_stride));
}

void CBackendBase::set_IB(IIndexBuffer* _ib)
{
	Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_ib->m_InternalResource.get());
	R_CHK(HW.pDevice->SetIndices(pBuffer->pIB));
}

#endif
