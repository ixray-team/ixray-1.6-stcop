#include "stdafx.h"
#include "R_IBackend.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10BufferUtils.h"

struct Buffer_DX11
{
	ID3D11Buffer* pBuffer;
};
#else // DX9
struct Buffer_DX9
{
	IDirect3DVertexBuffer9* pVB;
	IDirect3DIndexBuffer9* pIB;
};
#endif // USE_DX11

CBackendBase::CBackendBase() :
	vb(nullptr), ib(nullptr), vb_stride(0)
{
}

CBackendBase::~CBackendBase()
{
}

#ifdef USE_DX11
IVertexBuffer* CBackendBase::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	R_CHK(dx10BufferUtils::CreateVertexBuffer(&buffer->pBuffer, data, length, usage == ResourceUsage::IMMUTABLE));

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackendBase::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	R_CHK(dx10BufferUtils::CreateIndexBuffer(&buffer->pBuffer, data, length, usage == ResourceUsage::IMMUTABLE));

	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

void CBackendBase::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		vb = _vb;
		vb_stride = _vb_stride;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_vb->m_InternalResource.get());

		u32	iOffset = 0;
		HW.pContext->IASetVertexBuffers(0, 1, &pBuffer->pBuffer, &_vb_stride, &iOffset);
	}
}

void CBackendBase::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		ib = _ib;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_ib->m_InternalResource.get());
		HW.pContext->IASetIndexBuffer(pBuffer->pBuffer, DXGI_FORMAT_R16_UINT, 0);
	}
}
#else
IVertexBuffer* CBackendBase::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
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

	R_CHK(HW.pDevice->CreateVertexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, 0, dwPool, &buffer->pVB, NULL));
	
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

IIndexBuffer* CBackendBase::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
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

	R_CHK(HW.pDevice->CreateIndexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, dwPool, &buffer->pIB, NULL));
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

void CBackendBase::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
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
		R_CHK(HW.pDevice->SetStreamSource(0, pBuffer->pVB, 0, _vb_stride));
	}
}

void CBackendBase::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		PGO(Msg("PGO:IB:%x", _ib));
#ifdef DEBUG
		//stat.ib++;
#endif
		ib = _ib;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_ib->m_InternalResource.get());
		R_CHK(HW.pDevice->SetIndices(pBuffer->pIB));
	}
}

#endif
