#include "stdafx.h"
#include "dx11Backend.h"
#include "dx10BufferUtils.h"

struct Buffer_DX11
{
	ID3D11Buffer* pBuffer;
};

CBackend_DX11 backend_dx11_impl;

CBackend_DX11::CBackend_DX11()
{
}

CBackend_DX11::~CBackend_DX11()
{
}

IVertexBuffer* CBackend_DX11::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	R_CHK(dx10BufferUtils::CreateVertexBuffer(&buffer->pBuffer, data, length, usage == ResourceUsage::IMMUTABLE));

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackend_DX11::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	R_CHK(dx10BufferUtils::CreateIndexBuffer(&buffer->pBuffer, data, length, usage == ResourceUsage::IMMUTABLE));

	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

void CBackend_DX11::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
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

void CBackend_DX11::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		ib = _ib;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_ib->m_InternalResource.get());
		HW.pContext->IASetIndexBuffer(pBuffer->pBuffer, DXGI_FORMAT_R16_UINT, 0);
	}
}