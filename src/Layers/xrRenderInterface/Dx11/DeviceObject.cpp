#include "stdafx.h"
#include <d3d11.h>
#include "../../../xrEngine/ICore_GPU.h"
#include <renderdoc/api/app/renderdoc_app.h>
#include "Dx11API.h"

UINT GetD3D11BindFlags(eBufferType bufferType)
{
	switch (bufferType)
	{
	case eVertexBuffer:
		return D3D11_BIND_VERTEX_BUFFER;
	case eIndexBuffer:
		return D3D11_BIND_INDEX_BUFFER;
	case eConstantBuffer:
		return D3D11_BIND_CONSTANT_BUFFER;
	}
	
	return 0;
}

D3D11_MAP GetD3D11Map(eLockType lockType)
{
	switch (lockType)
	{
	case eLOCK_DISCARD:
		return D3D11_MAP_WRITE_DISCARD;
	case eLOCK_NOOVERWRITE:
		return D3D11_MAP_WRITE_NO_OVERWRITE;
	case eLOCK_READONLY:
		return D3D11_MAP_READ;

	// #TODO: To Implement
	case eLOCK_NO_DIRTY_UPDATE:
	case eLOCK_NOSYSLOCK:
	default:
		break;
	}

	R_ASSERT(0);
	return (D3D11_MAP)0;
}

class CD3D11Buffer : public IRHIBuffer
{
public:
	CD3D11Buffer();
	~CD3D11Buffer();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void UpdateData(const void* data, int size) override;

	bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) override;
	bool Unlock() override;

	ID3D11Buffer* GetD3DBufferObject();

private:
	ID3D11Buffer* m_pBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;
};

CD3D11Buffer::CD3D11Buffer() :
	m_pBuffer(nullptr),
	m_bImmutable(false)
{
}

CD3D11Buffer::~CD3D11Buffer()
{
}

HRESULT CD3D11Buffer::Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	m_BufferType = bufferType;
	m_bImmutable = bImmutable;

	D3D11_BUFFER_DESC desc;
	desc.ByteWidth = DataSize;
	desc.Usage = bImmutable ? D3D11_USAGE_DEFAULT : D3D11_USAGE_DYNAMIC;
	desc.BindFlags = GetD3D11BindFlags(bufferType);
	desc.CPUAccessFlags = bImmutable ? 0 : D3D11_CPU_ACCESS_WRITE;
	desc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subData;
	subData.pSysMem = pData;

	HRESULT res = pDevice->CreateBuffer(&desc, pData? &subData : NULL, &m_pBuffer);
	//R_CHK(res);
	return res;
}

void CD3D11Buffer::UpdateData(const void* data, int size)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->UpdateSubresource(m_pBuffer, 0, NULL, data, 0, 0);
}

bool CD3D11Buffer::Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags)
{
//	R_ASSERT(m_bImmutable);

	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	D3D11_MAPPED_SUBRESOURCE mappedSubresource = {};
	HRESULT hr = pImmediateContext->Map(m_pBuffer, 0, GetD3D11Map(Flags), 0, &mappedSubresource);
	if (FAILED(hr))
	{
		Msg("CD3D11Buffer::Lock: Failed to lock buffer. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	*ppbData = (byte*)mappedSubresource.pData + OffsetToLock;

	return true;
}

bool CD3D11Buffer::Unlock()
{
//	R_ASSERT(m_bImmutable);

	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->Unmap(m_pBuffer, 0);

	return true;
}

ID3D11Buffer* CD3D11Buffer::GetD3DBufferObject()
{
	return m_pBuffer;
}

IRHIBuffer* CreateD3D11Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CD3D11Buffer* pBuffer = new CD3D11Buffer();

	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));

	return pBuffer;
}

void SetVertexBuffersD3D11(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	CD3D11Buffer* pAPIBuffer = (CD3D11Buffer*)pVertexBuffer;
	if (pAPIBuffer)
	{
		ID3D11Buffer* pD3DBuffer = pAPIBuffer->GetD3DBufferObject();

		const UINT uStrides = Strides;
		const UINT uOffsets = Offsets;
		pImmediateContext->IASetVertexBuffers(StartSlot, 1, &pD3DBuffer, &uStrides, &uOffsets);
	}
	else
	{
		pImmediateContext->IASetVertexBuffers(StartSlot, 0, nullptr, 0, 0);
	}
}

void SetIndexBufferD3D11(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	CD3D11Buffer* pAPIBuffer = (CD3D11Buffer*)pIndexBuffer;
	if (pAPIBuffer)
	{
		ID3D11Buffer* pD3DBuffer = pAPIBuffer->GetD3DBufferObject();
		DXGI_FORMAT indicesFormat = Is32BitBuffer ? DXGI_FORMAT_R32_UINT : DXGI_FORMAT_R16_UINT;
		pImmediateContext->IASetIndexBuffer(pD3DBuffer, indicesFormat, Offset);
	}
	else
	{
		pImmediateContext->IASetIndexBuffer(nullptr, DXGI_FORMAT_UNKNOWN, 0);
	}
}