#include "stdafx.h"
#include "Dx11API.h"
#include <d3d11.h>

#include "Dx11Buffer.h"

D3D11_MAP GetD3D11Map(eLockType lockType);

static u32 GetD3D11BindFlags(eBufferType bufferType)
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

CD3D11Buffer::CD3D11Buffer() :
	m_pBuffer(nullptr),
	m_bImmutable(false)
{
}

CD3D11Buffer::~CD3D11Buffer()
{
	if (m_pBuffer)
	{
		m_pBuffer->Release();
		m_pBuffer = nullptr;
	}
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

	HRESULT res = pDevice->CreateBuffer(&desc, pData ? &subData : NULL, &m_pBuffer);
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
		Msg("! CD3D11Buffer::Lock: Failed to lock buffer. DirectX Error: %s", Debug.error2string(hr));
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

EResourceType CD3D11Buffer::GetType()
{
	switch (m_BufferType)
	{
	case eVertexBuffer:
		return eResourceVertexBuffer;
	case eIndexBuffer:
		return eResourceIndexBuffer;
	case eConstantBuffer:
		return eResourceConstantBuffer;
	}

	return eResourceUnknown;
}
