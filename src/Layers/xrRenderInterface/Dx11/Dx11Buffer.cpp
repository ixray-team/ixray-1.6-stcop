#include "stdafx.h"

#include "Dx11Buffer.h"
#include "Dx11Device.h"

D3D11_MAP GetD3D11Map(eBufferMapping Mapping)
{
	switch (Mapping)
	{
	case MAPPING_READ:
		return D3D11_MAP_READ;
	case MAPPING_WRITE:
		return D3D11_MAP_WRITE;
	case MAPPING_WRITE_NO_OVERWRITE:
		return D3D11_MAP_WRITE_NO_OVERWRITE;
	case MAPPING_WRITE_DISCARD:
		return D3D11_MAP_WRITE_DISCARD;
	case MAPPING_READ_AND_WRITE:
		return D3D11_MAP_READ_WRITE;
	}

	return (D3D11_MAP)0;
}

u32 GetD3D11BindFlags(eBufferType bufferType)
{
	switch (bufferType)
	{
	case EBUFFER_VERTEX:
		return D3D11_BIND_VERTEX_BUFFER;
	case EBUFFER_INDEX:
		return D3D11_BIND_INDEX_BUFFER;
	case EBUFFER_CONSTANT:
		return D3D11_BIND_CONSTANT_BUFFER;
	}

	return 0;
}

CD3D11Buffer::CD3D11Buffer() :
	m_pBuffer(nullptr),
	m_bImmutable(false)
{
	AddRef();
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
	ID3D11Device* pDevice = g_RenderRHI_DX11Implementation.GetDevice();
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

bool CD3D11Buffer::Map(eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pData)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)g_RenderRHI_DX11Implementation.GetDeviceContext();
	R_ASSERT(pImmediateContext);

	D3D11_MAPPED_SUBRESOURCE mappedSubresource = {};
	HRESULT hr = pImmediateContext->Map(m_pBuffer, 0, GetD3D11Map(MapType), 0, &mappedSubresource);
	if (FAILED(hr))
	{
		Msg("! CD3D11Buffer::Map: Failed to map buffer. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	return true;
}

void CD3D11Buffer::Unmap()
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)g_RenderRHI_DX11Implementation.GetDeviceContext();
	R_ASSERT(pImmediateContext);

	pImmediateContext->Unmap(m_pBuffer, 0);
}

void CD3D11Buffer::UpdateSubresource(void* pData, size_t Size)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)g_RenderRHI_DX11Implementation.GetDeviceContext();
	R_ASSERT(pImmediateContext);

	pImmediateContext->UpdateSubresource(m_pBuffer, 0, NULL, pData, 0, 0);
}

ID3D11Buffer* CD3D11Buffer::GetD3DObject()
{
	return m_pBuffer;
}
