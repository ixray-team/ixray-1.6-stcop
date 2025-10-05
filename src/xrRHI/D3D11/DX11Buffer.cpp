#include "Dx11Buffer.h"
#include "Device.h"

D3D11_MAP GetD3D11Map(ERHI_BUFFER_MAP Mapping)
{
	switch (Mapping)
	{
	case ERHI_BUFFER_MAP::READ:
		return D3D11_MAP_READ;
	case ERHI_BUFFER_MAP::WRITE:
		return D3D11_MAP_WRITE;
	case ERHI_BUFFER_MAP::WRITE_NO_OVERWRITE:
		return D3D11_MAP_WRITE_NO_OVERWRITE;
	case ERHI_BUFFER_MAP::WRITE_DISCARD:
		return D3D11_MAP_WRITE_DISCARD;
	case ERHI_BUFFER_MAP::READ_AND_WRITE:
		return D3D11_MAP_READ_WRITE;
	}

	return (D3D11_MAP)0;
}

u32 GetD3D11BindFlags(ERHI_BUFFER_TYPE bufferType)
{
	switch (bufferType)
	{
	case ERHI_BUFFER_TYPE::VERTEX:
		return D3D11_BIND_VERTEX_BUFFER;
	case ERHI_BUFFER_TYPE::INDEX:
		return D3D11_BIND_INDEX_BUFFER;
	case ERHI_BUFFER_TYPE::CONSTANT:
		return D3D11_BIND_CONSTANT_BUFFER;
	case ERHI_BUFFER_TYPE::STRUCTURED:
		return D3D11_BIND_SHADER_RESOURCE;
	}

	return 0;
}

D3D11_USAGE GetD3D11Usage(ERHI_USAGE usage)
{
	switch (usage)
	{
	case ERHI_USAGE::USAGE_DEFAULT:
		return D3D11_USAGE_DEFAULT;
	case ERHI_USAGE::USAGE_IMMUTABLE:
		return D3D11_USAGE_IMMUTABLE;
	case ERHI_USAGE::USAGE_DYNAMIC:
		return D3D11_USAGE_DYNAMIC;
	case ERHI_USAGE::USAGE_STAGING:
		return D3D11_USAGE_STAGING;

	}

	R_ASSERT(0);

	return D3D11_USAGE_DEFAULT;
}

u32 GetD3D11CPUAccess(ERHI_CPU_ACCESS_FLAG flag)
{
	switch (flag)
	{
	case ERHI_CPU_ACCESS_FLAG_WRITE:
		return D3D11_CPU_ACCESS_WRITE;
	case ERHI_CPU_ACCESS_FLAG_READ:
		return D3D11_CPU_ACCESS_READ;
	}

	return 0;
}

CD3D11Buffer::CD3D11Buffer() :
	m_pBuffer(nullptr)
{
	memset(&m_bufferDesc, 0, sizeof(m_bufferDesc));
}

CD3D11Buffer::~CD3D11Buffer()
{
	if (m_pBuffer)
	{
		m_pBuffer->Release();
		m_pBuffer = nullptr;
	}
}

void CD3D11Buffer::Create(const RHIBufferDesc& desc /*= {}*/, const RHIBufferSubresource* pSubresource /*= nullptr*/)
{
	ID3D11Device* pDevice = static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice);
	R_ASSERT(pDevice);

	m_bufferDesc = desc;

	D3D11_BUFFER_DESC d3d11desc;
	d3d11desc.ByteWidth = m_bufferDesc.Size;
	d3d11desc.Usage = GetD3D11Usage(m_bufferDesc.Usage);
	d3d11desc.BindFlags = GetD3D11BindFlags(m_bufferDesc.Type);
	d3d11desc.CPUAccessFlags = GetD3D11CPUAccess(static_cast<ERHI_CPU_ACCESS_FLAG>(m_bufferDesc.CPUAccessFlags));
	d3d11desc.MiscFlags = 0;

	if (m_bufferDesc.Type == ERHI_BUFFER_TYPE::STRUCTURED)
	{
		d3d11desc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_STRUCTURED;
		d3d11desc.StructureByteStride = m_bufferDesc.StructureByteStride;
	}
	else
	{
		d3d11desc.MiscFlags = 0;
	}

	D3D11_SUBRESOURCE_DATA subData;
	subData.pSysMem = pSubresource ? pSubresource->pSysMem : nullptr;

	HRESULT res = pDevice->CreateBuffer(&d3d11desc, pSubresource ? &subData : nullptr, &m_pBuffer);
	R_CHK(res);
}

bool CD3D11Buffer::Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData)
{
	ID3D11DeviceContext* pImmediateContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	R_ASSERT(pImmediateContext);

	D3D11_MAPPED_SUBRESOURCE mappedSubresource = {};
	HRESULT hr = pImmediateContext->Map(m_pBuffer, 0, GetD3D11Map(MapType), 0, &mappedSubresource);
	if (FAILED(hr))
	{
		Msg("! CD3D11Buffer::Map: Failed to map buffer. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	pData->pData = mappedSubresource.pData;
	pData->DepthPitch = mappedSubresource.DepthPitch;
	pData->RowPitch = mappedSubresource.RowPitch;

	return true;
}

void CD3D11Buffer::Unmap()
{
	ID3D11DeviceContext* pImmediateContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	R_ASSERT(pImmediateContext);

	pImmediateContext->Unmap(m_pBuffer, 0);
}

void CD3D11Buffer::UpdateSubresource(void* pData, u32 Size)
{
	ID3D11DeviceContext* pImmediateContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	R_ASSERT(pImmediateContext);

	pImmediateContext->UpdateSubresource(m_pBuffer, 0, NULL, pData, 0, 0);
}

ID3D11Buffer* CD3D11Buffer::GetD3DObject()
{
	return m_pBuffer;
}

void CD3D11Buffer::SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset)
{
	ID3D11DeviceContext* pImmediateContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	R_ASSERT(pImmediateContext);

	ID3D11Buffer* pD3DBuffer = m_pBuffer;

	const UINT uStrides = Stride;
	const UINT uOffsets = Offset;
	pImmediateContext->IASetVertexBuffers(StartSlot, 1, &pD3DBuffer, &uStrides, &uOffsets);
}

void CD3D11Buffer::SetIndexBuffer(bool Is32BitBuffer, u32 Offset)
{
	ID3D11DeviceContext* pImmediateContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	R_ASSERT(pImmediateContext);

	ID3D11Buffer* pD3DBuffer = m_pBuffer;
	DXGI_FORMAT indicesFormat = Is32BitBuffer ? DXGI_FORMAT_R32_UINT : DXGI_FORMAT_R16_UINT;
	pImmediateContext->IASetIndexBuffer(pD3DBuffer, indicesFormat, Offset);
}

void CD3D11Buffer::AddRef()
{
	m_pBuffer->AddRef();
}

u32 CD3D11Buffer::Release()
{
	return static_cast<u32>(m_pBuffer->Release());
}
