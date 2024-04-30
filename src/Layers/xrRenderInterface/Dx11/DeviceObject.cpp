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
	case eLOCK_NO_DIRTY_UPDATE:
		break;
	case eLOCK_NOSYSLOCK:
		break;
	case eLOCK_READONLY:
		return D3D11_MAP_READ;
	default:
		break;
	}

	R_ASSERT(0);
	return (D3D11_MAP)0;
}

class CRenderBufferBaseDX11 : public IRender_BufferBase
{
public:
	CRenderBufferBaseDX11();
	~CRenderBufferBaseDX11();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void UpdateData(const void* data, int size) override;

	bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) override;
	bool Unlock() override;

private:
	ID3D11Buffer* m_pBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;
};

CRenderBufferBaseDX11::CRenderBufferBaseDX11() :
	m_pBuffer(nullptr),
	m_bImmutable(false)
{
}

CRenderBufferBaseDX11::~CRenderBufferBaseDX11()
{
}

HRESULT CRenderBufferBaseDX11::Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
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

void CRenderBufferBaseDX11::UpdateData(const void* data, int size)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->UpdateSubresource(m_pBuffer, 0, NULL, data, 0, 0);
}

bool CRenderBufferBaseDX11::Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags)
{
	R_ASSERT(m_bImmutable);

	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	D3D11_MAPPED_SUBRESOURCE mappedSubresource = {};
	HRESULT hr = pImmediateContext->Map(m_pBuffer, 0, GetD3D11Map(Flags), 0, &mappedSubresource);
	if (FAILED(hr))
	{
		Msg("CRenderBufferBaseDX11::Lock: Failed to lock buffer. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	return true;
}

bool CRenderBufferBaseDX11::Unlock()
{
	R_ASSERT(m_bImmutable);

	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->Unmap(m_pBuffer, 0);

	return true;
}

IRender_BufferBase* CreateD3D11Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CRenderBufferBaseDX11* pBuffer = new CRenderBufferBaseDX11();

	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));

	return pBuffer;
}
