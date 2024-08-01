#include "stdafx.h"
#include <d3d11.h>
#include "DeviceRHI.h"
#include "Dx11/Dx11Buffer.h"

CRenderRHI_DX11 g_RenderRHI_DX11Implementation;

CRenderRHI_DX11::CRenderRHI_DX11()
{
}

CRenderRHI_DX11::~CRenderRHI_DX11()
{
}

void CRenderRHI_DX11::Create(APILevel API, void* renderDevice, void* renderContext)
{
	HWRenderDevice = renderDevice;
	HWRenderContext = renderContext;
}

IBuffer* CRenderRHI_DX11::CreateAPIBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CD3D11Buffer* pBuffer = new CD3D11Buffer();
	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));
	return pBuffer;
}

void CRenderRHI_DX11::SetVertexBuffer(u32 StartSlot, IBuffer* pVertexBuffer, const u32 Stride, const u32 Offset)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	CD3D11Buffer* pAPIBuffer = (CD3D11Buffer*)pVertexBuffer;
	if (pAPIBuffer)
	{
		ID3D11Buffer* pD3DBuffer = pAPIBuffer->GetD3DBufferObject();

		const UINT uStrides = Stride;
		const UINT uOffsets = Offset;
		pImmediateContext->IASetVertexBuffers(StartSlot, 1, &pD3DBuffer, &uStrides, &uOffsets);
	}
	else
	{
		pImmediateContext->IASetVertexBuffers(StartSlot, 0, nullptr, 0, 0);
	}
}

void CRenderRHI_DX11::SetIndexBuffer(IBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset)
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

ID3D11Device* CRenderRHI_DX11::GetDevice()
{
	return (ID3D11Device*)HWRenderDevice;
}

ID3D11DeviceContext* CRenderRHI_DX11::GetDeviceContext()
{
	return (ID3D11DeviceContext*)HWRenderContext;
}
