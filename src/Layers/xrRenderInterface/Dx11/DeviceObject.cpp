#include "stdafx.h"
#include <d3d11.h>
#include "../../../xrEngine/ICore_GPU.h"
#include <renderdoc/api/app/renderdoc_app.h>
#include "Dx11API.h"

#include "Dx11Buffer.h"
#include "Dx11Texture.h"
#include "Dx11StencilView.h"
#include "Dx11Surface.h"

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

IRHITexture* CreateD3D11Texture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData )
{
	CD3D11Texture2D* pTexture = new CD3D11Texture2D();
	R_CHK(pTexture->Create(pTextureDesc, pSubresourceData));

	pTexture->AddRef();

	return pTexture;
}

IRHITexture* CreateD3D11Texture3D(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData)
{
	CD3D11Texture3D* pTexture = new CD3D11Texture3D();

	R_CHK(pTexture->Create(pTextureDesc, pSubresourceData));

	pTexture->AddRef();

	return pTexture;
}

IRHIBuffer* CreateD3D11Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CD3D11Buffer* pBuffer = new CD3D11Buffer();
	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));

	pBuffer->AddRef();

	return pBuffer;
}

void SetVertexBufferD3D11(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets)
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

void ClearD3D11(ERHIClearStage Stage, IRHIUnknown* Ptr, const ClearData& Data)
{
	ID3D11DeviceContext* Context = ((ID3D11DeviceContext*)g_RenderRHI->GetRenderContext());

	switch (Stage)
	{
		case ERHIClearStage::eClearDepth:
		{
			Context->ClearDepthStencilView(((CD3D11DepthStencilView*)(Ptr))->GetDXObj(), D3D11_CLEAR_DEPTH, Data.Depth, Data.Stencil);
			break;
		}
		case ERHIClearStage::eClearStencil:
		{
			Context->ClearDepthStencilView(((CD3D11DepthStencilView*)(Ptr))->GetDXObj(), D3D11_CLEAR_STENCIL, Data.Depth, Data.Stencil);
			break;
		}
		case ERHIClearStage::eClearTarget:
		{
			Context->ClearRenderTargetView(((CD3D11Surface*)(Ptr))->GetDXObj(), &Data.Color.x);
			break;
		}
	}
}
