#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

#include "Dx9Texture.h"
#include "Dx9Buffer.h"

D3DFORMAT ConvertTextureFormat(ERHITextureFormat dx9FMT);
IRHIDepthStencilView* CreateD3D9DepthStencilSurface(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	CD3D9Surface* pDepthTexture = new CD3D9Surface();

	pDevice->CreateDepthStencilSurface(Width, Height, ConvertTextureFormat(Format), (D3DMULTISAMPLE_TYPE)MultiSample, MultisampleQuality, Discard, &pDepthTexture->m_pSurface, nullptr);
	pDepthTexture->AddRef();

	return pDepthTexture;
}

IRHITexture* CreateD3D9Texture(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData )
{
	CD3D9Texture* pTexture = new CD3D9Texture();

	R_CHK( pTexture->Create( pTextureDesc, pSubresourceData ) );

	pTexture->AddRef();

	return pTexture;
}

IRHIBuffer* CreateD3D9Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CD3D9Buffer* pBuffer = new CD3D9Buffer();

	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));

	pBuffer->AddRef();

	return pBuffer;
}

void SetVertexBufferD3D9(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	CD3D9Buffer* pAPIBuffer = (CD3D9Buffer*)pVertexBuffer;
	if (pAPIBuffer)
	{
		R_CHK(pDevice->SetStreamSource(StartSlot, pAPIBuffer->GetD3DVertexBuffer(), 0, Strides));
	}
	else
	{
		R_CHK(pDevice->SetStreamSource(StartSlot, nullptr, 0, 0));
	}
}

void SetIndexBufferD3D9(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	CD3D9Buffer* pAPIBuffer = (CD3D9Buffer*)pIndexBuffer;
	if (pAPIBuffer)
	{
		R_CHK(pDevice->SetIndices(pAPIBuffer->GetD3DIndexBuffer()));
	}
	else
	{
		R_CHK(pDevice->SetIndices(nullptr));
	}
}

void SetRenderTargetD3D9(u32 RenderTargetIndex, IRHISurface* pRenderTarget)
{
	IDirect3DDevice9* pDevice = ( IDirect3DDevice9* )HWRenderDevice;
	R_ASSERT( pDevice );

	if ( pRenderTarget )
	{
		CD3D9Surface* pAPISurface = ( CD3D9Surface* )pRenderTarget;
		pDevice->SetRenderTarget( RenderTargetIndex, pAPISurface->GetD3D9SurfaceObject() );
	}
	else
	{
		pDevice->SetRenderTarget( RenderTargetIndex, NULL );
	}
}

void SetDepthStencilViewD3D9(IRHIDepthStencilView* pDepthStencilView)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	if (pDepthStencilView)
		pDepthStencilView->SetActive();
	else
		pDevice->SetDepthStencilSurface(0);
}

IRHISurface* CreateOffscreenPlainSurfaceD3D9(u32 Width, u32 Height, ERHITextureFormat Format, bool DefaultPool)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	IDirect3DSurface9* pSurface = nullptr;
	HRESULT hr = pDevice->CreateOffscreenPlainSurface(Width, Height, ConvertTextureFormat(Format), DefaultPool ? D3DPOOL_DEFAULT : D3DPOOL_SYSTEMMEM, &pSurface, NULL);
	R_CHK(hr);

	CD3D9Surface* pAPISurface = new CD3D9Surface(pSurface);
	pAPISurface->AddRef();
	return pAPISurface;
}

void GetRenderTargetDataD3D9(IRHISurface* pRenderTarget, IRHISurface* pDestSurface)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	CD3D9Surface* pAPIRenderTarget = (CD3D9Surface*)pRenderTarget;
	CD3D9Surface* pAPIDestSurface = (CD3D9Surface*)pDestSurface;
	HRESULT hr = pDevice->GetRenderTargetData(pAPIRenderTarget->GetD3D9SurfaceObject(), pAPIDestSurface->GetD3D9SurfaceObject());
	R_CHK(hr);
}
