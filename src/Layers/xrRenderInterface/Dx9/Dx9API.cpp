#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

#include "Dx9Texture.h"
#include "Dx9Buffer.h"

IRHITexture* CreateD3D9Texture(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch)
{
	CD3D9Texture* pTexture = new CD3D9Texture();

	R_CHK( pTexture->Create( pTextureDesc, pData, Size, Pitch ) );

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
