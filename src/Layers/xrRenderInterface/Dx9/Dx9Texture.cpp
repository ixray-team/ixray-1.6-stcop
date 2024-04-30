#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

#include "Dx9Texture.h"

CD3D9Texture::CD3D9Texture() :
	m_pTexture(nullptr)
{
	memset(&m_textureDesc, 0, sizeof(m_textureDesc));
}

CD3D9Texture::~CD3D9Texture()
{
	if (m_pTexture)
	{
		m_pTexture->Release();
		m_pTexture = nullptr;
	}
}

HRESULT CD3D9Texture::Create(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch)
{
	R_ASSERT(pTextureDesc);

	m_textureDesc = *pTextureDesc;

	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	HRESULT hr = pDevice->CreateTexture(
		pTextureDesc->Width,
		pTextureDesc->Height,
		pTextureDesc->NumMips,
		0,
		D3DFMT_A8R8G8B8,
		D3DPOOL_MANAGED,
		&m_pTexture,
		NULL);

	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::Create: Failed to create texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return hr;
	}

	SetData(pData, Size);

	return S_OK;
}

bool CD3D9Texture::LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags)
{
	R_ASSERT(m_pTexture);

	D3DLOCKED_RECT lockedRect;
	memset(&lockedRect, 0, sizeof(lockedRect));

	RECT rect;
	memset(&rect, 0, sizeof(rect));
	if (pRect)
		rect = { pRect->x1, pRect->y1, pRect->x2, pRect->y2 };

	HRESULT hr = m_pTexture->LockRect(Level, &lockedRect, pRect ? &rect : NULL, Flags);
	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::LockRect: Failed to lock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	return true;
}

bool CD3D9Texture::UnlockRect(u32 Level)
{
	HRESULT hr = m_pTexture->UnlockRect(Level);
	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::UnlockRect: Failed to unlock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	return true;
}

void CD3D9Texture::SetData(const void* pData, const int size)
{
	if (!pData)
		return;

	if (!size)
		return;

	// #TODO: Level fill

	// #TODO: Properly format pitch calc
	int pitchSize = 0;
	if (m_textureDesc.Format == FMT_R8G8B8)
		pitchSize = 3;
	else if (m_textureDesc.Format == FMT_R8G8B8A8)
		pitchSize = 4;

	// lock rect
	LOCKED_RECT lockRect;
	if (this->LockRect(0, &lockRect, NULL, eLOCK_DISCARD))
	{
		// copy image data
		uint8_t* textureData = (uint8_t*)lockRect.pBits;
		memcpy(textureData, pData, m_textureDesc.Width * m_textureDesc.Height * pitchSize);

		this->UnlockRect(0);
	}
}
