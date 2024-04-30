#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

class CRenderTextureDX9 : public IRHITexture
{
public:
	CRenderTextureDX9();
	~CRenderTextureDX9();

	HRESULT Create(const TextureDesc* pTextureDesc, const void* pData, const int size);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;

	void SetData( const void* pData, const int size );

private:
	IDirect3DTexture9* m_pTexture;
	TextureDesc m_textureDesc;
};

CRenderTextureDX9::CRenderTextureDX9() :
	m_pTexture( nullptr )
{
	memset(&m_textureDesc, 0, sizeof(m_textureDesc));
}

CRenderTextureDX9::~CRenderTextureDX9()
{
	if (m_pTexture)
	{
		m_pTexture->Release();
		m_pTexture = nullptr;
	}
}

HRESULT CRenderTextureDX9::Create( const TextureDesc* pTextureDesc, const void* pData, const int size )
{
	R_ASSERT( pTextureDesc );

	m_textureDesc = *pTextureDesc;

	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT( pDevice );

	HRESULT hr = pDevice->CreateTexture( 
		pTextureDesc->width, 
		pTextureDesc->height,
		pTextureDesc->numMips,
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

	SetData( pData, size );

	return S_OK;
}

bool CRenderTextureDX9::LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags)
{
	R_ASSERT( m_pTexture );

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

bool CRenderTextureDX9::UnlockRect(u32 Level)
{
	HRESULT hr = m_pTexture->UnlockRect(Level);
	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::UnlockRect: Failed to unlock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	return true;
}

void CRenderTextureDX9::SetData(const void* pData, const int size)
{
	if (!pData)
		return;

	if (!size)
		return;

	// #TODO: Level fill

	// #TODO: Properly format pitch calc
	int pitchSize = 0;
	if (m_textureDesc.format == FMT_R8G8B8)
		pitchSize = 3;
	else if (m_textureDesc.format == FMT_R8G8B8A8)
		pitchSize = 4;

	// lock rect
	LOCKED_RECT lockRect;
	if (this->LockRect(0, &lockRect, NULL, eLOCK_DISCARD))
	{
		// copy image data
		uint8_t* textureData = (uint8_t*)lockRect.pBits;
		memcpy(textureData, pData, m_textureDesc.width * m_textureDesc.height * pitchSize);

		this->UnlockRect(0);
	}
}

IRHITexture* CreateD3D9Texture(const TextureDesc* pTextureDesc, const void* pData, const int size)
{
	CRenderTextureDX9* pTexture = new CRenderTextureDX9();
	
	R_CHK( pTexture->Create( pTextureDesc, pData, size ) );

	return pTexture;
}

DWORD GetD3D9BufferLockType(eLockType lockType)
{
	switch (lockType)
	{
	case eLOCK_DISCARD:
		return D3DLOCK_DISCARD;
	case eLOCK_NO_DIRTY_UPDATE:
		return D3DLOCK_NO_DIRTY_UPDATE;
	case eLOCK_NOOVERWRITE:
		return D3DLOCK_NOOVERWRITE;
	case eLOCK_NOSYSLOCK:
		return D3DLOCK_NOSYSLOCK;
	case eLOCK_READONLY:
		return D3DLOCK_READONLY;
	default:
		break;
	}

	return 0;
}

class CD3D9Buffer : public IRHIBuffer
{
public:
	CD3D9Buffer();
	~CD3D9Buffer();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void UpdateData(const void* data, int size) override;

	bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) override;
	bool Unlock() override;

private:
	IDirect3DVertexBuffer9* m_pVertexBuffer;
	IDirect3DIndexBuffer9* m_pIndexBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;
};

CD3D9Buffer::CD3D9Buffer() :
	m_pVertexBuffer(nullptr),
	m_pIndexBuffer(nullptr),
	m_bImmutable(false)
{
}

CD3D9Buffer::~CD3D9Buffer()
{
}

HRESULT CD3D9Buffer::Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	m_BufferType = bufferType;
	m_bImmutable = bImmutable;

	HRESULT hr = S_OK;

	switch (bufferType)
	{
	case eVertexBuffer:
		hr = pDevice->CreateVertexBuffer(DataSize, D3DUSAGE_WRITEONLY, 0, D3DPOOL_MANAGED, &m_pVertexBuffer, NULL);
		break;
	case eIndexBuffer:
		hr = pDevice->CreateIndexBuffer(DataSize, D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_MANAGED, &m_pIndexBuffer, NULL);
		break;
	case eConstantBuffer:
	default:
		break;
	}

	R_CHK(hr);

	return hr;
}

void CD3D9Buffer::UpdateData(const void* data, int size)
{
}

bool CD3D9Buffer::Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags)
{
	HRESULT hr = S_OK;

	switch (m_BufferType)
	{
	case eVertexBuffer:
		hr = m_pVertexBuffer->Lock(OffsetToLock, SizeToLock, ppbData, GetD3D9BufferLockType(Flags));
		break;
	case eIndexBuffer:
		hr = m_pIndexBuffer->Lock(OffsetToLock, SizeToLock, ppbData, GetD3D9BufferLockType(Flags));
		break;
	default:
		break;
	}

	R_CHK(hr);

	return true;
}

bool CD3D9Buffer::Unlock()
{
	HRESULT hr = S_OK;

	switch (m_BufferType)
	{
	case eVertexBuffer:
		hr = m_pVertexBuffer->Unlock();
		break;
	case eIndexBuffer:
		hr = m_pIndexBuffer->Unlock();
		break;
	default:
		break;
	}

	R_CHK(hr);

	return true;
}

IRHIBuffer* CreateD3D9Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CD3D9Buffer* pBuffer = new CD3D9Buffer();

	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));

	return pBuffer;
}

