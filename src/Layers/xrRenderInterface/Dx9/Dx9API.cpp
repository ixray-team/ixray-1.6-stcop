#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

class CD3D9Texture : public IRHITexture
{
public:
	CD3D9Texture();
	~CD3D9Texture();

	HRESULT Create(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;

	void SetData( const void* pData, const int size );

private:
	IDirect3DTexture9* m_pTexture;
	TextureDesc m_textureDesc;
};

CD3D9Texture::CD3D9Texture() :
	m_pTexture( nullptr )
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

HRESULT CD3D9Texture::Create( const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch )
{
	R_ASSERT( pTextureDesc );

	m_textureDesc = *pTextureDesc;

	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT( pDevice );

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

	SetData( pData, Size );

	return S_OK;
}

bool CD3D9Texture::LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags)
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

IRHITexture* CreateD3D9Texture(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch)
{
	CD3D9Texture* pTexture = new CD3D9Texture();

	R_CHK( pTexture->Create( pTextureDesc, pData, Size, Pitch ) );

	pTexture->AddRef();

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

	IDirect3DVertexBuffer9* GetD3DVertexBuffer();
	IDirect3DIndexBuffer9* GetD3DIndexBuffer();

private:
	template <typename T>
	void SetData( T* pBuffer, const void* pData, u32 DataSize );

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
	if (m_pIndexBuffer)
	{
		m_pIndexBuffer->Release();
		m_pIndexBuffer = nullptr;
	}

	if (m_pVertexBuffer)
	{
		m_pVertexBuffer->Release();
		m_pVertexBuffer = nullptr;
	}
}

HRESULT CD3D9Buffer::Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	m_BufferType = bufferType;
	m_bImmutable = bImmutable;

	D3DPOOL dwPool = D3DPOOL_DEFAULT;
	DWORD dwUsage = 0;

	if (bImmutable)
		dwPool = D3DPOOL_MANAGED;
	else
		dwUsage = D3DUSAGE_DYNAMIC;

	HRESULT hr = S_OK;

	switch (bufferType)
	{
	case eVertexBuffer:
		hr = pDevice->CreateVertexBuffer(DataSize, D3DUSAGE_WRITEONLY | dwUsage, 0, dwPool, &m_pVertexBuffer, NULL);
		SetData(m_pVertexBuffer, pData, DataSize);
		break;
	case eIndexBuffer:
		hr = pDevice->CreateIndexBuffer(DataSize, D3DUSAGE_WRITEONLY | dwUsage, D3DFMT_INDEX16, dwPool, &m_pIndexBuffer, NULL);
		SetData(m_pIndexBuffer, pData, DataSize);
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

IDirect3DVertexBuffer9* CD3D9Buffer::GetD3DVertexBuffer()
{
	return m_pVertexBuffer;
}

IDirect3DIndexBuffer9* CD3D9Buffer::GetD3DIndexBuffer()
{
	return m_pIndexBuffer;
}

template<typename T>
void CD3D9Buffer::SetData( T* pBuffer, const void* pData, u32 DataSize )
{
	if (!pData)
		return;

	byte* bytes = 0;
	R_CHK( pBuffer->Lock( 0, 0, (void**)&bytes, 0 ) );
	memcpy( bytes, pData, DataSize );
	R_CHK( pBuffer->Unlock() );
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
